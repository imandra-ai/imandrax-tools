import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import { describe, expect, it } from "vitest";

import { drawTasks } from "../src/task/view";

// Fixtures are the exact `TasksWidget` input (the `task_entries` traitlet list
// the Python side feeds the frontend), generated from real API output by
// scripts/gen_widget_input_fixtures.
function loadFixture(name) {
  // vitest runs with the package dir as cwd.
  const path = resolve(
    process.cwd(),
    `test/fixtures/inputs/${name}.widget_input.json`,
  );
  return JSON.parse(readFileSync(path, "utf8"));
}

const admitRec = loadFixture("tasks.admit_rec.iml"); // 1 task (info), 2 artifacts
const longProof = loadFixture("tasks.long_proof.iml"); // 7 tasks, 1 error
const mixed = loadFixture("tasks.mixed.iml"); // 8 tasks across every level but debug

// A synthetic entry, for shapes the fixtures don't cover.
const task = (over) => ({
  id: "task:po:abcdefghij",
  kind: "TASK_CHECK_PO",
  level: "info",
  from_sym: "f",
  artifacts: [{ kind: "po_res", repr: "PORes()" }],
  ...over,
});

const render = (data) => {
  const el = document.createElement("div");
  drawTasks(el, data);
  return el;
};
// A checkbox only fires `change` on click when connected to the document.
const renderAttached = (data) => document.body.appendChild(render(data));
// A click whose pointer moved `dx` px between press and release; `detail` is the
// click count (2 for a double click's second click).
const press = (row, dx, detail = 1) => {
  row.dispatchEvent(new MouseEvent("mousedown", { clientX: 10, clientY: 10, detail, bubbles: true }));
  row.dispatchEvent(
    new MouseEvent("click", { clientX: 10 + dx, clientY: 10, detail, bubbles: true }),
  );
};
const rows = (el) => [...el.querySelectorAll(".imdx-task-row")];
const cell = (row, name) => row.querySelector(`.imdx-task-${name}`);
describe("task", () => {
  it("renders a row per task", () => {
    expect(rows(render(longProof)).length).toBe(7);
  });

  it("shows each task's symbol, kind, and short id", () => {
    const [row] = rows(render(admitRec));
    expect(cell(row, "sym").textContent).toBe("f");
    expect(cell(row, "kind").textContent).toBe("CHECK_PO");
    const id = cell(row, "id");
    expect(id.title).toBe(admitRec[0].id);
    expect(id.textContent).toBe(admitRec[0].id.slice(0, "task:po:".length + 6));
  });

  it("sorts rows by level, most severe first", () => {
    const levels = rows(render(mixed)).map((r) => r.dataset.level);
    expect(levels).toEqual([...levels].sort(
      (a, b) => ["debug", "info", "warning", "error"].indexOf(b) -
        ["debug", "info", "warning", "error"].indexOf(a),
    ));
    expect(levels[0]).toBe("error");
  });

  it("prints the symbol on every row", () => {
    const el = render([
      task({ id: "task:po:1", from_sym: "f" }),
      task({ id: "task:po:2", from_sym: "f" }),
      task({ id: "task:po:3", from_sym: "g" }),
    ]);
    expect(rows(el).map((r) => cell(r, "sym").textContent)).toEqual(["f", "f", "g"]);
  });

  it("keeps a symbol's tasks together within a level", () => {
    const el = render([
      task({ id: "task:po:1", from_sym: "f" }),
      task({ id: "task:po:2", from_sym: "g" }),
      task({ id: "task:po:3", from_sym: "f" }),
    ]);
    expect(rows(el).map((r) => cell(r, "id").title)).toEqual([
      "task:po:1",
      "task:po:3",
      "task:po:2",
    ]);
  });

  it("toggles all artifacts of a task by clicking its row", () => {
    const el = render([
      task({ id: "task:po:1", from_sym: "f" }),
      task({ id: "task:po:2", from_sym: "f" }),
    ]);
    const openIds = () =>
      [...el.querySelectorAll(".imdx-task-detail")].map(
        (d) => cell(d.previousElementSibling, "id").title,
      );
    // Any cell of the row will do; only that row opens.
    cell(rows(el)[0], "task-descr").click();
    expect(openIds()).toEqual(["task:po:1"]);
    expect(rows(el)[0].getAttribute("aria-expanded")).toBe("true");
    cell(rows(el)[0], "sym").click();
    expect(openIds()).toEqual([]);
  });

  it("toggles on a slightly sloppy click, but not after a drag", () => {
    const el = render(admitRec);
    const arts = () => el.querySelectorAll(".imdx-task-art").length;
    press(rows(el)[0], 2);
    expect(arts()).toBe(2);
    press(rows(el)[0], 20); // a drag selecting text
    expect(arts()).toBe(2);
  });

  it("leaves a row as it was after a double or triple click", () => {
    const el = render(admitRec);
    const arts = () => el.querySelectorAll(".imdx-task-art").length;
    const clicks = (n) => {
      for (let d = 1; d <= n; d++) press(rows(el)[0], 0, d);
    };
    clicks(2); // double click: selects a word
    expect(arts()).toBe(0);
    clicks(3); // triple click: selects a line
    expect(arts()).toBe(0);
  });

  it("keeps other rows' DOM when toggling one", () => {
    const el = render(mixed);
    const before = [...el.querySelectorAll(".imdx-task-art")];
    const quiet = rows(el).find((r) => r.dataset.level === "info");
    quiet.click();
    const after = [...el.querySelectorAll(".imdx-task-art")];
    expect(after.length).toBe(before.length + quiet.querySelectorAll(".imdx-task-chip").length);
    // The artifacts already open are the same nodes, so their scroll and selection survive.
    expect(before.every((b) => after.includes(b))).toBe(true);
  });

  it("opens the rest when a row is partly open", () => {
    const el = render(admitRec);
    el.querySelectorAll(".imdx-task-chip")[0].click();
    rows(el)[0].click();
    expect(el.querySelectorAll(".imdx-task-art").length).toBe(2);
  });

  it("toggles a row from the keyboard, keeping focus on it", () => {
    const el = renderAttached(admitRec);
    rows(el)[0].focus();
    rows(el)[0].dispatchEvent(new KeyboardEvent("keydown", { key: "Enter", bubbles: true }));
    expect(el.querySelectorAll(".imdx-task-art").length).toBe(2);
    expect(document.activeElement).toBe(rows(el)[0]);
  });

  it("doesn't make a row without artifacts clickable", () => {
    const [row] = rows(render([task({ artifacts: [] })]));
    expect(row.classList.contains("imdx-task-row-toggle")).toBe(false);
    expect(row.tabIndex).toBe(-1);
  });

  it("shows an em dash for a task without a symbol", () => {
    const [row] = rows(render([task({ from_sym: null })]));
    expect(cell(row, "sym").textContent).toBe("—");
  });

  it("opens artifacts of warning/error tasks and collapses the rest", () => {
    const el = render(mixed);
    for (const row of rows(el)) {
      const loud = ["warning", "error"].includes(row.dataset.level);
      const next = row.nextElementSibling;
      const hasDetail = next?.classList.contains("imdx-task-detail") ?? false;
      expect(hasDetail).toBe(loud);
    }
  });

  it("toggles an artifact from its chip", () => {
    const el = render(admitRec);
    expect(el.querySelector(".imdx-task-art")).toBeNull();
    const chip = () => el.querySelectorAll(".imdx-task-chip")[1]; // po_res
    chip().click();
    expect(chip().getAttribute("aria-pressed")).toBe("true");
    // The chip's click doesn't also toggle its row.
    const arts = el.querySelectorAll(".imdx-task-art");
    expect(arts.length).toBe(1);
    expect(arts[0].querySelector(".imdx-task-art-kind").textContent).toBe("po_res");
    chip().click();
    expect(el.querySelector(".imdx-task-art")).toBeNull();
  });

  it("closes an artifact from its × button", () => {
    const el = render(admitRec);
    const chips = () => el.querySelectorAll(".imdx-task-chip");
    chips()[0].click();
    chips()[1].click();
    el.querySelector(".imdx-task-close").click(); // po_task's
    const kinds = [...el.querySelectorAll(".imdx-task-art-kind")].map((k) => k.textContent);
    expect(kinds).toEqual(["po_res"]);
    expect(chips()[0].getAttribute("aria-pressed")).toBe("false");
  });

  it("closes an artifact from its header, but not from copy", () => {
    const el = render(admitRec);
    el.querySelectorAll(".imdx-task-chip")[1].click(); // po_res
    el.querySelector(".imdx-task-copy").click();
    expect(el.querySelectorAll(".imdx-task-art").length).toBe(1);
    el.querySelector(".imdx-task-art-kind").click();
    expect(el.querySelector(".imdx-task-art")).toBeNull();
  });

  it("keeps open artifacts in the task's own order", () => {
    const el = render(admitRec);
    const chips = () => el.querySelectorAll(".imdx-task-chip");
    chips()[1].click();
    chips()[0].click();
    const kinds = [...el.querySelectorAll(".imdx-task-art-kind")].map((k) => k.textContent);
    expect(kinds).toEqual(admitRec[0].artifacts.map((a) => a.kind));
  });

  it("renders an artifact's text verbatim, syntax-highlighted", () => {
    // longProof's first task is the failing one, so its po_task is open; it
    // carries the full kwarg form (from_sym=..., count=0).
    const pre = render(longProof).querySelector(".imdx-task-pre");
    expect(pre.querySelector(".t-cls")).not.toBeNull(); // POTask(...)
    expect(pre.querySelector(".t-attr")).not.toBeNull(); // from_sym=
    expect(pre.querySelector(".t-str")).not.toBeNull(); // 'len_append'
    const failing = longProof.find((t) => t.level === "error");
    expect(pre.textContent).toBe(failing.artifacts[0].repr);
  });

  it("hides debug tasks", () => {
    const el = render([
      task({ id: "task:po:1", level: "debug" }),
      task({ id: "task:po:2", level: "info" }),
    ]);
    expect(rows(el).map((r) => cell(r, "id").title)).toEqual(["task:po:2"]);
  });

  it("treats a task without a level as info", () => {
    const [row] = rows(render([task({ level: undefined })]));
    expect(row.dataset.level).toBe("info");
  });

  it("reports when every task is debug", () => {
    const el = render([task({ level: "debug" })]);
    expect(el.querySelector(".imdx-task-placeholder").hidden).toBe(false);
  });

  it("orders columns: task, result, symbol, artifacts, kind", () => {
    const el = render(admitRec);
    const heads = [...el.querySelectorAll("thead th")].map((th) => th.textContent);
    expect(heads.slice(0, 5)).toEqual(["task", "result", "symbol", "artifacts", "kind"]);
    const [row] = rows(el);
    expect([...row.children].slice(0, 5).map((c) => c.className)).toEqual([
      "imdx-task-task-descr",
      "imdx-task-res-descr",
      "imdx-task-sym",
      "imdx-task-chips",
      "imdx-task-kind",
    ]);
  });

  it("shows the task and result descriptions, with the level icon after the result", () => {
    const [row] = rows(render(admitRec));
    expect(cell(row, "task-descr").textContent).toBe(admitRec[0].task_descr);
    const res = cell(row, "res-descr");
    const parts = [...res.querySelector(".imdx-task-descr").children];
    expect(parts.map((p) => p.className)).toEqual(["imdx-task-descr-text", "imdx-task-level"]);
    expect(parts[0].textContent).toBe(admitRec[0].res_descr);
    expect(parts[1].textContent).toBe("✅");
    expect(parts[1].title).toBe("info");
  });

  it("shows a dash for missing descriptions, keeping the level icon", () => {
    for (const descr of [undefined, null, ""]) {
      const [row] = rows(render([task({ task_descr: descr, res_descr: descr, level: "error" })]));
      for (const name of ["task-descr", "res-descr"]) {
        const text = cell(row, name).querySelector(".imdx-task-descr-text");
        expect(text.textContent).toBe("—");
        expect(text.classList.contains("imdx-task-descr-none")).toBe(true);
      }
      expect(cell(row, "level").textContent).toBe("❌");
    }
  });

  it("shows debug tasks when 'show debug' is ticked", () => {
    const el = renderAttached([
      task({ id: "task:po:1", level: "debug" }),
      task({ id: "task:po:2", level: "info" }),
    ]);
    const box = el.querySelector(".imdx-task-show-debug input");
    expect(box.checked).toBe(false);
    box.click();
    expect(rows(el).map((r) => cell(r, "id").title)).toEqual(["task:po:2", "task:po:1"]);
    box.click();
    expect(rows(el).map((r) => cell(r, "id").title)).toEqual(["task:po:2"]);
  });

  it("dims 'show debug' when there is no debug task", () => {
    const label = render([task({ level: "info" })]).querySelector(".imdx-task-show-debug");
    expect(label.querySelector("input").disabled).toBe(false);
    expect(label.classList.contains("imdx-task-show-debug-none")).toBe(true);

    const withDebug = render([task({ level: "debug" })]).querySelector(".imdx-task-show-debug");
    expect(withDebug.classList.contains("imdx-task-show-debug-none")).toBe(false);
  });

  it("keeps the header, and its toggle, when every task is debug", () => {
    const el = renderAttached([task({ level: "debug" })]);
    expect(el.querySelector(".imdx-task-table").hidden).toBe(false);
    el.querySelector(".imdx-task-show-debug input").click();
    expect(rows(el).length).toBe(1);
    expect(el.querySelector(".imdx-task-placeholder").hidden).toBe(true);
  });

  it("tolerates an empty task list", () => {
    const el = render([]);
    expect(rows(el).length).toBe(0);
    expect(el.querySelector(".imdx-task-placeholder").textContent).toBe(
      "No tasks.",
    );
  });
});

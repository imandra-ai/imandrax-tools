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

  it("prints a symbol once for consecutive rows sharing it", () => {
    const el = render([
      task({ id: "task:po:1", from_sym: "f" }),
      task({ id: "task:po:2", from_sym: "f" }),
      task({ id: "task:po:3", from_sym: "g" }),
    ]);
    expect(rows(el).map((r) => cell(r, "sym").textContent)).toEqual(["f", "", "g"]);
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

  it("toggles all artifacts of a symbol's rows from its name", () => {
    const el = render([
      task({ id: "task:po:1", from_sym: "f" }),
      task({ id: "task:po:2", from_sym: "f" }),
      task({ id: "task:po:3", from_sym: "g" }),
    ]);
    const sym = (name) =>
      [...el.querySelectorAll(".imdx-task-sym-btn")].find((b) => b.textContent === name);
    const openIds = () =>
      [...el.querySelectorAll(".imdx-task-detail")].map(
        (d) => cell(d.previousElementSibling, "id").title,
      );
    sym("f").click();
    expect(openIds()).toEqual(["task:po:1", "task:po:2"]);
    // Partly open counts as closed: the next click opens the rest.
    el.querySelectorAll(".imdx-task-chip")[0].click();
    sym("f").click();
    expect(openIds()).toEqual(["task:po:1", "task:po:2"]);
    sym("f").click();
    expect(openIds()).toEqual([]);
  });

  it("toggles a symbol's run from any of its cells, highlighting them together", () => {
    const el = render([
      task({ id: "task:po:1", from_sym: "f" }),
      task({ id: "task:po:2", from_sym: "f" }),
      task({ id: "task:po:3", from_sym: "g" }),
    ]);
    const symCells = () => rows(el).map((r) => cell(r, "sym"));
    // The blank cell under `f` stands for `f` too.
    symCells()[1].dispatchEvent(new MouseEvent("mouseenter"));
    expect(symCells().map((c) => c.classList.contains("imdx-task-sym-hover"))).toEqual([
      true,
      true,
      false,
    ]);
    symCells()[1].click();
    expect(el.querySelectorAll(".imdx-task-detail").length).toBe(2);
    expect(rows(el).map((r) => !!r.nextElementSibling?.classList.contains("imdx-task-detail")))
      .toEqual([true, true, false]);
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

  it("treats a task without a level as debug", () => {
    expect(rows(render([task({ level: undefined })])).length).toBe(0);
  });

  it("reports when every task is debug", () => {
    const el = render([task({ level: "debug" })]);
    expect(el.querySelector(".imdx-task-placeholder").hidden).toBe(false);
  });

  it("puts the artifacts column before kind", () => {
    const el = render(admitRec);
    const heads = [...el.querySelectorAll("thead th")].map((th) => th.textContent);
    expect(heads.slice(0, 4)).toEqual(["", "symbol", "artifacts", "kind"]);
    const [row] = rows(el);
    expect(row.children[2].className).toBe("imdx-task-chips");
    expect(row.children[3].className).toBe("imdx-task-kind");
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

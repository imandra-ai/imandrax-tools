// Task-artifact view: a flat table with one row per task, and a detail row under
// it holding whichever of its artifacts are open, rendered as escaped, scrollable
// <pre> text.
//
// Each row leads with the task's description and its result's description (both
// extracted on the Python side, `—` when missing); the level icon sits in the
// result cell, right of its text.
//
// Rows are sorted by level (most severe first), keeping a symbol's tasks
// together. The symbol is an ordinary column rather than a grouping level, since
// a snippet often has one task per symbol. Clicking a row toggles all of its
// artifacts; its chips toggle them one by one. Artifacts of warning / error
// tasks start open, others start collapsed. Debug tasks are hidden unless "show debug" in the
// header is ticked.
//
// `drawTasks(el, tasks)` builds the DOM, wires interaction, and returns nothing.

import { highlightRepr } from "./highlight";
import { ROOT_CLASS, TASK_STYLE } from "./style";
import type { Artifact, TaskData, TaskLevel } from "./types";

const LEVELS: TaskLevel[] = ["debug", "info", "warning", "error"];

const LEVEL_ICON: Record<TaskLevel, string> = {
  error: "❌",
  warning: "⚠️",
  info: "✅",
  debug: "💡",
};

// How far the pointer may move during a row click before it counts as a text
// selection rather than a toggle.
const DRAG_PX = 4;

// Tasks below this level are hidden, unless "show debug" is ticked.
const MIN_LEVEL: TaskLevel = "info";

function rank(level: TaskLevel): number {
  return LEVELS.indexOf(level);
}

function levelOf(task: TaskData): TaskLevel {
  return task.level ?? "info";
}

// `task:po:<hash>` -> `task:po:<first 6 of hash>`, same as `TaskEntry.name`.
function shortId(id: string): string {
  const [a, b, ...rest] = id.split(":");
  return rest.length ? `${a}:${b}:${rest.join(":").slice(0, 6)}` : id;
}

function el<K extends keyof HTMLElementTagNameMap>(
  tag: K,
  cls?: string,
  text?: string,
): HTMLElementTagNameMap[K] {
  const e = document.createElement(tag);
  if (cls) e.className = `${ROOT_CLASS}-${cls}`;
  if (text !== undefined) e.textContent = text;
  return e;
}

// A description cell; `—` when the description is missing. Its content sits in a
// flex box, so the result cell can append the level icon on the right.
function descrCell(cls: string, descr: string | null | undefined): HTMLElement {
  const td = el("td", cls);
  const box = el("div", "descr");
  const text = el("span", "descr-text", descr || "—");
  if (!descr) text.classList.add(`${ROOT_CLASS}-descr-none`);
  box.appendChild(text);
  td.appendChild(box);
  return td;
}

function makeArtifact(art: Artifact, onClose: () => void): HTMLElement {
  const box = el("div", "art");

  // Clicking the header closes the artifact, like clicking a symbol toggles its
  // rows; `×` makes that discoverable and just lets its click bubble up here.
  const head = el("div", "art-head");
  head.title = "Close";
  head.addEventListener("click", onClose);
  head.appendChild(el("span", "art-kind", art.kind));
  head.appendChild(el("span", "meta", `${art.repr.length.toLocaleString()} chars`));

  const copy = el("button", "copy", "copy");
  copy.type = "button";
  copy.title = "Copy";
  copy.addEventListener("click", (e) => {
    e.stopPropagation(); // copying shouldn't close the artifact
    navigator.clipboard?.writeText(art.repr).then(() => {
      copy.textContent = "copied";
      setTimeout(() => (copy.textContent = "copy"), 1200);
    });
  });
  head.appendChild(copy);

  const close = el("button", "close", "×");
  close.type = "button";
  close.setAttribute("aria-label", `Close ${art.kind}`);
  head.appendChild(close);
  box.appendChild(head);

  const scroll = el("div", "scroll");
  const pre = el("pre", "pre");
  pre.innerHTML = highlightRepr(art.repr); // tokens are HTML-escaped by highlightRepr
  scroll.appendChild(pre);
  box.appendChild(scroll);
  return box;
}

export function drawTasks(root: HTMLElement, tasks: TaskData[]): void {
  root.innerHTML = "";
  root.classList.add(ROOT_CLASS);

  const style = document.createElement("style");
  style.textContent = TASK_STYLE;
  root.appendChild(style);

  if (!tasks || tasks.length === 0) {
    root.appendChild(el("div", "placeholder", "No tasks."));
    return;
  }

  // Sort by level (most severe first), keeping tasks of one symbol together and
  // otherwise preserving the input order.
  const firstSeen = new Map<string, number>();
  tasks.forEach((t, i) => {
    const sym = t.from_sym ?? "";
    if (!firstSeen.has(sym)) firstSeen.set(sym, i);
  });
  const order = tasks
    .map((t, i) => ({ t, i }))
    .sort(
      (a, b) =>
        rank(levelOf(b.t)) - rank(levelOf(a.t)) ||
        firstSeen.get(a.t.from_sym ?? "")! - firstSeen.get(b.t.from_sym ?? "")! ||
        a.i - b.i,
    );

  // Per task (by input index), the kinds of its open artifacts.
  const open = new Map<number, Set<string>>();
  for (const { t, i } of order) {
    const loud = rank(levelOf(t)) >= rank("warning");
    open.set(i, new Set(loud ? t.artifacts.map((a) => a.kind) : []));
  }

  // Table
  // -----
  const table = el("table", "table");
  const thead = document.createElement("thead");
  const hr = document.createElement("tr");
  for (const h of ["task", "result", "symbol", "artifacts", "kind"]) {
    hr.appendChild(el("th", undefined, h));
  }
  // The last header cell also holds the "show debug" toggle, right-aligned.
  const idTh = el("th", undefined);
  const idHead = el("div", "id-head");
  idHead.appendChild(el("span", undefined, "id"));
  const debugLabel = el("label", "show-debug");
  const debugBox = document.createElement("input");
  debugBox.type = "checkbox";
  debugBox.addEventListener("change", () => renderRows());
  // Nothing to reveal: dim it, but leave it clickable.
  if (!tasks.some((t) => levelOf(t) === "debug")) {
    debugLabel.classList.add(`${ROOT_CLASS}-show-debug-none`);
    debugLabel.title = "No debug tasks";
  }
  debugLabel.append(debugBox, "show debug");
  idHead.appendChild(debugLabel);
  idTh.appendChild(idHead);
  hr.appendChild(idTh);
  thead.appendChild(hr);
  table.appendChild(thead);
  const tbody = document.createElement("tbody");
  table.appendChild(tbody);
  root.appendChild(table);

  const empty = el("div", "placeholder", "No tasks at info level or above.");
  root.appendChild(empty);

  // Each task's rows are built once and updated in place, so toggling one task
  // leaves the rest of the DOM -- text selections, scroll positions inside open
  // artifacts, focus -- untouched. Only "show debug" re-lays the rows out.
  const rowsOf = new Map<number, { row: HTMLElement; detail: HTMLElement }>();
  for (const { t, i } of order) rowsOf.set(i, buildRows(t, open.get(i)!));

  function renderRows(): void {
    tbody.innerHTML = "";
    const minRank = rank(debugBox.checked ? "debug" : MIN_LEVEL);
    const shown = order.filter(({ t }) => rank(levelOf(t)) >= minRank);
    // The header stays visible, so "show debug" can reveal hidden tasks.
    empty.hidden = shown.length > 0;
    for (const { i } of shown) {
      const { row, detail } = rowsOf.get(i)!;
      tbody.appendChild(row);
      if (!detail.hidden) tbody.appendChild(detail);
    }
  }

  // A task's row and its detail row (holding its open artifacts), plus the
  // interaction that keeps them in sync with `opened`.
  function buildRows(
    t: TaskData,
    opened: Set<string>,
  ): { row: HTMLElement; detail: HTMLElement } {
    const level = levelOf(t);
    const row = el("tr", "row");
    row.dataset.level = level;

    const detail = el("tr", "detail");
    const detailCell = document.createElement("td");
    detailCell.colSpan = 6;
    detail.appendChild(detailCell);
    // Artifact boxes by kind, made on first open and kept while open.
    const boxes = new Map<string, HTMLElement>();
    const chipOf = new Map<string, HTMLElement>();

    // Reflect `opened` in the chips, the row, and the detail row.
    const update = (): void => {
      for (const [kind, chip] of chipOf) {
        chip.setAttribute("aria-pressed", String(opened.has(kind)));
      }
      if (t.artifacts.length > 0) row.setAttribute("aria-expanded", String(opened.size > 0));
      // Keep artifacts in their own order, not the order they were opened.
      for (const art of t.artifacts) {
        let box = boxes.get(art.kind);
        if (!opened.has(art.kind)) {
          box?.remove();
          boxes.delete(art.kind);
          continue;
        }
        if (!box) {
          box = makeArtifact(art, () => {
            opened.delete(art.kind);
            update();
          });
          boxes.set(art.kind, box);
        }
        detailCell.appendChild(box); // (re-)appending in order keeps them sorted
      }
      detail.hidden = opened.size === 0;
      if (detail.hidden) detail.remove();
      else if (row.parentNode && row.nextSibling !== detail) row.after(detail);
    };

    // Open every artifact of the task, or close them all if all are open.
    const toggleAll = (): void => {
      const allOpen = opened.size === t.artifacts.length;
      opened.clear();
      if (!allOpen) for (const a of t.artifacts) opened.add(a.kind);
      update();
    };

    if (t.artifacts.length > 0) {
      row.classList.add(`${ROOT_CLASS}-row-toggle`);
      row.tabIndex = 0;
      row.title = "Toggle all artifacts";
      // Text stays selectable: a drag past DRAG_PX is a selection, not a click.
      // Every click of a double click toggles, so selecting a word by double
      // clicking leaves the row as it was; a triple click (selecting a line)
      // stops at two toggles for the same reason.
      let down: { x: number; y: number } | null = null;
      row.addEventListener("mousedown", (e) => (down = { x: e.clientX, y: e.clientY }));
      row.addEventListener("click", (e) => {
        const moved = down ? Math.hypot(e.clientX - down.x, e.clientY - down.y) > DRAG_PX : false;
        if (moved || e.detail > 2) return;
        // A sloppy single click may have selected a few characters; drop them.
        if (e.detail === 1) {
          const sel = window.getSelection();
          if (sel && !sel.isCollapsed && row.contains(sel.anchorNode)) sel.removeAllRanges();
        }
        toggleAll();
      });
      row.addEventListener("keydown", (e) => {
        if (e.target !== row || (e.key !== "Enter" && e.key !== " ")) return;
        e.preventDefault();
        toggleAll();
      });
    }

    row.appendChild(descrCell("task-descr", t.task_descr));
    const res = descrCell("res-descr", t.res_descr);
    const lvl = el("span", "level", LEVEL_ICON[level]);
    lvl.title = level;
    res.firstElementChild!.appendChild(lvl);
    row.appendChild(res);

    const sym = el("td", "sym", t.from_sym ?? "—");
    if (t.from_sym == null) sym.classList.add(`${ROOT_CLASS}-sym-none`);
    row.appendChild(sym);

    const chips = el("td", "chips");
    for (const art of t.artifacts) {
      const chip = el("button", "chip", art.kind);
      chip.type = "button";
      chip.addEventListener("click", (e) => {
        e.stopPropagation(); // a chip toggles one artifact, not the row's
        if (opened.has(art.kind)) opened.delete(art.kind);
        else opened.add(art.kind);
        update();
      });
      chipOf.set(art.kind, chip);
      chips.appendChild(chip);
    }
    row.appendChild(chips);

    row.appendChild(el("td", "kind", t.kind.replace(/^TASK_/, "")));

    const id = el("td", "id", shortId(t.id));
    id.title = t.id;
    row.appendChild(id);

    update();
    return { row, detail };
  }

  renderRows();
}

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
// a snippet often has one task per symbol. Debug tasks are hidden unless "show
// debug" in the header is ticked.
//
// Three controls, one job each:
//
//   chip / artifact `×`   → add / remove that artifact (added back expanded)
//   artifact title bar    → collapse / expand that artifact's body
//   row                   → fold / unfold the task's whole detail row, keeping
//                           which artifacts are open and collapsed as they were;
//                           with none open, it opens them all
//
// Folding and collapsing only hide, so an artifact's selection survives; its
// scroll position, which browsers drop on `display: none`, is saved and restored
// around the hide. A chip click on a folded task unfolds it, so the click always
// has a visible result; while folded, the chips of its open artifacts show a
// dimmed pressed state. Artifacts of warning / error tasks start open, others
// start closed.
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

// Scroll offsets of artifact bodies hidden by a collapse or a fold, restored when
// they show again. Only rendered bodies are saved: one already hidden (an
// artifact collapsed inside a task now folding) keeps the entry saved when it was
// hidden, and only restores once it is rendered again.
const savedScroll = new WeakMap<Element, { top: number; left: number }>();
const rendered = (e: Element): boolean => e.getClientRects().length > 0;

function saveScroll(scope: HTMLElement): void {
  for (const s of scope.querySelectorAll(`.${ROOT_CLASS}-scroll`)) {
    if (rendered(s)) savedScroll.set(s, { top: s.scrollTop, left: s.scrollLeft });
  }
}

function restoreScroll(scope: HTMLElement): void {
  for (const s of scope.querySelectorAll(`.${ROOT_CLASS}-scroll`)) {
    const saved = savedScroll.get(s);
    if (!saved || !rendered(s)) continue;
    s.scrollTop = saved.top;
    s.scrollLeft = saved.left;
    savedScroll.delete(s);
  }
}

function makeArtifact(art: Artifact, onRemove: () => void): HTMLElement {
  const box = el("div", "art");

  // Clicking the header collapses / expands the body; `×` removes the artifact.
  const head = el("div", "art-head");
  head.tabIndex = 0;
  const setCollapsed = (collapsed: boolean): void => {
    if (collapsed) saveScroll(box);
    box.classList.toggle(`${ROOT_CLASS}-art-collapsed`, collapsed);
    if (!collapsed) restoreScroll(box);
    head.setAttribute("aria-expanded", String(!collapsed));
    head.title = collapsed ? "Expand" : "Collapse";
  };
  const toggle = (): void =>
    setCollapsed(!box.classList.contains(`${ROOT_CLASS}-art-collapsed`));
  setCollapsed(false);
  head.addEventListener("click", toggle);
  head.addEventListener("keydown", (e) => {
    if (e.target !== head || (e.key !== "Enter" && e.key !== " ")) return;
    e.preventDefault();
    toggle();
  });
  head.appendChild(el("span", "art-kind", art.kind));
  head.appendChild(el("span", "meta", `${art.repr.length.toLocaleString()} chars`));

  const copy = el("button", "copy", "copy");
  copy.type = "button";
  copy.title = "Copy";
  copy.addEventListener("click", (e) => {
    e.stopPropagation(); // copying shouldn't collapse the artifact
    navigator.clipboard?.writeText(art.repr).then(() => {
      copy.textContent = "copied";
      setTimeout(() => (copy.textContent = "copy"), 1200);
    });
  });
  head.appendChild(copy);

  const close = el("button", "close", "×");
  close.type = "button";
  close.title = "Remove";
  close.setAttribute("aria-label", `Remove ${art.kind}`);
  close.addEventListener("click", (e) => {
    e.stopPropagation(); // removing, not collapsing
    onRemove();
  });
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

  // Per task (by input index), the kinds of its open artifacts; whether the task
  // is folded lives with its rows (see `buildRows`).
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
      // A folded task's detail row goes back in too, still hidden.
      if (open.get(i)!.size > 0) tbody.appendChild(detail);
    }
  }

  // A task's row and its detail row (holding its open artifacts), plus the
  // interaction that keeps them in sync with `opened` and `folded`.
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
    // The detail row is hidden but kept, open artifacts and all.
    let folded = false;

    // Reflect `opened` and `folded` in the chips, the row, and the detail row.
    const update = (): void => {
      for (const [kind, chip] of chipOf) {
        chip.setAttribute("aria-pressed", String(opened.has(kind)));
      }
      if (opened.size === 0) folded = false; // nothing left to fold
      row.classList.toggle(`${ROOT_CLASS}-row-folded`, folded);
      if (t.artifacts.length > 0) {
        row.setAttribute("aria-expanded", String(opened.size > 0 && !folded));
      }
      // Keep artifacts in their own order, not the order they were opened. A box
      // moves only when out of place: moving one resets its scroll position.
      let prev: HTMLElement | null = null;
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
        const at: ChildNode | null = prev ? prev.nextSibling : detailCell.firstChild;
        if (at !== box) detailCell.insertBefore(box, at);
        prev = box;
      }
      // Folded, the detail row stays in place, hidden rather than detached.
      if (folded && !detail.hidden) saveScroll(detail);
      const unfolding = !folded && detail.hidden;
      detail.hidden = folded;
      if (opened.size === 0) detail.remove();
      else if (row.parentNode && row.nextSibling !== detail) row.after(detail);
      if (unfolding) restoreScroll(detail);
    };

    // Fold / unfold the task's detail row; with no artifact open, open them all.
    const toggleRow = (): void => {
      if (opened.size === 0) for (const a of t.artifacts) opened.add(a.kind);
      else folded = !folded;
      update();
    };

    if (t.artifacts.length > 0) {
      row.classList.add(`${ROOT_CLASS}-row-toggle`);
      row.tabIndex = 0;
      row.title = "Show / hide artifacts";
      // Text stays selectable: a drag past DRAG_PX is a selection, not a click.
      // Every click of a double click toggles, so selecting a word by double
      // clicking leaves the row as it was; a triple click (selecting a line)
      // stops at two toggles for the same reason.
      let down: { x: number; y: number } | null = null;
      row.addEventListener("mousedown", (e) => (down = { x: e.clientX, y: e.clientY }));
      row.addEventListener("click", (e) => {
        // Consume the press so a later click with no mousedown of its own
        // (assistive tech, `row.click()`) isn't measured against it.
        const start = down;
        down = null;
        const moved = start ? Math.hypot(e.clientX - start.x, e.clientY - start.y) > DRAG_PX : false;
        if (moved || e.detail > 2) return;
        // A sloppy single click may have selected a few characters; drop them.
        if (e.detail === 1) {
          const sel = window.getSelection();
          if (sel && !sel.isCollapsed && row.contains(sel.anchorNode)) sel.removeAllRanges();
        }
        toggleRow();
      });
      row.addEventListener("keydown", (e) => {
        if (e.target !== row || (e.key !== "Enter" && e.key !== " ")) return;
        e.preventDefault();
        toggleRow();
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
        folded = false; // so the click has a visible result
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

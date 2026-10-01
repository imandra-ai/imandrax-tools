// Task-artifact view: a flat table with one row per task, and a detail row under
// it holding whichever of its artifacts are open, rendered as escaped, scrollable
// <pre> text.
//
// Rows are sorted by level (most severe first). The symbol is an ordinary column
// rather than a grouping level, since a snippet often has one task per symbol;
// consecutive rows of the same symbol only print it once, and clicking it toggles
// all artifacts of those rows. Artifacts of warning / error tasks start open,
// others start collapsed. Debug tasks are hidden unless "show debug" in the
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

// Tasks below this level are hidden, unless "show debug" is ticked.
const MIN_LEVEL: TaskLevel = "info";

function rank(level: TaskLevel): number {
  return LEVELS.indexOf(level);
}

function levelOf(task: TaskData): TaskLevel {
  return task.level ?? "debug";
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
  for (const h of ["", "symbol", "artifacts", "kind"]) hr.appendChild(el("th", undefined, h));
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

  function renderRows(): void {
    tbody.innerHTML = "";
    const minRank = rank(debugBox.checked ? "debug" : MIN_LEVEL);
    const shown = order.filter(({ t }) => rank(levelOf(t)) >= minRank);
    // The header stays visible, so "show debug" can reveal hidden tasks.
    empty.hidden = shown.length > 0;

    // Open every artifact of `group`'s tasks, or close them all if all are open.
    const toggleAll = (group: typeof shown): void => {
      const allOpen = group.every(({ t, i }) => open.get(i)!.size === t.artifacts.length);
      for (const { t, i } of group) {
        open.set(i, new Set(allOpen ? [] : t.artifacts.map((a) => a.kind)));
      }
      renderRows();
    };

    // The run of rows the current symbol heads, and their symbol cells.
    let prevSym: string | null | undefined;
    let group: typeof shown = [];
    let groupCells: HTMLElement[] = [];
    for (const [pos, { t, i }] of shown.entries()) {
      const level = levelOf(t);
      const row = el("tr", "row");
      row.dataset.level = level;

      const lvl = el("td", "level", LEVEL_ICON[level]);
      lvl.title = level;
      row.appendChild(lvl);

      // Only the first of consecutive rows sharing a symbol prints it, but every
      // cell of the run toggles the whole run, and they highlight together.
      const sym = t.from_sym ?? null;
      const symCell = el("td", "sym");
      if (sym === null || sym !== prevSym) {
        // A task without a symbol is a run of its own.
        let end = pos + 1;
        while (sym !== null && end < shown.length && shown[end].t.from_sym === sym) end++;
        group = shown.slice(pos, end);
        groupCells = [];

        // A real button for keyboard focus; its click bubbles to the cell.
        const btn = el("button", "sym-btn", sym ?? "—");
        btn.type = "button";
        if (sym === null) btn.classList.add(`${ROOT_CLASS}-sym-none`);
        symCell.appendChild(btn);
      }
      prevSym = sym;
      const [runRows, runCells] = [group, groupCells];
      runCells.push(symCell);
      symCell.title = "Toggle all artifacts";
      symCell.addEventListener("click", () => toggleAll(runRows));
      const hover = (on: boolean) => () =>
        runCells.forEach((c) => c.classList.toggle(`${ROOT_CLASS}-sym-hover`, on));
      symCell.addEventListener("mouseenter", hover(true));
      symCell.addEventListener("mouseleave", hover(false));
      row.appendChild(symCell);

      const chips = el("td", "chips");
      const opened = open.get(i)!;
      for (const art of t.artifacts) {
        const chip = el("button", "chip", art.kind);
        chip.type = "button";
        chip.setAttribute("aria-pressed", String(opened.has(art.kind)));
        chip.addEventListener("click", () => {
          if (opened.has(art.kind)) opened.delete(art.kind);
          else opened.add(art.kind);
          renderRows();
        });
        chips.appendChild(chip);
      }
      row.appendChild(chips);

      row.appendChild(el("td", "kind", t.kind.replace(/^TASK_/, "")));

      const id = el("td", "id", shortId(t.id));
      id.title = t.id;
      row.appendChild(id);
      tbody.appendChild(row);

      if (opened.size > 0) {
        const detail = el("tr", "detail");
        const cell = document.createElement("td");
        cell.colSpan = 5;
        // Keep artifacts in their own order, not the order they were opened.
        for (const art of t.artifacts) {
          if (!opened.has(art.kind)) continue;
          cell.appendChild(
            makeArtifact(art, () => {
              opened.delete(art.kind);
              renderRows();
            }),
          );
        }
        detail.appendChild(cell);
        tbody.appendChild(detail);
      }
    }
  }

  renderRows();
}

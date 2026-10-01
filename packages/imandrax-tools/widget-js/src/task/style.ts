// Scoped styles for the task-artifact view. Namespaced under `.imdx-task` and
// injected once per widget root, mirroring the region-decomposition views'
// palette (borders #d8dde2, muted #6b727b, code bg #eef1f4) so the two look of a
// piece when a decomp artifact renders its treemap inside a task.

export const ROOT_CLASS = "imdx-task";

export const TASK_STYLE = `
.${ROOT_CLASS} { display: flex; flex-direction: column; gap: 8px;
  font-family: ui-sans-serif, system-ui, sans-serif; font-size: 12px;
  color: #1a1d21; box-sizing: border-box; }
.${ROOT_CLASS} *, .${ROOT_CLASS} *::before, .${ROOT_CLASS} *::after { box-sizing: border-box; }

.${ROOT_CLASS}-table { width: 100%; border-collapse: collapse; border: 1px solid #d8dde2;
  border-radius: 6px; overflow: hidden; background: #fafbfc; }
.${ROOT_CLASS}-table th { text-align: left; font-weight: 600; color: #6b727b; font-size: 11px;
  padding: 5px 10px; border-bottom: 1px solid #d8dde2; background: #fff; }
.${ROOT_CLASS}-table td { padding: 4px 10px; vertical-align: middle; }
.${ROOT_CLASS}-row + .${ROOT_CLASS}-row > td,
.${ROOT_CLASS}-detail + .${ROOT_CLASS}-row > td { border-top: 1px solid #eef1f4; }
.${ROOT_CLASS}-row[data-level="error"] { background: #fff5f5; }
.${ROOT_CLASS}-row[data-level="warning"] { background: #fffaeb; }
.${ROOT_CLASS}-level { width: 24px; text-align: center; color: #9aa1a9; }
.${ROOT_CLASS}-sym { cursor: pointer; font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-weight: 600; }
.${ROOT_CLASS}-sym-btn { font: inherit; color: inherit; background: none; border: 0;
  padding: 0; cursor: pointer; text-align: left; }
.${ROOT_CLASS}-sym-none { color: #9aa1a9; font-weight: 400; }
/* Same shade as an artifact title on hover. */
.${ROOT_CLASS}-table td.${ROOT_CLASS}-sym-hover { background: #f6f8fa; }
.${ROOT_CLASS}-kind { color: #6b727b; font-size: 11px; letter-spacing: 0.02em; }
.${ROOT_CLASS}-id { color: #9aa1a9; font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 11px; white-space: nowrap; }
.${ROOT_CLASS}-id-head { display: flex; align-items: center; gap: 10px; }
.${ROOT_CLASS}-show-debug { margin-left: auto; display: inline-flex; align-items: center; gap: 4px;
  font-weight: 400; white-space: nowrap; cursor: pointer; user-select: none; }
.${ROOT_CLASS}-show-debug input { margin: 0; cursor: pointer; }
.${ROOT_CLASS}-show-debug-none { opacity: 0.5; }
.${ROOT_CLASS}-meta { color: #6b727b; font-size: 11px; font-variant-numeric: tabular-nums; }

.${ROOT_CLASS}-chips { display: flex; gap: 4px; flex-wrap: wrap; }
.${ROOT_CLASS}-chip { font: inherit; font-size: 11px; cursor: pointer; padding: 1px 7px;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  color: #6b727b; background: #fff; border: 1px solid #d8dde2; border-radius: 10px; }
.${ROOT_CLASS}-chip:hover { color: #1a1d21; border-color: #b7c0c9; }
.${ROOT_CLASS}-chip[aria-pressed="true"] { color: #1a1d21; background: #e3e8ee; border-color: #b7c0c9; }

.${ROOT_CLASS}-detail > td { padding: 0 10px 8px 34px; }
.${ROOT_CLASS}-detail > td > * + * { margin-top: 6px; }
.${ROOT_CLASS}-art { border: 1px solid #d8dde2; border-radius: 6px; overflow: hidden;
  background: #fff; }
.${ROOT_CLASS}-art-head { display: flex; align-items: center; gap: 8px; padding: 4px 10px;
  cursor: pointer; user-select: none; }
.${ROOT_CLASS}-art-head:hover { background: #f6f8fa; }
.${ROOT_CLASS}-art-kind { font-weight: 600; color: #1a1d21;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }

.${ROOT_CLASS}-copy { margin-left: auto; }
.${ROOT_CLASS}-copy, .${ROOT_CLASS}-close { font: inherit; font-size: 11px; color: #6b727b;
  background: transparent; border: 1px solid #d8dde2; border-radius: 4px; padding: 1px 6px;
  cursor: pointer; }
.${ROOT_CLASS}-copy:hover, .${ROOT_CLASS}-close:hover { color: #1a1d21; border-color: #b7c0c9; }

.${ROOT_CLASS}-scroll { max-height: 720px; overflow: auto; border-top: 1px solid #d8dde2; }
.${ROOT_CLASS}-pre { margin: 0; padding: 10px; white-space: pre; tab-size: 2; font-size: 12px;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }

/* Syntax highlighting for the Python-repr artifact text (see task/highlight.ts).
   Light palette tuned for the #fff code bg. */
.${ROOT_CLASS}-pre .t-cls { color: #8250df; }   /* constructor / class names */
.${ROOT_CLASS}-pre .t-attr { color: #0550ae; }  /* keyword-arg names */
.${ROOT_CLASS}-pre .t-str { color: #0a7d33; }   /* string literals */
.${ROOT_CLASS}-pre .t-num { color: #953800; }   /* numbers */
.${ROOT_CLASS}-pre .t-lit { color: #cf222e; }   /* None / True / False */

.${ROOT_CLASS}-placeholder { color: #9aa1a9; font-style: italic; padding: 8px; }
`;

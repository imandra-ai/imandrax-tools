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

.${ROOT_CLASS}-task, .${ROOT_CLASS}-art { border: 1px solid #d8dde2; border-radius: 6px;
  overflow: hidden; }
.${ROOT_CLASS}-task { background: #fafbfc; }

.${ROOT_CLASS}-summary { display: flex; align-items: center; gap: 8px; padding: 6px 10px;
  cursor: pointer; user-select: none; list-style: none; }
.${ROOT_CLASS}-summary::-webkit-details-marker { display: none; }
.${ROOT_CLASS}-summary::before { content: "\\25B8"; color: #6b727b; font-size: 10px;
  transition: transform 0.12s ease; }
details[open] > .${ROOT_CLASS}-summary::before { transform: rotate(90deg); }

.${ROOT_CLASS}-kind { font-weight: 600; letter-spacing: 0.02em; }
.${ROOT_CLASS}-id { color: #6b727b; font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 11px; }
.${ROOT_CLASS}-meta { margin-left: auto; color: #6b727b; font-size: 11px;
  font-variant-numeric: tabular-nums; }

.${ROOT_CLASS}-body { padding: 8px; display: flex; flex-direction: column; gap: 8px; }
.${ROOT_CLASS}-art { background: #fff; }
.${ROOT_CLASS}-art-icon { font-size: 12px; line-height: 1; }
.${ROOT_CLASS}-art-kind { font-weight: 600; color: #1a1d21;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }

.${ROOT_CLASS}-copy { margin-left: auto; font: inherit; font-size: 11px; color: #6b727b;
  background: transparent; border: 1px solid #d8dde2; border-radius: 4px; padding: 1px 6px;
  cursor: pointer; }
.${ROOT_CLASS}-copy:hover { color: #1a1d21; border-color: #b7c0c9; }

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

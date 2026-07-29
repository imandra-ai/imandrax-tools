// Scoped styles for the jsonable view. Namespaced under `.imdx-jsonable`, injected
// once per widget root, sharing the task/region-decomposition palette (borders
// #d8dde2, muted #6b727b, code bg #fff on a #fafbfc chrome) so the widgets look
// of a piece.
//
// Layout note: nesting is conveyed by the source line's own leading spaces, not
// by per-level padding — that keeps every column exactly where PyYAML put it.
// The fold arrow therefore lives in a fixed-width gutter present on *every* line
// (empty for leaves), so it shifts all lines equally.

export const ROOT_CLASS = 'imdx-jsonable';

export const JSONABLE_STYLE = `
.${ROOT_CLASS} { font-family: ui-sans-serif, system-ui, sans-serif; font-size: 12px;
  color: #1a1d21; border: 1px solid #d8dde2; border-radius: 6px; overflow: hidden;
  background: #fff; box-sizing: border-box; }
.${ROOT_CLASS} *, .${ROOT_CLASS} *::before, .${ROOT_CLASS} *::after { box-sizing: border-box; }

.${ROOT_CLASS}-bar { display: flex; align-items: center; gap: 8px; padding: 6px 10px;
  background: #fafbfc; border-bottom: 1px solid #d8dde2; }
.${ROOT_CLASS}-label { font-weight: 600; letter-spacing: 0.02em; }
.${ROOT_CLASS}-meta { color: #6b727b; font-size: 11px; font-variant-numeric: tabular-nums; }
.${ROOT_CLASS}-actions { margin-left: auto; display: flex; gap: 6px; }
.${ROOT_CLASS}-btn { font: inherit; font-size: 11px; color: #6b727b; background: transparent;
  border: 1px solid #d8dde2; border-radius: 4px; padding: 1px 6px; cursor: pointer; }
.${ROOT_CLASS}-btn:hover { color: #1a1d21; border-color: #b7c0c9; }

.${ROOT_CLASS}-scroll { max-height: 720px; overflow: auto; padding: 8px 0; }
.${ROOT_CLASS}-doc { font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 12px; line-height: 1.5; tab-size: 2; }

.${ROOT_CLASS}-line { display: flex; align-items: baseline; padding: 0 10px 0 4px; }
.${ROOT_CLASS}-line:hover { background: #f4f6f8; }
summary.${ROOT_CLASS}-line { cursor: pointer; user-select: none; list-style: none; }
summary.${ROOT_CLASS}-line::-webkit-details-marker { display: none; }

/* The fold gutter: same width on foldable and leaf lines, so text stays aligned. */
.${ROOT_CLASS}-arrow { flex: 0 0 1.1em; color: #9aa1a9; font-size: 9px; line-height: 1.7;
  text-align: center; }
summary.${ROOT_CLASS}-line > .${ROOT_CLASS}-arrow::before { content: "\\25B8"; display: inline-block;
  transition: transform 0.12s ease; }
details[open] > summary.${ROOT_CLASS}-line > .${ROOT_CLASS}-arrow::before { transform: rotate(90deg); }
summary.${ROOT_CLASS}-line:hover > .${ROOT_CLASS}-arrow { color: #1a1d21; }

.${ROOT_CLASS}-text { white-space: pre; }
.${ROOT_CLASS}-count { margin-left: 10px; color: #9aa1a9; font-size: 11px; font-style: italic;
  font-variant-numeric: tabular-nums; }
details[open] > summary > .${ROOT_CLASS}-count { display: none; }

/* Block-scalar bodies (\`key: |\`) — opaque text, dimmed and rendered verbatim. */
.${ROOT_CLASS}-block { margin: 0; padding: 0 10px 0 calc(1.1em + 4px); white-space: pre;
  color: #3c4249; }

/* Token colors (see jsonable/highlight.ts); light palette tuned for the #fff bg. */
.${ROOT_CLASS}-text .t-key { color: #0550ae; }      /* mapping keys */
.${ROOT_CLASS}-text .t-str { color: #0a7d33; }      /* quoted and plain scalars */
.${ROOT_CLASS}-text .t-num { color: #953800; }      /* numbers */
.${ROOT_CLASS}-text .t-lit { color: #cf222e; }      /* true / false / null / ~ */
.${ROOT_CLASS}-text .t-punct { color: #6b727b; }    /* \`-\`, \`:\`, \`---\` */
.${ROOT_CLASS}-text .t-ref { color: #8250df; }      /* anchors / aliases / tags */
.${ROOT_CLASS}-text .t-block { color: #8250df; }    /* \`|\` / \`>\` indicators */
.${ROOT_CLASS}-text .t-comment { color: #9aa1a9; font-style: italic; }

.${ROOT_CLASS}-placeholder { color: #9aa1a9; font-style: italic; padding: 10px; }
`;

// Scoped styles for the IDF graph view. Everything is namespaced under
// `.imdx-idf` and injected once per widget root, so the view is self-contained
// in any host without a separate stylesheet.
//
// The detail-panel block deliberately mirrors the region-decomposition widget's
// panel (`.k` / `pre` / `ol` conventions) so the two widgets read as a family.

export const ROOT_CLASS = 'imdx-idf';

export const IDF_STYLE = `
.${ROOT_CLASS} { display: flex; height: var(--imdx-h); border: 1px solid #d8dde2;
  border-radius: 6px; overflow: hidden; font-family: ui-sans-serif, system-ui, sans-serif;
  font-size: 12px; color: #1a1d21; background: #fff; box-sizing: border-box; }
.${ROOT_CLASS} *, .${ROOT_CLASS} *::before, .${ROOT_CLASS} *::after { box-sizing: border-box; }

/* Graph pane: a scrollable canvas holding an SVG (bands + edges) under
   absolutely-positioned node cards. */
.${ROOT_CLASS}-main { flex: 1 1 auto; min-width: 0; overflow: auto; position: relative;
  background:
    linear-gradient(#fbfcfd, #fbfcfd); }
.${ROOT_CLASS}-canvas { position: relative; }
.${ROOT_CLASS}-svg { position: absolute; inset: 0; overflow: visible; }
.${ROOT_CLASS}-edge { fill: none; stroke: #c2ccd6; stroke-width: 1.5px; }
.${ROOT_CLASS}-edge.is-active { stroke: #4c78c9; stroke-width: 2px; }

/* Step bands: a translucent rounded row behind each step's cards, with a
   clickable label at its left edge. */
.${ROOT_CLASS}-band { fill: #f1f4f8; stroke: #e4e9ef; rx: 8px; cursor: pointer; }
.${ROOT_CLASS}-band.is-root { fill: #f7f8fa; }
.${ROOT_CLASS}-band.is-selected { fill: #eaf1fb; stroke: #b9cdec; }
.${ROOT_CLASS}-bandlabel { cursor: pointer; }
.${ROOT_CLASS}-bandlabel .step-n { font-weight: 700; fill: #4c78c9;
  font-variant-numeric: tabular-nums; }
.${ROOT_CLASS}-bandlabel .step-msg { fill: #6b727b; }
.${ROOT_CLASS}-band-hit { fill: transparent; cursor: pointer; }

/* Node cards: color + shape vary by kind (root / leaf / interior). */
.${ROOT_CLASS}-node { position: absolute; overflow: hidden; cursor: pointer;
  background: #fff; border: 1px solid #d2dae3; border-left: 3px solid #4c78c9;
  border-radius: 6px; padding: 5px 8px; line-height: 1.25;
  box-shadow: 0 1px 2px rgba(20, 30, 45, 0.06);
  transition: box-shadow 0.1s ease, outline-color 0.1s ease; }
.${ROOT_CLASS}-node:hover { box-shadow: 0 2px 6px rgba(20, 30, 45, 0.14); }
.${ROOT_CLASS}-node.is-leaf { border-left-color: #2f9e6f; border-color: #bfe0cf; }
.${ROOT_CLASS}-node.is-root { border-left: 1px solid #ccd3db; border-radius: 999px;
  background: #f3f5f7; text-align: center; }
.${ROOT_CLASS}-node.is-selected { outline: 2px solid #d6336c; outline-offset: -1px; }

.${ROOT_CLASS}-nhead { display: flex; align-items: center; gap: 6px; }
.${ROOT_CLASS}-nid { font-weight: 700; font-variant-numeric: tabular-nums; }
.${ROOT_CLASS}-badge { font-size: 9px; font-weight: 700; letter-spacing: 0.04em;
  text-transform: uppercase; padding: 1px 5px; border-radius: 999px;
  background: #e3f3ea; color: #1f7a54; }
.${ROOT_CLASS}-ncst { margin-top: 3px; color: #4a525b; opacity: 0.92;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-size: 10.5px;
  display: -webkit-box; -webkit-line-clamp: 2; -webkit-box-orient: vertical;
  overflow: hidden; }

/* Detail panel (mirrors the region-decomp panel). */
.${ROOT_CLASS}-divider { flex: 0 0 1px; background: #d8dde2; }
.${ROOT_CLASS}-detail { flex: 0 0 var(--imdx-dw); overflow-y: auto; padding: 14px 16px;
  background: #fafbfc; }
.${ROOT_CLASS}-detail h3 { margin: 0 0 2px; font-size: 13px; font-variant-numeric: tabular-nums;
  display: flex; align-items: center; gap: 8px; }
.${ROOT_CLASS}-detail .sub { color: #6b727b; margin: 0 0 12px; }
.${ROOT_CLASS}-detail .k { font-weight: 600; margin: 12px 0 4px; font-size: 11px;
  text-transform: uppercase; letter-spacing: 0.04em; color: #6b727b; }
.${ROOT_CLASS}-detail code, .${ROOT_CLASS}-detail pre { font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 12px; }
.${ROOT_CLASS}-detail pre { white-space: pre-wrap; word-break: break-word; margin: 0;
  background: #eef1f4; padding: 7px 9px; border-radius: 4px; }
.${ROOT_CLASS}-detail ol { margin: 0; padding-left: 18px; }
.${ROOT_CLASS}-detail ol li { margin: 2px 0; }
.${ROOT_CLASS}-detail ol li.cur code { background: #ffe8ef; }
.${ROOT_CLASS}-detail .stats { display: flex; flex-wrap: wrap; gap: 4px 14px; margin: 4px 0; }
.${ROOT_CLASS}-detail .stats span b { font-variant-numeric: tabular-nums; }
.${ROOT_CLASS}-tag { font-size: 10px; font-weight: 700; padding: 1px 7px; border-radius: 999px; }
.${ROOT_CLASS}-tag.-leaf { background: #e3f3ea; color: #1f7a54; }
.${ROOT_CLASS}-tag.-interior { background: #e7eefb; color: #315fa8; }
.${ROOT_CLASS}-tag.-root { background: #eceef1; color: #6b727b; }
.${ROOT_CLASS}-placeholder { color: #9aa1a9; font-style: italic; }
`;

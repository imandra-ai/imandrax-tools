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

/* Graph pane: a pan/zoom viewport holding a transformed canvas (SVG edges under
   absolutely-positioned node cards), with a sticky step axis down the left. */
.${ROOT_CLASS}-main { flex: 1 1 auto; min-width: 0; position: relative; overflow: hidden;
  background: #fbfcfd; user-select: none; -webkit-user-select: none; }

/* Reset-view button, floating top-right of the graph pane. */
.${ROOT_CLASS}-reset { position: absolute; top: 8px; right: 8px; z-index: 4;
  border: 1px solid #d2dae3; background: #fff; color: #3a424c; cursor: pointer;
  font: inherit; font-size: 11px; padding: 4px 9px; border-radius: 6px;
  box-shadow: 0 1px 2px rgba(20, 30, 45, 0.08); }
.${ROOT_CLASS}-reset:hover { background: #eef1f4; color: #1a1d21; }
.${ROOT_CLASS}-viewport { position: absolute; inset: 0; overflow: hidden; cursor: grab;
  touch-action: none; }
.${ROOT_CLASS}-viewport.is-panning { cursor: grabbing; }
.${ROOT_CLASS}-canvas { position: absolute; top: 0; left: 0; transform-origin: 0 0; }
.${ROOT_CLASS}-svg { position: absolute; inset: 0; overflow: visible; }
.${ROOT_CLASS}-edge { fill: none; stroke: #c7d0da; stroke-width: 1.4px; }
.${ROOT_CLASS}-edge.is-active { stroke: #4c78c9; stroke-width: 2.2px; }

/* Step hulls: a soft rounded region around a step's nodes. One flat colour --
   we paint fill + a fat round-joined stroke opaque, then drop the whole path's
   opacity, so the self-overlap doesn't compound into a two-tone rim. */
.${ROOT_CLASS}-hull { fill: #8ea4c6; stroke: #8ea4c6; stroke-width: 42px;
  stroke-linejoin: round; stroke-linecap: round; opacity: 0.16; cursor: pointer;
  transition: opacity 0.1s ease; }
.${ROOT_CLASS}-hull:hover { opacity: 0.24; }

/* Sticky step axis: labels float over the left edge, tracking each stratum. */
.${ROOT_CLASS}-axis { position: absolute; left: 0; top: 0; height: 100%; width: var(--imdx-axis);
  pointer-events: none; z-index: 3;
  background: linear-gradient(to right, #fbfcfd 62%, rgba(251, 252, 253, 0)); }
.${ROOT_CLASS}-axislabel { position: absolute; left: 8px; transform: translateY(-50%);
  max-width: calc(var(--imdx-axis) - 12px); pointer-events: auto; cursor: pointer;
  border: 0; background: transparent; font: inherit; text-align: left;
  padding: 2px 5px; border-radius: 5px; }
.${ROOT_CLASS}-axislabel:hover { background: #eef1f4; }
.${ROOT_CLASS}-axislabel.is-selected { background: #eaf1fb; outline: 1px solid #b9cdec; }
.${ROOT_CLASS}-axislabel .step-n { display: block; font-weight: 700; font-size: 11px;
  color: #4c78c9; font-variant-numeric: tabular-nums; }
.${ROOT_CLASS}-axislabel .step-msg { display: block; font-size: 10px; color: #6b727b;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.${ROOT_CLASS}-axislabel.is-root { color: #8a929c; font-weight: 700; cursor: default; }
.${ROOT_CLASS}-axislabel.is-root:hover { background: transparent; }

/* Node cards: compact; color + shape vary by kind (root / leaf / interior). */
.${ROOT_CLASS}-node { position: absolute; overflow: hidden; cursor: pointer;
  background: #fff; border: 1px solid #d2dae3; border-left: 3px solid #4c78c9;
  border-radius: 6px; padding: 4px 7px; line-height: 1.15;
  box-shadow: 0 1px 2px rgba(20, 30, 45, 0.07);
  transition: box-shadow 0.1s ease, outline-color 0.1s ease; }
.${ROOT_CLASS}-node:hover { box-shadow: 0 2px 7px rgba(20, 30, 45, 0.16); }
.${ROOT_CLASS}-node.is-dragging { cursor: grabbing; z-index: 5;
  box-shadow: 0 4px 12px rgba(20, 30, 45, 0.22); }
.${ROOT_CLASS}-node.is-leaf { border-left-color: #2f9e6f; border-color: #bfe0cf; }
.${ROOT_CLASS}-node.is-root { border: 1px solid #ccd3db; border-radius: 999px;
  background: #f3f5f7; display: flex; align-items: center; justify-content: center; }
.${ROOT_CLASS}-node.is-selected { outline: 2px solid #d6336c; outline-offset: -1px; }
.${ROOT_CLASS}-nhead { display: flex; align-items: center; gap: 5px; }
.${ROOT_CLASS}-nid { font-weight: 700; font-variant-numeric: tabular-nums; }
.${ROOT_CLASS}-badge { font-size: 8.5px; font-weight: 700; letter-spacing: 0.04em;
  text-transform: uppercase; padding: 0 4px; border-radius: 999px;
  background: #e3f3ea; color: #1f7a54; }
.${ROOT_CLASS}-ninv { margin-top: 2px; color: #4a525b;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-size: 10px;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }

/* Detail panel (mirrors the region-decomp panel). */
.${ROOT_CLASS}-divider { flex: 0 0 1px; background: #d8dde2; }
.${ROOT_CLASS}-detail { flex: 0 0 var(--imdx-dw); overflow-y: auto; padding: 14px 16px;
  background: #fafbfc; user-select: text; -webkit-user-select: text; }
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
.${ROOT_CLASS}-detail .stats { display: flex; flex-wrap: wrap; gap: 4px 14px; margin: 4px 0; }
.${ROOT_CLASS}-detail .stats span b { font-variant-numeric: tabular-nums; }
.${ROOT_CLASS}-tag { font-size: 10px; font-weight: 700; padding: 1px 7px; border-radius: 999px; }
.${ROOT_CLASS}-tag.-leaf { background: #e3f3ea; color: #1f7a54; }
.${ROOT_CLASS}-tag.-interior { background: #e7eefb; color: #315fa8; }
.${ROOT_CLASS}-tag.-root { background: #eceef1; color: #6b727b; }
.${ROOT_CLASS}-placeholder { color: #9aa1a9; font-style: italic; }
`;

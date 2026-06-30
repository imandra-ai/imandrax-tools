// Scoped styles for the region-decomposition views. Everything is namespaced
// under `.imdx-rd` and injected once per widget root, so a view is
// self-contained in any host without a separate stylesheet. Each view injects
// SHARED_STYLE plus its own layout block.

export const ROOT_CLASS = "imdx-rd";

// Shared: the flex container, the detail panel, and the cell/tile label spans —
// everything both the icicle and the treemap use.
export const SHARED_STYLE = `
.${ROOT_CLASS} { display: flex; height: var(--imdx-h); border: 1px solid #d8dde2;
  border-radius: 6px; overflow: hidden; font-family: ui-sans-serif, system-ui, sans-serif;
  font-size: 12px; color: #1a1d21; background: #fff; box-sizing: border-box; }
.${ROOT_CLASS} *, .${ROOT_CLASS} *::before, .${ROOT_CLASS} *::after { box-sizing: border-box; }
.${ROOT_CLASS}-lp { font-weight: 600; font-variant-numeric: tabular-nums; }
.${ROOT_CLASS}-cst { opacity: 0.78; font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }
.${ROOT_CLASS}-divider { flex: 0 0 1px; background: #d8dde2; }
.${ROOT_CLASS}-detail { flex: 0 0 var(--imdx-dw); overflow-y: auto; padding: 14px 16px;
  background: #fafbfc; }
.${ROOT_CLASS}-detail h3 { margin: 0 0 2px; font-size: 13px; font-variant-numeric: tabular-nums; }
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
.${ROOT_CLASS}-placeholder { color: #9aa1a9; font-style: italic; }
`;

// Icicle: depth-rows of absolutely-positioned cells in a scrolling chart pane.
export const ICICLE_STYLE = `
.${ROOT_CLASS}-chart { flex: 1 1 auto; min-width: 0; overflow: auto; position: relative; }
.${ROOT_CLASS}-canvas { position: relative; }
.${ROOT_CLASS}-cell { position: absolute; overflow: hidden; cursor: pointer;
  border-radius: 3px; padding: 3px 6px; line-height: 1.25; white-space: nowrap;
  transition: filter 0.08s ease, outline-color 0.08s ease;
  outline: 1.5px solid transparent; outline-offset: -1.5px; }
.${ROOT_CLASS}-cell:hover { filter: brightness(0.94); }
.${ROOT_CLASS}-cell.is-onpath { outline-color: #1a1d21; }
.${ROOT_CLASS}-cell.is-selected { outline-color: #d6336c; outline-width: 2px; }
`;

// Treemap: a breadcrumb top bar above a pane of nested tiles. Tiles animate
// their geometry on zoom via a CSS transition.
export const TREEMAP_STYLE = `
.${ROOT_CLASS}-main { flex: 1 1 auto; min-width: 0; display: flex; flex-direction: column; }
.${ROOT_CLASS}-topbar { flex: 0 0 auto; display: flex; align-items: center; flex-wrap: wrap;
  gap: 2px; padding: 7px 10px; border-bottom: 1px solid #eceef1; min-height: 30px;
  font-variant-numeric: tabular-nums; }
.${ROOT_CLASS}-crumb { border: 0; background: transparent; padding: 1px 4px; border-radius: 4px;
  font: inherit; color: #6b727b; cursor: pointer; }
.${ROOT_CLASS}-crumb:hover { background: #eef1f4; color: #1a1d21; }
.${ROOT_CLASS}-crumb.-current { color: #1a1d21; font-weight: 600; cursor: default; }
.${ROOT_CLASS}-crumb.-current:hover { background: transparent; }
.${ROOT_CLASS}-sep { color: #c4cad1; padding: 0 1px; }
.${ROOT_CLASS}-tiles { position: relative; flex: 1 1 auto; min-height: 0; overflow: hidden; }
.${ROOT_CLASS}-tile { position: absolute; overflow: hidden; cursor: pointer;
  border-radius: 3px; padding: 3px 6px; line-height: 1.2; white-space: nowrap;
  background:rgb(114, 148, 186);
  border: 1px solid rgba(255, 255, 255, 0.85);
  transition: left 0.32s ease, top 0.32s ease, width 0.32s ease, height 0.32s ease,
    filter 0.08s ease, outline-color 0.08s ease;
  outline: 2px solid transparent; outline-offset: -2px; }
.${ROOT_CLASS}-tile:hover { filter: brightness(0.94); }
.${ROOT_CLASS}-tile.is-selected { outline-color: #d6336c; }
.${ROOT_CLASS}-tile.is-leaf { border-width: 2px; border-color: rgba(26, 29, 33, 0.5); }
.${ROOT_CLASS}-tile.is-ghost { background: none; pointer-events: none;
  border: 1px solid rgba(26, 29, 33, 0.25); }
`;

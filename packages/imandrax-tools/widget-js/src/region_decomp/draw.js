// Pure, host-agnostic icicle view for region decomposition.
//
// `draw(el, regionGroups, opts)` takes a DOM node and the forest of region groups,
// and mutates the DOM, and returns nothing. 

import { hierarchy, partition } from 'd3-hierarchy';
import { scaleOrdinal } from 'd3-scale';
import { schemeTableau10 } from 'd3-scale-chromatic';
import { select } from 'd3-selection';

const ROOT_CLASS = 'imdx-rd';

const DEFAULTS = {
  width: 720, // chart pane width in px (falls back to el.clientWidth)
  rowHeight: 30, // px per tree level
  detailWidth: 300, // detail pane width in px
  height: 520, // overall widget height in px
};

// Styles
// ====================

// Scoped under `.imdx-rd` and injected once per widget root, so the view is
// self-contained in either host without a separate stylesheet.
const STYLE = `
.${ROOT_CLASS} { display: flex; height: var(--imdx-h); border: 1px solid #d8dde2;
  border-radius: 6px; overflow: hidden; font-family: ui-sans-serif, system-ui, sans-serif;
  font-size: 12px; color: #1a1d21; background: #fff; box-sizing: border-box; }
.${ROOT_CLASS} *, .${ROOT_CLASS} *::before, .${ROOT_CLASS} *::after { box-sizing: border-box; }
.${ROOT_CLASS}-chart { flex: 1 1 auto; min-width: 0; overflow: auto; position: relative; }
.${ROOT_CLASS}-canvas { position: relative; }
.${ROOT_CLASS}-cell { position: absolute; overflow: hidden; cursor: pointer;
  border-radius: 3px; padding: 3px 6px; line-height: 1.25; white-space: nowrap;
  transition: filter 0.08s ease, outline-color 0.08s ease;
  outline: 1.5px solid transparent; outline-offset: -1.5px; }
.${ROOT_CLASS}-cell:hover { filter: brightness(0.94); }
.${ROOT_CLASS}-cell.is-onpath { outline-color: #1a1d21; }
.${ROOT_CLASS}-cell.is-selected { outline-color: #d6336c; outline-width: 2px; }
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

// Color
// ====================

// Each top-level branch gets a stable Tableau hue; cells grow paler with depth,
// so a subtree reads as one color family and dark labels stay legible.
function makeColorFn(rootNode) {
  const branchColor = scaleOrdinal(schemeTableau10);
  const maxDepth = Math.max(1, rootNode.height);
  return (node) => {
    if (node.depth === 0) return '#eceff1';
    const top = node.ancestors().find((a) => a.depth === 1);
    const base = branchColor(String(top.data.label_path));
    const t = 0.12 + 0.46 * ((node.depth - 1) / maxDepth);
    return mixWhite(base, t);
  };
}

// Mix a `#rrggbb` color toward white. t=0 → base, t=1 → white.
function mixWhite(hex, t) {
  const r = parseInt(hex.slice(1, 3), 16);
  const g = parseInt(hex.slice(3, 5), 16);
  const b = parseInt(hex.slice(5, 7), 16);
  const m = (v) => Math.round(v + (255 - v) * t);
  return `rgb(${m(r)}, ${m(g)}, ${m(b)})`;
}

// Render
// ====================

export function draw(el, regionGroups, opts = {}) {
  const cfg = { ...DEFAULTS, ...opts };
  const width = cfg.width || el.clientWidth || DEFAULTS.width;

  // d3.hierarchy needs a single root, so we wrap the region-group forest in a
  // synthetic "all regions" node. This is a view concern, not part of the data
  // contract — the widget is handed the bare forest.
  const groups = regionGroups || [];
  const syntheticRoot = {
    label_path: 'root',
    introduced_constraint: 'all regions',
    constraints: [],
    weight: groups.reduce((s, g) => s + (g.weight || 0), 0),
    n_children_regions: groups.length,
    children: groups,
  };

  // Layout: x is the weight axis (leaf count), y is tree depth. We take x0/x1
  // from d3.partition and impose a fixed row height per depth so deep trees stay
  // readable and the chart scrolls instead of squashing rows.
  const root = hierarchy(syntheticRoot)
    .sum((d) => (d.children && d.children.length ? 0 : d.weight || 1))
    .sort((a, b) => b.value - a.value || (a.data.label_path < b.data.label_path ? -1 : 1));
  partition().size([width, 1]).padding(0)(root);

  // Backfill aggregate stats on the synthetic root for its detail panel.
  syntheticRoot.n_leaf_regions = root.leaves().length;
  syntheticRoot.n_descendant_regions = root.descendants().length - 1;

  const nodes = root.descendants();
  const canvasHeight = (root.height + 1) * cfg.rowHeight;
  const color = makeColorFn(root);

  el.innerHTML = '';
  el.classList.add(ROOT_CLASS);
  el.style.setProperty('--imdx-h', `${cfg.height}px`);
  el.style.setProperty('--imdx-dw', `${cfg.detailWidth}px`);

  const style = document.createElement('style');
  style.textContent = STYLE;
  el.appendChild(style);

  const chart = document.createElement('div');
  chart.className = `${ROOT_CLASS}-chart`;
  const canvas = document.createElement('div');
  canvas.className = `${ROOT_CLASS}-canvas`;
  canvas.style.height = `${canvasHeight}px`;
  canvas.style.width = `${width}px`;
  chart.appendChild(canvas);

  const divider = document.createElement('div');
  divider.className = `${ROOT_CLASS}-divider`;

  const detail = document.createElement('div');
  detail.className = `${ROOT_CLASS}-detail`;

  el.appendChild(chart);
  el.appendChild(divider);
  el.appendChild(detail);

  let selected = null;

  const cells = select(canvas)
    .selectAll('div.cell')
    .data(nodes)
    .join('div')
    .attr('class', `${ROOT_CLASS}-cell`)
    .style('left', (d) => `${round2(d.x0)}px`)
    .style('top', (d) => `${d.depth * cfg.rowHeight}px`)
    .style('width', (d) => `${round2(Math.max(0, d.x1 - d.x0))}px`)
    .style('height', `${cfg.rowHeight}px`)
    .style('background', (d) => color(d))
    .attr('title', (d) => cellTooltip(d.data));

  cells.html((d) => cellLabel(d.data));

  cells
    .on('mouseenter', function (_event, d) {
      const onPath = new Set(d.ancestors());
      cells.classed('is-onpath', (n) => onPath.has(n));
    })
    .on('mouseleave', () => cells.classed('is-onpath', false))
    .on('click', function (_event, d) {
      selected = d;
      cells.classed('is-selected', (n) => n === selected);
      detail.innerHTML = detailHtml(d.data);
    });

  detail.innerHTML = `<p class="${ROOT_CLASS}-placeholder">Click a region to see its constraints and example.</p>`;
}

// Cell rendering helpers
// --------------------

function cellLabel(d) {
  const lp = esc(String(d.label_path ?? ''));
  const cst = esc(d.introduced_constraint ?? '');
  return `<span class="${ROOT_CLASS}-lp">${lp}</span> <span class="${ROOT_CLASS}-cst">${cst}</span>`;
}

function cellTooltip(d) {
  const lp = `[${d.label_path}]`;
  const cst = d.introduced_constraint ? ` ${d.introduced_constraint}` : '';
  return `${lp}${cst}`;
}

// Detail panel rendering
// --------------------

function detailHtml(d) {
  const parts = [];
  parts.push(`<h3>[${esc(String(d.label_path))}]</h3>`);
  if (d.introduced_constraint) {
    parts.push(`<p class="sub"><code>${esc(d.introduced_constraint)}</code></p>`);
  }

  parts.push(`<div class="stats">${statsHtml(d)}</div>`);

  const constraints = d.constraints || [];
  if (constraints.length) {
    parts.push(`<div class="k">Constraint path</div>`);
    const lis = constraints
      .map((c) => {
        const cur = c === d.introduced_constraint ? ' class="cur"' : '';
        return `<li${cur}><code>${esc(c)}</code></li>`;
      })
      .join('');
    parts.push(`<ol>${lis}</ol>`);
  }

  if (d.invariant != null) {
    parts.push(`<div class="k">Invariant</div><pre>${esc(d.invariant)}</pre>`);
  }
  if (d.example_input != null) {
    parts.push(`<div class="k">Example input</div><pre>${esc(fmtModel(d.example_input))}</pre>`);
  }
  if (d.example_output != null) {
    parts.push(`<div class="k">Example output</div><pre>${esc(String(d.example_output))}</pre>`);
  }
  return parts.join('');
}

function statsHtml(d) {
  const stat = (label, value) =>
    value == null ? '' : `<span>${label}: <b>${esc(String(value))}</b></span>`;
  const isLeaf = !(d.n_children_regions > 0);
  return [
    stat('leaf regions', d.n_leaf_regions),
    stat('direct children', d.n_children_regions),
    stat('descendants', d.n_descendant_regions),
    isLeaf ? '<span><b>concrete region</b></span>' : '',
  ]
    .filter(Boolean)
    .join('');
}

// `example_input` is a {var: value-string} map; render it as aligned lines.
function fmtModel(model) {
  if (typeof model !== 'object' || model === null) return String(model);
  const entries = Object.entries(model);
  if (!entries.length) return '(no inputs)';
  return entries.map(([k, v]) => `${k} = ${v}`).join('\n');
}

function round2(x) {
  return Math.round(x * 100) / 100;
}

// HTML-escape a value for safe interpolation into innerHTML.
function esc(s) {
  if (s == null) return '';
  return String(s)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;');
}

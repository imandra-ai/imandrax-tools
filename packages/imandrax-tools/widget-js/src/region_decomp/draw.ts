// Pure, host-agnostic icicle view for region decomposition.
//
// `draw(el, input, opts)` takes a DOM node and the region-group forest (or the
// whole decomposition result), mutates the DOM, and returns nothing. Every
// aggregate is read off the d3 hierarchy, so the widget consumes the raw forest
// as-is — no preprocessing pass.

import {
  hierarchy,
  partition,
  type HierarchyRectangularNode,
} from "d3-hierarchy";
import { scaleOrdinal } from "d3-scale";
import { schemeTableau10 } from "d3-scale-chromatic";
import { select } from "d3-selection";

import { ROOT_CLASS, STYLE } from "./style";
import type { DrawInput, DrawOptions, Region, RegionGroup } from "./types";

type Node = HierarchyRectangularNode<RegionGroup>;

const DEFAULTS = {
  width: 720, // chart pane width in px (falls back to el.clientWidth)
  rowHeight: 30, // px per tree level
  detailWidth: 300, // detail pane width in px
  height: 520, // overall widget height in px
};

// Normalize the caller's input to a bare region-group forest. Accepts the whole
// EnrichedDecomposeRes, just its `region_groups`, or nothing.
function normalizeGroups(input: DrawInput): RegionGroup[] {
  if (Array.isArray(input)) return input;
  if (input && Array.isArray(input.region_groups)) return input.region_groups;
  return [];
}

// Node-derived view values
// --------------------

// d3.hierarchy needs a single root, so the forest is wrapped in a synthetic
// "all regions" group. That root is the only node at depth 0, which is how the
// helpers below tell it apart from a real group.
function isRoot(node: Node): boolean {
  return node.depth === 0;
}

function labelPath(node: Node): string {
  return isRoot(node) ? "root" : node.data.label_path.map(String).join(".");
}

function introducedConstraint(node: Node): string {
  if (isRoot(node)) return "all regions";
  const cs = node.data.constraints;
  return cs.length ? cs[cs.length - 1] : "";
}

// Color
// ====================

// Each top-level branch gets a stable Tableau hue; cells grow paler with depth,
// so a subtree reads as one color family and dark labels stay legible.
function makeColorFn(rootNode: Node): (node: Node) => string {
  const branchColor = scaleOrdinal(schemeTableau10);
  const maxDepth = Math.max(1, rootNode.height);
  return (node) => {
    if (node.depth === 0) return "#eceff1";
    const top = node.ancestors().find((a) => a.depth === 1)!;
    const base = branchColor(labelPath(top));
    const t = 0.12 + 0.46 * ((node.depth - 1) / maxDepth);
    return mixWhite(base, t);
  };
}

// Mix a `#rrggbb` color toward white. t=0 → base, t=1 → white.
function mixWhite(hex: string, t: number): string {
  const r = parseInt(hex.slice(1, 3), 16);
  const g = parseInt(hex.slice(3, 5), 16);
  const b = parseInt(hex.slice(5, 7), 16);
  const m = (v: number) => Math.round(v + (255 - v) * t);
  return `rgb(${m(r)}, ${m(g)}, ${m(b)})`;
}

// Render
// ====================

export function draw(
  el: HTMLElement,
  input: DrawInput,
  opts: DrawOptions = {},
): void {
  const cfg = { ...DEFAULTS, ...opts };
  const width = cfg.width || el.clientWidth || DEFAULTS.width;

  // The synthetic root carries no constraints or region; depth 0 marks it.
  const groups = normalizeGroups(input);
  const syntheticRoot: RegionGroup = {
    label_path: [],
    constraints: [],
    weight: 0,
    region: null,
    children: groups,
  };

  // Layout: x is the weight axis (leaf count), y is tree depth. We take x0/x1
  // from d3.partition and impose a fixed row height per depth so deep trees stay
  // readable and the chart scrolls instead of squashing rows.
  const root = hierarchy<RegionGroup>(syntheticRoot)
    .sum((d) => (d.children && d.children.length ? 0 : d.weight || 1))
    .sort(
      (a, b) =>
        b.value! - a.value! ||
        (labelPath(a as Node) < labelPath(b as Node) ? -1 : 1),
    );
  partition<RegionGroup>().size([width, 1]).padding(0)(root);

  const nodes = (root as Node).descendants();
  const canvasHeight = (root.height + 1) * cfg.rowHeight;
  const color = makeColorFn(root as Node);

  el.innerHTML = "";
  el.classList.add(ROOT_CLASS);
  el.style.setProperty("--imdx-h", `${cfg.height}px`);
  el.style.setProperty("--imdx-dw", `${cfg.detailWidth}px`);

  const style = document.createElement("style");
  style.textContent = STYLE;
  el.appendChild(style);

  const chart = document.createElement("div");
  chart.className = `${ROOT_CLASS}-chart`;
  const canvas = document.createElement("div");
  canvas.className = `${ROOT_CLASS}-canvas`;
  canvas.style.height = `${canvasHeight}px`;
  canvas.style.width = `${width}px`;
  chart.appendChild(canvas);

  const divider = document.createElement("div");
  divider.className = `${ROOT_CLASS}-divider`;

  const detail = document.createElement("div");
  detail.className = `${ROOT_CLASS}-detail`;

  el.appendChild(chart);
  el.appendChild(divider);
  el.appendChild(detail);

  let selected: Node | null = null;

  const cells = select(canvas)
    .selectAll<HTMLDivElement, Node>("div.cell")
    .data(nodes)
    .join("div")
    .attr("class", `${ROOT_CLASS}-cell`)
    .style("left", (d) => `${round2(d.x0)}px`)
    .style("top", (d) => `${d.depth * cfg.rowHeight}px`)
    .style("width", (d) => `${round2(Math.max(0, d.x1 - d.x0))}px`)
    .style("height", `${cfg.rowHeight}px`)
    .style("background", (d) => color(d))
    .attr("title", (d) => cellTooltip(d));

  cells.html((d) => cellLabel(d));

  cells
    .on("mouseenter", function (_event, d) {
      const onPath = new Set(d.ancestors());
      cells.classed("is-onpath", (n) => onPath.has(n));
    })
    .on("mouseleave", () => cells.classed("is-onpath", false))
    .on("click", function (_event, d) {
      selected = d;
      cells.classed("is-selected", (n) => n === selected);
      detail.innerHTML = detailHtml(d);
    });

  detail.innerHTML = `<p class="${ROOT_CLASS}-placeholder">Click a region to see its constraints and example.</p>`;
}

// Cell rendering helpers
// --------------------

function cellLabel(node: Node): string {
  const lp = esc(labelPath(node));
  const cst = esc(introducedConstraint(node));
  return `<span class="${ROOT_CLASS}-lp">${lp}</span> <span class="${ROOT_CLASS}-cst">${cst}</span>`;
}

function cellTooltip(node: Node): string {
  const cst = introducedConstraint(node);
  return `[${labelPath(node)}]${cst ? ` ${cst}` : ""}`;
}

// Detail panel rendering
// --------------------

function detailHtml(node: Node): string {
  const introduced = introducedConstraint(node);
  const parts: string[] = [];
  parts.push(`<h3>[${esc(labelPath(node))}]</h3>`);
  if (introduced) {
    parts.push(`<p class="sub"><code>${esc(introduced)}</code></p>`);
  }

  parts.push(`<div class="stats">${statsHtml(node)}</div>`);

  const constraints = node.data.constraints || [];
  if (constraints.length) {
    parts.push(`<div class="k">Constraint path</div>`);
    const lis = constraints
      .map((c) => {
        const cur = c === introduced ? ' class="cur"' : "";
        return `<li${cur}><code>${esc(c)}</code></li>`;
      })
      .join("");
    parts.push(`<ol>${lis}</ol>`);
  }

  const region = node.data.region;
  if (region) {
    parts.push(
      `<div class="k">Invariant</div><pre>${esc(region.invariant_str)}</pre>`,
    );
    parts.push(
      `<div class="k">Example input</div><pre>${esc(fmtModel(region.model_str))}</pre>`,
    );
    parts.push(
      `<div class="k">Example output</div><pre>${esc(region.model_eval_str)}</pre>`,
    );
  }
  return parts.join("");
}

function statsHtml(node: Node): string {
  const stat = (label: string, value: number) =>
    `<span>${label}: <b>${value}</b></span>`;
  const nChildren = node.children ? node.children.length : 0;
  return [
    stat("leaf regions", node.leaves().length),
    stat("direct children", nChildren),
    stat("descendants", node.descendants().length - 1),
  ]
    .filter(Boolean)
    .join("");
}

// `model_str` is a {var: value-string} map; render it as aligned lines.
function fmtModel(model: Region["model_str"]): string {
  if (typeof model !== "object" || model === null) return String(model);
  const entries = Object.entries(model);
  if (!entries.length) return "(no inputs)";
  return entries.map(([k, v]) => `${k} = ${v}`).join("\n");
}

function round2(x: number): number {
  return Math.round(x * 100) / 100;
}

// HTML-escape a value for safe interpolation into innerHTML.
function esc(s: unknown): string {
  if (s == null) return "";
  return String(s)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

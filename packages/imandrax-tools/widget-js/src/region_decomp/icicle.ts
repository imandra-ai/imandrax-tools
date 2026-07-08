// Icicle view: region depth runs top-to-bottom, leaf weight sets cell width.
// A secondary view to the treemap; kept lean on the shared node/color/detail
// modules. `drawIcicle(el, input, opts)` mutates the DOM and returns nothing.

import { partition, type HierarchyRectangularNode } from 'd3-hierarchy';
import { select } from 'd3-selection';

import { makeColorFn } from './color';
import { detailHtml, esc, PLACEHOLDER_HTML } from './detail';
import { buildHierarchy, introducedConstraint, labelPath } from './nodes';
import { ICICLE_STYLE, ROOT_CLASS, SHARED_STYLE } from './style';
import type { DrawInput, DrawOptions, RegionGroup } from './types';

type Rect = HierarchyRectangularNode<RegionGroup>;

const DEFAULTS = {
  width: 720, // chart pane width in px (falls back to el.clientWidth)
  rowHeight: 30, // px per tree level
  detailWidth: 300, // detail pane width in px
  height: 520, // overall widget height in px
};

export function drawIcicle(el: HTMLElement, input: DrawInput, opts: DrawOptions = {}): void {
  const cfg = { ...DEFAULTS, ...opts };
  const width = cfg.width || el.clientWidth || DEFAULTS.width;

  const root = buildHierarchy(input);
  partition<RegionGroup>().size([width, 1]).padding(0)(root);
  const nodes = (root as Rect).descendants();
  const canvasHeight = (root.height + 1) * cfg.rowHeight;
  const color = makeColorFn(root);

  el.innerHTML = '';
  el.classList.add(ROOT_CLASS);
  el.style.setProperty('--imdx-h', `${cfg.height}px`);
  el.style.setProperty('--imdx-dw', `${cfg.detailWidth}px`);

  const style = document.createElement('style');
  style.textContent = SHARED_STYLE + ICICLE_STYLE;
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

  el.append(chart, divider, detail);

  let selected: Rect | null = null;

  const cells = select(canvas)
    .selectAll<HTMLDivElement, Rect>('div.cell')
    .data(nodes)
    .join('div')
    .attr('class', `${ROOT_CLASS}-cell`)
    .style('left', (d) => `${round2(d.x0)}px`)
    .style('top', (d) => `${d.depth * cfg.rowHeight}px`)
    .style('width', (d) => `${round2(Math.max(0, d.x1 - d.x0))}px`)
    .style('height', `${cfg.rowHeight}px`)
    .style('background', (d) => color(d))
    .attr('title', (d) => cellTooltip(d))
    .html((d) => cellLabel(d));

  cells
    .on('mouseenter', function (_event, d) {
      const onPath = new Set(d.ancestors());
      cells.classed('is-onpath', (n) => onPath.has(n));
    })
    .on('mouseleave', () => cells.classed('is-onpath', false))
    .on('click', function (_event, d) {
      selected = d;
      cells.classed('is-selected', (n) => n === selected);
      detail.innerHTML = detailHtml(d);
    });

  detail.innerHTML = PLACEHOLDER_HTML;
}

function cellLabel(node: Rect): string {
  return `<span class="${ROOT_CLASS}-lp">${esc(labelPath(node))}</span> <span class="${ROOT_CLASS}-cst">${esc(introducedConstraint(node))}</span>`;
}

function cellTooltip(node: Rect): string {
  const cst = introducedConstraint(node);
  return `[${labelPath(node)}]${cst ? ` ${cst}` : ''}`;
}

function round2(x: number): number {
  return Math.round(x * 100) / 100;
}

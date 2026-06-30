// Treemap view: nested rectangles sized by leaf weight. The whole tree is laid
// out once; zooming projects the selected node's sub-rectangle to fill the
// viewport (an idea borrowed from wigglystuff's treemap, MIT-licensed), so a
// zoom is a re-projection rather than a relayout and the tiles can animate their
// geometry with a plain CSS transition.
//
//   single click → show that region's detail panel
//   double click → zoom into that region (if it has children)
//   breadcrumb   → zoom back out to any ancestor
//
// `drawTreemap(el, input, opts)` builds the DOM, wires interaction, and returns
// nothing.

import { treemap, type HierarchyRectangularNode } from 'd3-hierarchy';
import { select } from 'd3-selection';

import { detailHtml, esc, PLACEHOLDER_HTML } from './detail';
import { buildHierarchy, introducedConstraint, isRoot, labelPath } from './nodes';
import { ROOT_CLASS, SHARED_STYLE, TREEMAP_STYLE } from './style';
import type { DrawInput, RegionGroup, TreemapOptions } from './types';

type Rect = HierarchyRectangularNode<RegionGroup>;
interface Extent {
  x0: number;
  y0: number;
  x1: number;
  y1: number;
}
interface Box {
  left: number;
  top: number;
  width: number;
  height: number;
}
// A tile to draw. `ghost` tiles sit one level past the depth limit and are drawn
// as dimmed, non-interactive outlines — a preview of what a zoom would reveal.
interface Item {
  node: Rect;
  ghost: boolean;
  box: Box;
}

const DEFAULTS = {
  width: 720, // tiles pane width in px (falls back to measured width)
  height: 520, // overall widget height in px
  detailWidth: 300, // detail pane width in px
  maxDepth: 3, // levels of descendants shown below the zoom root
};

const TOPBAR_H = 31; // breadcrumb bar height, for the jsdom size fallback
const MIN_TILE_PX = 2; // tiles smaller than this on a side are culled
const LABEL_MIN_W = 40; // tile must be this wide to show a label
const LABEL_MIN_H = 15; // tile must be this tall to show a label
const TILE_GAP = 2; // gap between sibling tiles, in layout px
const PARENT_HEADER = 16; // header strip a parent reserves above its children,
// so an interior node stays visible and clickable instead of being fully
// covered by its descendants.

// Pure layout helpers
// --------------------

// The box the node's children occupy — its rect minus the padding the layout
// reserves (the header strip on top, the gap on the other sides). Projecting
// against this makes the zoom root's children fill the pane with no dead strip.
function contentExtent(node: Rect): Extent {
  const top = node.children && node.children.length ? PARENT_HEADER : TILE_GAP;
  return {
    x0: node.x0 + TILE_GAP,
    y0: node.y0 + top,
    x1: node.x1 - TILE_GAP,
    y1: node.y1 - TILE_GAP,
  };
}

// Map a node's laid-out rectangle into the viewport, given the extent currently
// filling it. When `ext` is the selected node, the selection fills the pane.
function projectedBox(node: Rect, ext: Extent, vw: number, vh: number): Box {
  const dx = ext.x1 - ext.x0 || 1;
  const dy = ext.y1 - ext.y0 || 1;
  return {
    left: ((node.x0 - ext.x0) / dx) * vw,
    top: ((node.y0 - ext.y0) / dy) * vh,
    width: ((node.x1 - node.x0) / dx) * vw,
    height: ((node.y1 - node.y0) / dy) * vh,
  };
}

// Place a ghost so it fills its parent's whole tile rather than just the
// parent's content box. Without this a ghost sits below the parent's header
// strip, making the title area read as a separate partition. We rescale the
// ghost's fractional position within the parent's content box onto the parent's
// full on-screen box, so the preview covers the entire tile.
function ghostBoxInParent(node: Rect, parent: Rect, parentBox: Box): Box {
  const pc = contentExtent(parent);
  const cdx = pc.x1 - pc.x0 || 1;
  const cdy = pc.y1 - pc.y0 || 1;
  const fx0 = (node.x0 - pc.x0) / cdx;
  const fy0 = (node.y0 - pc.y0) / cdy;
  const fx1 = (node.x1 - pc.x0) / cdx;
  const fy1 = (node.y1 - pc.y0) / cdy;
  return {
    left: parentBox.left + fx0 * parentBox.width,
    top: parentBox.top + fy0 * parentBox.height,
    width: (fx1 - fx0) * parentBox.width,
    height: (fy1 - fy0) * parentBox.height,
  };
}

// Descendants of `selected` to draw: full tiles down to `maxDepth`, plus one
// further level of ghost outlines (the zoom preview).
function visibleDescendants(selected: Rect, maxDepth: number): { node: Rect; ghost: boolean }[] {
  const out: { node: Rect; ghost: boolean }[] = [];
  const walk = (node: Rect, rel: number) => {
    if (rel > maxDepth + 1) return;
    out.push({ node, ghost: rel > maxDepth });
    // Ghosts are leaves of the preview — don't descend past them.
    if (rel <= maxDepth && node.children) for (const c of node.children) walk(c as Rect, rel + 1);
  };
  if (selected.children) for (const c of selected.children) walk(c as Rect, 1);
  return out;
}

function zoomable(node: Rect): boolean {
  return !!node.children && node.children.length > 0;
}

// Tile classes: ghosts get the preview outline, true leaves (concrete regions)
// get a bold border, groupings keep the default light border.
function tileClass(item: Item): string {
  if (item.ghost) return `${ROOT_CLASS}-tile is-ghost`;
  return zoomable(item.node) ? `${ROOT_CLASS}-tile` : `${ROOT_CLASS}-tile is-leaf`;
}

// A short summary of a node for the breadcrumb meta slot.
function nodeSummary(node: Rect): string {
  const n = node.leaves().length;
  return `${n} leaf ${n === 1 ? 'region' : 'regions'}`;
}

// Render
// ====================

export function drawTreemap(el: HTMLElement, input: DrawInput, opts: TreemapOptions = {}): void {
  const cfg = { ...DEFAULTS, ...opts };

  const root = buildHierarchy(input);

  el.innerHTML = '';
  el.classList.add(ROOT_CLASS);
  el.style.setProperty('--imdx-h', `${cfg.height}px`);
  el.style.setProperty('--imdx-dw', `${cfg.detailWidth}px`);

  const style = document.createElement('style');
  style.textContent = SHARED_STYLE + TREEMAP_STYLE;

  const main = document.createElement('div');
  main.className = `${ROOT_CLASS}-main`;
  const topbar = document.createElement('div');
  topbar.className = `${ROOT_CLASS}-topbar`;
  const tiles = document.createElement('div');
  tiles.className = `${ROOT_CLASS}-tiles`;
  main.append(topbar, tiles);

  const divider = document.createElement('div');
  divider.className = `${ROOT_CLASS}-divider`;
  const detail = document.createElement('div');
  detail.className = `${ROOT_CLASS}-detail`;
  detail.innerHTML = PLACEHOLDER_HTML;

  el.append(style, main, divider, detail);

  // State: `selected` is the zoom root, `picked` is the node whose detail shows.
  let selected: Rect = root as Rect;
  let picked: Rect | null = null;
  let laidOutW = 0;
  let laidOutH = 0;

  function viewport(): { vw: number; vh: number } {
    return {
      vw: tiles.clientWidth || cfg.width,
      vh: tiles.clientHeight || cfg.height - TOPBAR_H,
    };
  }

  // Lay the whole tree out once per size; zoom only re-projects. paddingTop
  // reserves a clickable header strip on every interior node.
  function layout(vw: number, vh: number): void {
    if (vw === laidOutW && vh === laidOutH) return;
    treemap<RegionGroup>()
      .size([vw, vh])
      .paddingInner(TILE_GAP)
      .paddingOuter(TILE_GAP)
      .paddingTop((node) => (node.children && node.children.length ? PARENT_HEADER : 0))
      .round(false)(root);
    laidOutW = vw;
    laidOutH = vh;
  }

  function setSelected(node: Rect): void {
    selected = node;
    renderBreadcrumb();
    renderTiles();
  }

  function showDetail(node: Rect): void {
    picked = node;
    detail.innerHTML = detailHtml(node);
    syncSelectedClass();
  }

  function syncSelectedClass(): void {
    tiles
      .querySelectorAll(`.${ROOT_CLASS}-tile`)
      .forEach((t) => t.classList.remove('is-selected'));
    if (picked && pickedEl) pickedEl.classList.add('is-selected');
  }

  let pickedEl: HTMLElement | null = null;

  function renderBreadcrumb(): void {
    const trail = selected.ancestors().reverse() as Rect[];
    topbar.replaceChildren();
    trail.forEach((node, i) => {
      if (i > 0) {
        const sep = document.createElement('span');
        sep.className = `${ROOT_CLASS}-sep`;
        sep.textContent = '›';
        topbar.appendChild(sep);
      }
      const crumb = document.createElement('button');
      crumb.type = 'button';
      const isCurrent = node === selected;
      crumb.className = `${ROOT_CLASS}-crumb${isCurrent ? ' -current' : ''}`;
      crumb.textContent = isRoot(node) ? 'root' : `[${labelPath(node)}]`;
      if (!isCurrent) crumb.addEventListener('click', () => setSelected(node));
      topbar.appendChild(crumb);
    });
    const meta = document.createElement('span');
    meta.className = `${ROOT_CLASS}-sep`;
    meta.style.marginLeft = 'auto';
    meta.textContent = nodeSummary(selected);
    topbar.appendChild(meta);
  }

  function renderTiles(): void {
    const { vw, vh } = viewport();
    layout(vw, vh);
    const ext = contentExtent(selected);

    const items: Item[] = visibleDescendants(selected, cfg.maxDepth)
      .map(({ node, ghost }) => {
        // A ghost fills its parent's whole tile; a real tile uses its own box.
        const box = ghost
          ? ghostBoxInParent(node, node.parent as Rect, projectedBox(node.parent as Rect, ext, vw, vh))
          : projectedBox(node, ext, vw, vh);
        return { node, ghost, box };
      })
      .filter(({ box }) => box.width >= MIN_TILE_PX && box.height >= MIN_TILE_PX);

    select(tiles)
      .selectAll<HTMLDivElement, Item>(`div.${ROOT_CLASS}-tile`)
      .data(items, (d) => labelPath(d.node))
      .join('div')
      .attr('class', (d) => tileClass(d))
      .style('left', (d) => `${round2(d.box.left)}px`)
      .style('top', (d) => `${round2(d.box.top)}px`)
      .style('width', (d) => `${round2(d.box.width)}px`)
      .style('height', (d) => `${round2(d.box.height)}px`)
      .attr('title', (d) => (d.ghost ? null : tileTooltip(d.node)))
      .html((d) => (!d.ghost && tileLabel(d.box) ? cellLabel(d.node) : ''))
      .on('click', (_event, d) => {
        if (d.ghost) return;
        pickedEl = currentTarget(_event);
        showDetail(d.node);
      })
      .on('dblclick', (_event, d) => {
        if (!d.ghost && zoomable(d.node)) setSelected(d.node);
      })
      // Data is pre-order (parent before child), so matching DOM order keeps a
      // parent's header strip behind its children.
      .order();

    syncSelectedClass();
  }

  renderBreadcrumb();
  renderTiles();
}

// Tile rendering helpers
// --------------------

function tileLabel(box: Box): boolean {
  return box.width >= LABEL_MIN_W && box.height >= LABEL_MIN_H;
}

function cellLabel(node: Rect): string {
  return `<span class="${ROOT_CLASS}-lp">${esc(labelPath(node))}</span> <span class="${ROOT_CLASS}-cst">${esc(introducedConstraint(node))}</span>`;
}

function tileTooltip(node: Rect): string {
  const cst = introducedConstraint(node);
  return `[${labelPath(node)}]${cst ? ` ${cst}` : ''}`;
}

function currentTarget(event: Event): HTMLElement {
  return event.currentTarget as HTMLElement;
}

function round2(x: number): number {
  return Math.round(x * 100) / 100;
}

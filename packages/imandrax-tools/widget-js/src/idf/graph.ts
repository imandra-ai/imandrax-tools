// IDF graph view: a two-panel widget. Left is a top-down layered tree of region
// nodes (grouped into clickable step bands); right is a detail panel showing the
// clicked node's body or the clicked step's metadata.
//
//   click a node card  -> detail panel shows that region's body
//   click a step band  -> detail panel shows that step's request metadata
//   (selecting a node also highlights its edge-path back to the root)
//
// `drawGraph(el, view, opts)` builds the DOM, wires interaction, returns nothing.

import { nodeDetailHtml, PLACEHOLDER_HTML, stepDetailHtml, esc } from './detail';
import { CARD_H, CARD_W, BAND_PAD, layoutGraph, type Layout, type Placed } from './layout';
import { IDF_STYLE, ROOT_CLASS } from './style';
import type { DrawInput, GraphOptions, StepView } from './types';

const DEFAULTS = { height: 560, detailWidth: 320 };
const SVG_NS = 'http://www.w3.org/2000/svg';

function svgEl<K extends keyof SVGElementTagNameMap>(name: K): SVGElementTagNameMap[K] {
  return document.createElementNS(SVG_NS, name);
}

// A vertical S-curve from a parent's bottom to a child's top.
function edgePath(x1: number, y1: number, x2: number, y2: number): string {
  const my = (y1 + y2) / 2;
  return `M${x1},${y1} C${x1},${my} ${x2},${my} ${x2},${y2}`;
}

export function drawGraph(el: HTMLElement, input: DrawInput, opts: GraphOptions = {}): void {
  const cfg = { ...DEFAULTS, ...opts };

  el.innerHTML = '';
  el.classList.add(ROOT_CLASS);
  el.style.setProperty('--imdx-h', `${cfg.height}px`);
  el.style.setProperty('--imdx-dw', `${cfg.detailWidth}px`);

  const style = document.createElement('style');
  style.textContent = IDF_STYLE;

  const main = document.createElement('div');
  main.className = `${ROOT_CLASS}-main`;
  const divider = document.createElement('div');
  divider.className = `${ROOT_CLASS}-divider`;
  const detail = document.createElement('div');
  detail.className = `${ROOT_CLASS}-detail`;
  detail.innerHTML = PLACEHOLDER_HTML;

  el.append(style, main, divider, detail);

  const layout = input ? layoutGraph(input) : null;
  if (!layout) {
    const empty = document.createElement('div');
    empty.className = `${ROOT_CLASS}-placeholder`;
    empty.style.padding = '16px';
    empty.textContent = 'No decomposition to display.';
    main.appendChild(empty);
    return;
  }

  // step_idx -> StepView, for band clicks.
  const stepByIdx = new Map<number, StepView>(input!.steps.map((s) => [s.step_idx, s]));
  // childId -> parentId, to walk a selected node's path back to the root.
  const parentOf = new Map<number, number>();
  for (const e of input!.edges) parentOf.set(e.dst_id, e.src_id);

  const canvas = document.createElement('div');
  canvas.className = `${ROOT_CLASS}-canvas`;
  canvas.style.width = `${layout.width}px`;
  canvas.style.height = `${layout.height}px`;
  main.appendChild(canvas);

  const svg = svgEl('svg');
  svg.setAttribute('class', `${ROOT_CLASS}-svg`);
  svg.setAttribute('width', `${layout.width}`);
  svg.setAttribute('height', `${layout.height}`);
  canvas.appendChild(svg);

  // Selection state.
  let selectedNodeEl: HTMLElement | null = null;
  let selectedBandEl: SVGRectElement | null = null;
  const edgeEls = new Map<number, SVGPathElement>(); // keyed by childId

  function clearSelection(): void {
    selectedNodeEl?.classList.remove('is-selected');
    selectedNodeEl = null;
    selectedBandEl?.classList.remove('is-selected');
    selectedBandEl = null;
    edgeEls.forEach((p) => p.classList.remove('is-active'));
  }

  function selectNode(p: Placed, cardEl: HTMLElement): void {
    clearSelection();
    selectedNodeEl = cardEl;
    cardEl.classList.add('is-selected');
    // Light up the edge chain from this node up to the root.
    let cur = p.region.id;
    while (parentOf.has(cur)) {
      edgeEls.get(cur)?.classList.add('is-active');
      cur = parentOf.get(cur)!;
    }
    detail.innerHTML = nodeDetailHtml(p.region, p.introduced);
  }

  function selectStep(step: StepView, bandEl: SVGRectElement): void {
    clearSelection();
    selectedBandEl = bandEl;
    bandEl.classList.add('is-selected');
    detail.innerHTML = stepDetailHtml(step);
  }

  // Step bands (behind everything): one rounded row per depth, with a label.
  for (const row of layout.rows) {
    const isRoot = row.depth === 0;
    const y = row.top - BAND_PAD;
    const h = CARD_H + 2 * BAND_PAD;

    const band = svgEl('rect');
    band.setAttribute('class', `${ROOT_CLASS}-band${isRoot ? ' is-root' : ''}`);
    band.setAttribute('x', '8');
    band.setAttribute('y', `${y}`);
    band.setAttribute('width', `${layout.width - 16}`);
    band.setAttribute('height', `${h}`);
    band.setAttribute('rx', '8');
    svg.appendChild(band);

    const label = svgEl('text');
    label.setAttribute('class', `${ROOT_CLASS}-bandlabel`);
    label.setAttribute('x', '18');
    label.setAttribute('y', `${y + 15}`);
    label.setAttribute('font-size', '11');
    if (isRoot) {
      label.textContent = 'root';
    } else {
      const n = svgEl('tspan');
      n.setAttribute('class', 'step-n');
      n.textContent = `Step ${row.step_idx}`;
      const msg = svgEl('tspan');
      msg.setAttribute('class', 'step-msg');
      const step = stepByIdx.get(row.step_idx);
      msg.textContent = step ? `  ·  ${step.message}` : '';
      label.append(n, msg);
    }
    svg.appendChild(label);

    // Bands (and their labels) are clickable to show step details.
    if (!isRoot) {
      const step = stepByIdx.get(row.step_idx);
      if (step) {
        const onClick = () => selectStep(step, band);
        band.addEventListener('click', onClick);
        label.addEventListener('click', onClick);
      }
    }
  }

  // Edges over the bands.
  for (const e of layout.edges) {
    const path = svgEl('path');
    path.setAttribute('class', `${ROOT_CLASS}-edge`);
    path.setAttribute('d', edgePath(e.x1, e.y1, e.x2, e.y2));
    svg.appendChild(path);
    edgeEls.set(e.childId, path);
  }

  // Node cards over everything.
  for (const p of layout.placed) {
    const card = document.createElement('div');
    const kindCls = p.region.is_root ? ' is-root' : p.region.is_leaf ? ' is-leaf' : '';
    card.className = `${ROOT_CLASS}-node${kindCls}`;
    card.style.left = `${p.cx - CARD_W / 2}px`;
    card.style.top = `${p.top}px`;
    card.style.width = `${CARD_W}px`;
    card.style.height = `${CARD_H}px`;
    card.innerHTML = cardInner(p);
    if (!p.region.is_root) card.title = p.introduced.join('\n') || p.region.invariant || '';
    card.addEventListener('click', () => selectNode(p, card));
    canvas.appendChild(card);
  }
}

// A card's face: `#id` + a LEAF badge (leaves only) over the constraints it
// introduced this step. The root card is just its label.
function cardInner(p: Placed): string {
  if (p.region.is_root) {
    return `<div class="${ROOT_CLASS}-nhead"><span class="${ROOT_CLASS}-nid">root</span></div>`;
  }
  const badge = p.region.is_leaf ? `<span class="${ROOT_CLASS}-badge">leaf</span>` : '';
  const cst = p.introduced.length
    ? `<div class="${ROOT_CLASS}-ncst">${esc(p.introduced.join('  ∧  '))}</div>`
    : '';
  return `<div class="${ROOT_CLASS}-nhead"><span class="${ROOT_CLASS}-nid">#${p.region.id}</span>${badge}</div>${cst}`;
}

export type { Layout };

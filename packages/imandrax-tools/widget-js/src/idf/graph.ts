// IDF graph view: a two-panel widget. Left is a force-laid-out graph of region
// nodes (steps ordered top-to-bottom as soft strata) with pan + zoom; right is
// a detail panel showing the clicked node's body or the clicked step's metadata.
//
//   click a node card      -> detail panel shows that region's body
//   click a step axis label -> detail panel shows that step's request metadata
//   drag background / wheel  -> pan / zoom the graph
//   (selecting a node also highlights its edge-path back to the root)
//
// `drawGraph(el, view, opts)` builds the DOM, wires interaction, returns nothing.

import { nodeDetailHtml, PLACEHOLDER_HTML, stepDetailHtml, esc } from './detail';
import { CARD_H, CARD_W, layoutGraph, type Placed } from './layout';
import { IDF_STYLE, ROOT_CLASS } from './style';
import type { DrawInput, GraphOptions, StepView } from './types';

const DEFAULTS = { height: 560, detailWidth: 320 };
const AXIS_W = 96; // sticky left step-axis gutter width
const SVG_NS = 'http://www.w3.org/2000/svg';
const MIN_K = 0.12;
const MAX_K = 2.5;

function svgEl<K extends keyof SVGElementTagNameMap>(name: K): SVGElementTagNameMap[K] {
  return document.createElementNS(SVG_NS, name);
}

function edgePath(x1: number, y1: number, x2: number, y2: number): string {
  const my = (y1 + y2) / 2;
  return `M${x1},${y1} C${x1},${my} ${x2},${my} ${x2},${y2}`;
}

const clamp = (v: number, lo: number, hi: number) => Math.min(hi, Math.max(lo, v));

export function drawGraph(el: HTMLElement, input: DrawInput, opts: GraphOptions = {}): void {
  const cfg = { ...DEFAULTS, ...opts };

  el.innerHTML = '';
  el.classList.add(ROOT_CLASS);
  el.style.setProperty('--imdx-h', `${cfg.height}px`);
  el.style.setProperty('--imdx-dw', `${cfg.detailWidth}px`);
  el.style.setProperty('--imdx-axis', `${AXIS_W}px`);

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
  const lay = layout; // non-null alias (TS won't narrow the const inside closures)

  const stepByIdx = new Map<number, StepView>(input!.steps.map((s) => [s.step_idx, s]));
  const parentOf = new Map<number, number>();
  for (const e of input!.edges) parentOf.set(e.dst_id, e.src_id);

  // Pan/zoom viewport holding the transformed canvas.
  const viewport = document.createElement('div');
  viewport.className = `${ROOT_CLASS}-viewport`;
  const canvas = document.createElement('div');
  canvas.className = `${ROOT_CLASS}-canvas`;
  canvas.style.width = `${layout.width}px`;
  canvas.style.height = `${layout.height}px`;
  viewport.appendChild(canvas);
  main.appendChild(viewport);

  const svg = svgEl('svg');
  svg.setAttribute('class', `${ROOT_CLASS}-svg`);
  svg.setAttribute('width', `${layout.width}`);
  svg.setAttribute('height', `${layout.height}`);
  canvas.appendChild(svg);

  // Step hulls go in first so they paint beneath the edges and cards.
  const hullLayer = svgEl('g');
  svg.appendChild(hullLayer);

  // Sticky left step axis (overlays the viewport; content pans beneath it).
  const axis = document.createElement('div');
  axis.className = `${ROOT_CLASS}-axis`;
  main.appendChild(axis);

  // Reset-view button (top-right): re-fits the graph to the pane.
  const resetBtn = document.createElement('button');
  resetBtn.type = 'button';
  resetBtn.className = `${ROOT_CLASS}-reset`;
  resetBtn.textContent = 'Reset view';
  resetBtn.title = 'Reset zoom & pan';
  resetBtn.addEventListener('click', () => fit());
  main.appendChild(resetBtn);

  // Selection state.
  let selectedNodeEl: HTMLElement | null = null;
  let selectedStepEl: HTMLElement | null = null;
  const edgeEls = new Map<number, SVGPathElement>();
  const axisLabelByStep = new Map<number, HTMLElement>(); // populated when axis is built

  function clearSelection(): void {
    selectedNodeEl?.classList.remove('is-selected');
    selectedNodeEl = null;
    selectedStepEl?.classList.remove('is-selected');
    selectedStepEl = null;
    edgeEls.forEach((p) => p.classList.remove('is-active'));
  }

  function selectNode(p: Placed, cardEl: HTMLElement): void {
    clearSelection();
    selectedNodeEl = cardEl;
    cardEl.classList.add('is-selected');
    let cur = p.region.id;
    while (parentOf.has(cur)) {
      edgeEls.get(cur)?.classList.add('is-active');
      cur = parentOf.get(cur)!;
    }
    detail.innerHTML = nodeDetailHtml(p.region);
  }

  function selectStep(step: StepView, labelEl: HTMLElement): void {
    clearSelection();
    selectedStepEl = labelEl;
    labelEl.classList.add('is-selected');
    detail.innerHTML = stepDetailHtml(step);
  }

  // Live node positions (canvas coords), so dragging a node can reposition its
  // card and reroute its incident edges without re-running the layout.
  const posById = new Map<number, { x: number; y: number }>();
  for (const p of lay.placed) posById.set(p.region.id, { x: p.x, y: p.y });

  // Step hulls: a soft outline around all the nodes of one step. Members are
  // grouped by step (the root is left out -- a hull around one node is noise).
  const stepOf = new Map<number, number>(); // node id -> step_idx
  const membersByStep = new Map<number, number[]>();
  for (const p of lay.placed) {
    if (p.region.is_root) continue;
    const s = p.region.step_idx;
    stepOf.set(p.region.id, s);
    (membersByStep.get(s) ?? membersByStep.set(s, []).get(s)!).push(p.region.id);
  }
  const hullElByStep = new Map<number, SVGPathElement>();
  for (const s of [...membersByStep.keys()].sort((a, b) => a - b)) {
    const hull = svgEl('path');
    hull.setAttribute('class', `${ROOT_CLASS}-hull`); // one flat colour, styled in CSS
    // Clicking a hull selects its step. The handler lives on the viewport's
    // pointerup (a hull's own click is swallowed by the viewport pan capture),
    // so we just tag the element with its step here.
    if (stepByIdx.has(s)) hull.setAttribute('data-step', String(s));
    hullLayer.appendChild(hull);
    hullElByStep.set(s, hull);
  }

  function updateHull(step_idx: number): void {
    const el = hullElByStep.get(step_idx);
    const ids = membersByStep.get(step_idx);
    if (!el || !ids) return;
    const d = hullPath(ids.map((id) => posById.get(id)!));
    if (d) el.setAttribute('d', d);
  }
  for (const s of membersByStep.keys()) updateHull(s);

  function selectStepByIdx(step_idx: number): void {
    const step = stepByIdx.get(step_idx);
    const label = axisLabelByStep.get(step_idx);
    if (step && label) selectStep(step, label);
  }

  // Edges (under the cards).
  interface EdgeRec {
    el: SVGPathElement;
    parentId: number;
    childId: number;
  }
  const edgeRecs: EdgeRec[] = [];
  for (const e of lay.edges) {
    const path = svgEl('path');
    path.setAttribute('class', `${ROOT_CLASS}-edge`);
    path.setAttribute('d', edgePath(e.x1, e.y1, e.x2, e.y2));
    svg.appendChild(path);
    edgeEls.set(e.childId, path);
    edgeRecs.push({ el: path, parentId: e.parentId, childId: e.childId });
  }

  function rerouteEdges(id: number): void {
    for (const e of edgeRecs) {
      if (e.parentId !== id && e.childId !== id) continue;
      const s = posById.get(e.parentId)!;
      const t = posById.get(e.childId)!;
      e.el.setAttribute('d', edgePath(s.x, s.y, t.x, t.y));
    }
  }

  // Node cards. Each is click-to-select and drag-to-reposition; a drag under a
  // few px counts as a click.
  for (const p of lay.placed) {
    const card = document.createElement('div');
    const kindCls = p.region.is_root ? ' is-root' : p.region.is_leaf ? ' is-leaf' : '';
    card.className = `${ROOT_CLASS}-node${kindCls}`;
    card.style.left = `${p.x - CARD_W / 2}px`;
    card.style.top = `${p.y - CARD_H / 2}px`;
    card.style.width = `${CARD_W}px`;
    card.style.height = `${CARD_H}px`;
    card.innerHTML = cardInner(p);
    if (!p.region.is_root) card.title = p.region.invariant ?? '';
    attachNodeDrag(card, p);
    canvas.appendChild(card);
  }

  function attachNodeDrag(card: HTMLElement, p: Placed): void {
    const id = p.region.id;
    card.addEventListener('pointerdown', (ev: PointerEvent) => {
      ev.stopPropagation(); // don't let the viewport start a pan
      const startX = ev.clientX;
      const startY = ev.clientY;
      const origin = { ...posById.get(id)! };
      let moved = false;
      if (card.setPointerCapture && ev.pointerId != null) card.setPointerCapture(ev.pointerId);
      card.classList.add('is-dragging');

      const onMove = (e: PointerEvent) => {
        const dx = (e.clientX - startX) / k;
        const dy = (e.clientY - startY) / k;
        if (!moved && Math.hypot(e.clientX - startX, e.clientY - startY) > 3) moved = true;
        const nx = origin.x + dx;
        const ny = origin.y + dy;
        posById.set(id, { x: nx, y: ny });
        card.style.left = `${nx - CARD_W / 2}px`;
        card.style.top = `${ny - CARD_H / 2}px`;
        rerouteEdges(id);
        const st = stepOf.get(id);
        if (st != null) updateHull(st); // keep the step outline hugging its nodes
      };
      const onUp = () => {
        card.removeEventListener('pointermove', onMove);
        card.removeEventListener('pointerup', onUp);
        card.classList.remove('is-dragging');
        if (!moved) selectNode(p, card); // treat a no-move press as a click
      };
      card.addEventListener('pointermove', onMove);
      card.addEventListener('pointerup', onUp);
    });
  }

  // Step axis labels: one per stratum, positioned by projecting the stratum
  // center through the current transform so it tracks pan/zoom.
  interface AxisLabel {
    el: HTMLElement;
    cy: number;
  }
  const axisLabels: AxisLabel[] = layout.strata.map((s) => {
    const label = document.createElement('button');
    label.type = 'button';
    const isRoot = s.step_idx < 0;
    label.className = `${ROOT_CLASS}-axislabel${isRoot ? ' is-root' : ''}`;
    if (isRoot) {
      label.textContent = 'root';
    } else {
      const step = stepByIdx.get(s.step_idx);
      label.innerHTML = `<span class="step-n">Step ${s.step_idx}</span>${
        step ? `<span class="step-msg">${esc(step.message)}</span>` : ''
      }`;
      if (step) {
        axisLabelByStep.set(s.step_idx, label);
        label.addEventListener('click', (ev) => {
          ev.stopPropagation();
          selectStep(step, label);
        });
      }
    }
    axis.appendChild(label);
    return { el: label, cy: s.cy };
  });

  // Transform (pan/zoom)
  // --------------------
  let k = 1;
  let tx = 0;
  let ty = 0;

  function viewSize(): { vw: number; vh: number } {
    return {
      vw: viewport.clientWidth || cfg.detailWidth * 2, // jsdom fallback
      vh: viewport.clientHeight || cfg.height,
    };
  }

  function applyTransform(): void {
    canvas.style.transform = `translate(${tx}px, ${ty}px) scale(${k})`;
    const { vh } = viewSize();
    for (const a of axisLabels) {
      const y = ty + a.cy * k;
      a.el.style.top = `${clamp(y, 10, vh - 10)}px`;
      a.el.style.display = y < -20 || y > vh + 20 ? 'none' : '';
    }
  }

  function fit(): void {
    const { vw, vh } = viewSize();
    const margin = 24;
    k = clamp(
      Math.min((vw - margin * 2) / lay.width, (vh - margin * 2) / lay.height),
      MIN_K,
      1.1,
    );
    tx = (vw - lay.width * k) / 2;
    ty = margin - Math.min(...lay.strata.map((s) => s.cy)) * k;
    applyTransform();
  }

  function zoomAt(px: number, py: number, factor: number): void {
    const nk = clamp(k * factor, MIN_K, MAX_K);
    const r = nk / k;
    tx = px - (px - tx) * r;
    ty = py - (py - ty) * r;
    k = nk;
    applyTransform();
  }

  viewport.addEventListener(
    'wheel',
    (ev: WheelEvent) => {
      ev.preventDefault();
      const rect = viewport.getBoundingClientRect();
      zoomAt(ev.clientX - rect.left, ev.clientY - rect.top, ev.deltaY < 0 ? 1.1 : 1 / 1.1);
    },
    { passive: false },
  );

  // Drag-to-pan on the background. We also detect a no-move press on a step hull
  // here (its own click is swallowed by the pan pointer-capture) and select the
  // step. Node cards handle their own pointer events and never reach this.
  let panning = false;
  let lastX = 0;
  let lastY = 0;
  let downX = 0;
  let downY = 0;
  let downTarget: Element | null = null;
  viewport.addEventListener('pointerdown', (ev: PointerEvent) => {
    panning = true;
    lastX = downX = ev.clientX;
    lastY = downY = ev.clientY;
    downTarget = ev.target as Element;
    if (viewport.setPointerCapture && ev.pointerId != null) viewport.setPointerCapture(ev.pointerId);
    viewport.classList.add('is-panning');
  });
  viewport.addEventListener('pointermove', (ev: PointerEvent) => {
    if (!panning) return;
    tx += ev.clientX - lastX;
    ty += ev.clientY - lastY;
    lastX = ev.clientX;
    lastY = ev.clientY;
    applyTransform();
  });
  viewport.addEventListener('pointerup', (ev: PointerEvent) => {
    panning = false;
    viewport.classList.remove('is-panning');
    const isClick = Math.hypot(ev.clientX - downX, ev.clientY - downY) <= 3;
    const hull = downTarget?.closest?.(`.${ROOT_CLASS}-hull`);
    if (isClick && hull) {
      const s = hull.getAttribute('data-step');
      if (s != null) selectStepByIdx(Number(s));
    }
  });
  const endPan = () => {
    panning = false;
    viewport.classList.remove('is-panning');
  };
  viewport.addEventListener('pointercancel', endPan);

  fit();
}

// A card's face: `#id` (+ leaf badge) over the region's invariant.
function cardInner(p: Placed): string {
  if (p.region.is_root) {
    return `<div class="${ROOT_CLASS}-nhead"><span class="${ROOT_CLASS}-nid">root</span></div>`;
  }
  const badge = p.region.is_leaf ? `<span class="${ROOT_CLASS}-badge">leaf</span>` : '';
  const inv = p.region.invariant
    ? `<div class="${ROOT_CLASS}-ninv">${esc(p.region.invariant)}</div>`
    : '';
  return `<div class="${ROOT_CLASS}-nhead"><span class="${ROOT_CLASS}-nid">#${p.region.id}</span>${badge}</div>${inv}`;
}

// Step-hull helpers
// --------------------

interface Pt {
  x: number;
  y: number;
}

// A closed path around the card rectangles of a step's nodes. We hull the card
// corners (not just centers) so the outline encloses whole cards; the halo/
// rounding comes from a fat round-joined stroke in CSS.
function hullPath(centers: Pt[]): string | null {
  if (!centers.length) return null;
  const pts: Pt[] = [];
  for (const c of centers) {
    pts.push(
      { x: c.x - CARD_W / 2, y: c.y - CARD_H / 2 },
      { x: c.x + CARD_W / 2, y: c.y - CARD_H / 2 },
      { x: c.x + CARD_W / 2, y: c.y + CARD_H / 2 },
      { x: c.x - CARD_W / 2, y: c.y + CARD_H / 2 },
    );
  }
  const h = convexHull(pts);
  const fmt = (p: Pt) => `${p.x.toFixed(1)},${p.y.toFixed(1)}`;
  if (h.length === 1) return `M${fmt(h[0])}Z`;
  return `M${h.map(fmt).join('L')}Z`;
}

// Andrew's monotone-chain convex hull; returns the boundary in CCW order.
function convexHull(points: Pt[]): Pt[] {
  const pts = points.slice().sort((a, b) => a.x - b.x || a.y - b.y);
  if (pts.length <= 2) return pts;
  const cross = (o: Pt, a: Pt, b: Pt) => (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x);
  const lower: Pt[] = [];
  for (const p of pts) {
    while (lower.length >= 2 && cross(lower[lower.length - 2], lower[lower.length - 1], p) <= 0)
      lower.pop();
    lower.push(p);
  }
  const upper: Pt[] = [];
  for (let i = pts.length - 1; i >= 0; i--) {
    const p = pts[i];
    while (upper.length >= 2 && cross(upper[upper.length - 2], upper[upper.length - 1], p) <= 0)
      upper.pop();
    upper.push(p);
  }
  lower.pop();
  upper.pop();
  return lower.concat(upper);
}

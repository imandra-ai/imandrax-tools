// Force-directed layout for the IDF region graph. A tidy tree lays every step
// out as one horizontal row, which for wide decompositions (hundreds of leaves)
// spreads into a kilometre-wide strip. Instead we run a force simulation that
// still orders steps top-to-bottom (a per-step `forceY` stratum) but lets the
// nodes in a step pack in 2D, so the graph stays compact and pannable.
//
// The step "grouping" is therefore soft: nodes gravitate to their step's row
// but are free to move, and the graph pane sticks a step axis down its left
// edge instead of boxing each row in a rectangle.

import {
  forceCollide,
  forceLink,
  forceManyBody,
  forceSimulation,
  forceX,
  forceY,
  type SimulationLinkDatum,
  type SimulationNodeDatum,
} from 'd3-force';

import type { RegionNodeView, View } from './types';

// Node card geometry (compact so hundreds fit). Overflowing text is clipped;
// the full body lives in the detail panel.
export const CARD_W = 116;
export const CARD_H = 44;
const NODE_R = 64; // collision radius (keeps cards from overlapping)
const ROW_GAP = 150; // vertical distance between step strata
const PAD = 60; // canvas padding around the graph
const TICKS = 400; // simulation iterations before we freeze the layout

const ROOT_ID = 0;

function syntheticRoot(): RegionNodeView {
  return {
    id: ROOT_ID,
    step_idx: -1,
    is_root: true,
    is_leaf: false,
    raw_id: 'root',
    constraints: [],
    invariant: null,
    model: null,
    model_eval: null,
  };
}

interface SimNode extends SimulationNodeDatum {
  id: number;
  region: RegionNodeView;
  row: number; // 0 for root, step_idx + 1 otherwise
}

export interface Placed {
  region: RegionNodeView;
  x: number; // card center
  y: number;
}

export interface Edge {
  x1: number;
  y1: number;
  x2: number;
  y2: number;
  parentId: number;
  childId: number;
}

export interface Stratum {
  step_idx: number; // -1 for the root row
  cy: number; // stratum center y (canvas coords)
}

export interface Layout {
  placed: Placed[];
  edges: Edge[];
  strata: Stratum[];
  width: number;
  height: number;
}

export function layoutGraph(view: View): Layout | null {
  const byId = new Map<number, RegionNodeView>([[ROOT_ID, syntheticRoot()]]);
  for (const step of view.steps) {
    for (const r of step.regions) byId.set(r.id, r);
  }
  if (byId.size <= 1) return null;

  const nodes: SimNode[] = [...byId.values()].map((region) => ({
    id: region.id,
    region,
    row: region.is_root ? 0 : region.step_idx + 1,
  }));
  const links: SimulationLinkDatum<SimNode>[] = view.edges
    .filter((e) => byId.has(e.src_id) && byId.has(e.dst_id))
    .map((e) => ({ source: e.src_id, target: e.dst_id }));

  const sim = forceSimulation(nodes)
    .force(
      'link',
      forceLink<SimNode, SimulationLinkDatum<SimNode>>(links)
        .id((d) => d.id)
        .distance(70)
        .strength(0.5),
    )
    .force('charge', forceManyBody<SimNode>().strength(-260))
    .force('collide', forceCollide<SimNode>(NODE_R).strength(1))
    // Strata: pull each node toward its step's row. Strong enough to keep steps
    // ordered, loose enough that a busy step spreads sideways instead of piling.
    .force('y', forceY<SimNode>((d) => d.row * ROW_GAP).strength(0.9))
    // Gentle horizontal centering so the graph doesn't drift into a wide strip.
    .force('x', forceX<SimNode>(0).strength(0.05))
    .stop();

  for (let i = 0; i < TICKS; i++) sim.tick();

  // Normalize so the top-left of the content sits at (PAD, PAD).
  const xs = nodes.map((n) => n.x ?? 0);
  const ys = nodes.map((n) => n.y ?? 0);
  const minX = Math.min(...xs);
  const minY = Math.min(...ys);
  const ox = PAD - minX + CARD_W / 2;
  const oy = PAD - minY + CARD_H / 2;

  const pos = new Map<number, { x: number; y: number }>();
  for (const n of nodes) pos.set(n.id, { x: (n.x ?? 0) + ox, y: (n.y ?? 0) + oy });

  const placed: Placed[] = nodes.map((n) => ({ region: n.region, ...pos.get(n.id)! }));

  const edges: Edge[] = links.map((l) => {
    const s = pos.get((l.source as SimNode).id)!;
    const t = pos.get((l.target as SimNode).id)!;
    return { x1: s.x, y1: s.y, x2: t.x, y2: t.y, parentId: (l.source as SimNode).id, childId: (l.target as SimNode).id };
  });

  // Stratum center = normalized row y (matches the `forceY` target).
  const rowsSeen = new Map<number, number>(); // row -> step_idx
  for (const n of nodes) rowsSeen.set(n.row, n.region.step_idx);
  const strata: Stratum[] = [...rowsSeen.entries()]
    .sort((a, b) => a[0] - b[0])
    .map(([row, step_idx]) => ({ step_idx, cy: row * ROW_GAP + oy }));

  const width = Math.max(...placed.map((p) => p.x + CARD_W / 2)) + PAD;
  const height = Math.max(...placed.map((p) => p.y + CARD_H / 2)) + PAD;

  return { placed, edges, strata, width, height };
}

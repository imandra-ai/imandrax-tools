// Turn a `View` (a flat list of region nodes + parent->child edges) into a laid
// out, top-down layered tree. The graph is genuinely a tree: every region's
// parent is a node from the previous step (via `merge_src`), so tree depth ==
// step_idx + 1. d3's `tree()` therefore places each step on its own row for
// free, which is exactly the "group nodes by step" we want.

import { hierarchy, tree, type HierarchyPointNode } from 'd3-hierarchy';

import type { RegionNodeView, View } from './types';

// Card + spacing geometry. Cards are a fixed size so edges anchor at consistent
// points and every step-row lines up; overflowing card text is clipped (the
// full text lives in the detail panel / title tooltip).
export const CARD_W = 168;
export const CARD_H = 74;
const H_GAP = 26; // horizontal gap between sibling cards
const ROW_H = 128; // vertical distance between step rows
export const PAD = 44; // canvas padding around the tree
export const BAND_PAD = 15; // vertical padding of a step band around its cards

// The synthetic root that all step-0 nodes hang off. It carries `id == 0` and
// no constraints, matching what the Python side leaves implicit (root appears
// only as `src_id` in edges, never as a `StepView` region).
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

// A node in the tree we hand to d3: the region plus its resolved children.
interface GNode {
  region: RegionNodeView;
  children: GNode[];
}

// One laid-out node: the region, its card box (top-left corner + center), and
// the constraints it introduces over its parent (for the card face).
export interface Placed {
  region: RegionNodeView;
  cx: number; // card center x
  top: number; // card top y
  depth: number;
  introduced: string[];
}

export interface Edge {
  x1: number;
  y1: number;
  x2: number;
  y2: number;
  parentId: number;
  childId: number;
}

export interface Layout {
  placed: Placed[];
  edges: Edge[];
  // Per depth row (including root at depth 0): the shared card-top y and the
  // step_idx it maps to (-1 for the root row).
  rows: { depth: number; step_idx: number; top: number }[];
  width: number;
  height: number;
}

// Constraints a node introduces over its parent: everything in the child's
// constraint set that the parent didn't already have. (The child's set is a
// superset of the parent's, but not a contiguous prefix, so we diff as sets.)
function introducedOver(child: RegionNodeView, parent: RegionNodeView | null): string[] {
  if (!parent) return child.constraints;
  const seen = new Set(parent.constraints);
  return child.constraints.filter((c) => !seen.has(c));
}

// Build the GNode tree from the flat view. Returns null when there's nothing to
// draw (no steps / no regions).
function buildTree(view: View): GNode | null {
  const byId = new Map<number, RegionNodeView>([[ROOT_ID, syntheticRoot()]]);
  for (const step of view.steps) {
    for (const r of step.regions) byId.set(r.id, r);
  }
  if (byId.size <= 1) return null;

  const childIds = new Map<number, number[]>();
  for (const e of view.edges) {
    const list = childIds.get(e.src_id);
    if (list) list.push(e.dst_id);
    else childIds.set(e.src_id, [e.dst_id]);
  }

  const build = (id: number): GNode => {
    const region = byId.get(id)!;
    const kids = (childIds.get(id) ?? [])
      .filter((cid) => byId.has(cid))
      .map(build);
    // Stable order: by id, so layout is deterministic across renders.
    kids.sort((a, b) => a.region.id - b.region.id);
    return { region, children: kids };
  };
  return build(ROOT_ID);
}

export function layoutGraph(view: View): Layout | null {
  const rootGNode = buildTree(view);
  if (!rootGNode) return null;

  const root = hierarchy<GNode>(rootGNode, (d) => d.children);
  tree<GNode>().nodeSize([CARD_W + H_GAP, ROW_H])(root);

  const nodes = root.descendants() as HierarchyPointNode<GNode>[];
  const minX = Math.min(...nodes.map((n) => n.x));
  const cx = (n: HierarchyPointNode<GNode>) => n.x - minX + PAD + CARD_W / 2;
  const top = (n: HierarchyPointNode<GNode>) => n.y + PAD;

  const placed: Placed[] = nodes.map((n) => ({
    region: n.data.region,
    cx: cx(n),
    top: top(n),
    depth: n.depth,
    introduced: introducedOver(n.data.region, n.parent?.data.region ?? null),
  }));

  const edges = nodes
    .filter((n) => n.parent)
    .map((n) => ({
      x1: cx(n.parent!),
      y1: top(n.parent!) + CARD_H,
      x2: cx(n),
      y2: top(n),
      parentId: n.parent!.data.region.id,
      childId: n.data.region.id,
    }));

  // One row per depth; all nodes at a depth share `top`.
  const rowMap = new Map<number, { depth: number; step_idx: number; top: number }>();
  for (const n of nodes) {
    if (!rowMap.has(n.depth)) {
      rowMap.set(n.depth, { depth: n.depth, step_idx: n.data.region.step_idx, top: top(n) });
    }
  }
  const rows = [...rowMap.values()].sort((a, b) => a.depth - b.depth);

  const width = Math.max(...placed.map((p) => p.cx + CARD_W / 2)) + PAD;
  const height = Math.max(...placed.map((p) => p.top + CARD_H)) + PAD;

  return { placed, edges, rows, width, height };
}

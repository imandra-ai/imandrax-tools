// Shared node logic for the region-decomposition views. Both the icicle and the
// treemap build the same d3 hierarchy from the raw forest and read the same
// per-node values off it; only the layout differs.

import { hierarchy, type HierarchyNode } from 'd3-hierarchy';

import type { DrawInput, RegionGroup } from './types';

export type Node = HierarchyNode<RegionGroup>;

// Normalize the caller's input to a bare region-group forest. Accepts the whole
// decomposition result, just its `region_groups`, or nothing.
export function normalizeGroups(input: DrawInput): RegionGroup[] {
  if (Array.isArray(input)) return input;
  if (input && Array.isArray(input.region_groups)) return input.region_groups;
  return [];
}

// d3.hierarchy needs a single root, so the forest is wrapped in a synthetic
// "all regions" group. It carries no constraints or region; depth 0 marks it.
function syntheticRoot(groups: RegionGroup[]): RegionGroup {
  return { label_path: [], constraints: [], weight: 0, region_stat: null, children: groups };
}

// Build the d3 hierarchy shared by both views: leaf weight drives the size, and
// siblings sort by weight then label so the layout is stable.
export function buildHierarchy(input: DrawInput): Node {
  return hierarchy<RegionGroup>(syntheticRoot(normalizeGroups(input)))
    .sum((d) => (d.children && d.children.length ? 0 : d.weight || 1))
    .sort((a, b) => b.value! - a.value! || (labelPath(a) < labelPath(b) ? -1 : 1));
}

// Per-node view values
// --------------------

export function isRoot(node: Node): boolean {
  return node.depth === 0;
}

// Display label: "root" for the synthetic root, else the dotted label path.
export function labelPath(node: Node): string {
  return isRoot(node) ? 'root' : node.data.label_path.map(String).join('.');
}

// The constraint this node introduces over its parent (last in the path).
export function introducedConstraint(node: Node): string {
  if (isRoot(node)) return 'all regions';
  const cs = node.data.constraints;
  return cs.length ? cs[cs.length - 1] : '';
}

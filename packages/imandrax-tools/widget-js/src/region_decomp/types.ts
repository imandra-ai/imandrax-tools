// Shapes of the raw decomposition JSON the widget is handed. We read these
// fields directly and derive every aggregate (leaf counts, etc.) from the d3
// hierarchy at render time rather than expecting pre-computed stats.
//
// The node shape (`RegionGroupView` + `RegionNonGroupStat`) is the frontend
// contract, code-generated from the Python `RegionGroupView` pydantic model
// into ./generated/node.ts

import type { RegionGroupView, RegionNonGroupStat } from './generated/node';

export type { RegionGroupView, RegionNonGroupStat };

// One node in the region-group forest (`region_stat` non-null only on leaves).
export type RegionGroup = RegionGroupView;
// Display stats for a concrete leaf region.
export type RegionStat = RegionNonGroupStat;

// The decomposition result; we only read `region_groups`.
export interface EnrichedDecomposeRes {
  region_groups: RegionGroup[];
}

// What `draw` accepts: the whole result, just its forest, or nothing.
export type DrawInput = EnrichedDecomposeRes | RegionGroup[] | null | undefined;

// Icicle layout options.
export interface DrawOptions {
  width?: number; // chart pane width in px (falls back to el.clientWidth)
  rowHeight?: number; // px per tree level
  detailWidth?: number; // detail pane width in px
  height?: number; // overall widget height in px
}

// Treemap layout options.
export interface TreemapOptions {
  width?: number; // tiles pane width in px (falls back to measured width)
  height?: number; // overall widget height in px
  detailWidth?: number; // detail pane width in px
  maxDepth?: number; // levels of descendants shown below the zoom root
}

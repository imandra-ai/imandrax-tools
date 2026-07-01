// Shapes of the raw decomposition JSON the widget is handed. We read these
// fields directly and derive every aggregate (leaf counts, etc.) from the d3
// hierarchy at render time rather than expecting pre-computed stats.

// Display stats for a concrete region, produced by the Python side (`Region.stat`).
export interface RegionStat {
  invariant_str: string;
  // {var: value-string} for the example input, or a bare string.
  model_str: Record<string, string> | string;
  model_eval_str: string;
}

// One node in the region-group forest. `children` is `[]` on leaves.
export interface RegionGroup {
  constraints: string[];
  label_path: number[];
  weight: number;
  // Present (non-null) only on leaf groups: the concrete region's display stats.
  // Interior nodes carry null.
  region_stat: RegionStat | null;
  children: RegionGroup[];
}

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

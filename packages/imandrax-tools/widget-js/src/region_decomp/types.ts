// Shapes of the raw decomposition JSON the widget is handed. We read these
// fields directly and derive every aggregate (leaf counts, etc.) from the d3
// hierarchy at render time rather than expecting pre-computed stats.

// A concrete region, present only on leaf groups (null on interior nodes).
export interface Region {
  constraints_str: string[];
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
  region: Region | null;
  children: RegionGroup[];
}

// The decomposition result; we only read `region_groups`.
export interface EnrichedDecomposeRes {
  region_groups: RegionGroup[];
}

// What `draw` accepts: the whole result, just its forest, or nothing.
export type DrawInput = EnrichedDecomposeRes | RegionGroup[] | null | undefined;

export interface DrawOptions {
  width?: number; // chart pane width in px (falls back to el.clientWidth)
  rowHeight?: number; // px per tree level
  detailWidth?: number; // detail pane width in px
  height?: number; // overall widget height in px
}

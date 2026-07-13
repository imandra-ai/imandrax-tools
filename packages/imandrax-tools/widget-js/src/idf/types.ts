// Shapes of the IDF (iterative decomposition) JSON the widget is handed. The
// Python side (see `imandrax_tools.idf.viz_view.View`) builds these; we only
// render.
//
// The contract (`View` + `StepView` + `RegionNodeView` + `Edge`) is
// code-generated from the Python pydantic models into ../generated/types.ts
// (shared by all widgets).

import type { Edge, RegionNodeView, StepView, View } from '../generated/types';

export type { Edge, RegionNodeView, StepView, View };

// What `drawGraph` accepts: the whole view, or nothing (decomposition error).
export type DrawInput = View | null | undefined;

// Graph layout options.
export interface GraphOptions {
  height?: number; // overall widget height in px
  detailWidth?: number; // detail pane width in px
}

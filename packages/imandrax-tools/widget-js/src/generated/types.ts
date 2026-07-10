/* tslint:disable */
/* eslint-disable */
/**
/* This file was automatically generated from pydantic models by running pydantic2ts.
/* Do not modify it by hand - just update the pydantic models and then re-run the script
*/

/**
 * The IDF widget entry model: the layered region graph the JS renders.
 */
export interface View {
  steps: StepView[];
  edges: Edge[];
}
/**
 * One decomposition step: its request metadata plus the regions it produced.
 */
export interface StepView {
  message: string;
  step_idx: number;
  n_regions: number;
  guard_iml: string;
  target_iml: string;
  name: string;
  assuming: string;
  basis: string[];
  merge_with: string | null;
  regions: RegionNodeView[];
}
/**
 * One region node in the decomposition graph.
 *
 * Flattens what used to be a (header, body) pair: the header fields
 * (`id`/`is_root`/`is_leaf`/`step_idx`) let the graph place and style the node
 * card; the body fields feed the detail panel when the node is clicked.
 */
export interface RegionNodeView {
  id: number;
  step_idx: number;
  is_root: boolean;
  is_leaf: boolean;
  raw_id: string;
  constraints: string[];
  invariant: string | null;
  model:
    | {
        [k: string]: string;
      }
    | string
    | null;
  model_eval: string | null;
}
/**
 * A parent -> child link between region nodes (root has `src_id == 0`).
 */
export interface Edge {
  src_id: number;
  dst_id: number;
}
/**
 * The single source of truth for the shape the JS region-decomp widget consumes
 *
 * RegionGroup but with `region` replaced with `region_stat`
 */
export interface RegionGroupView {
  label_path: number[];
  constraints: string[];
  weight: number;
  region_stat?: RegionNonGroupStat | null;
  children?: RegionGroupView[];
}
/**
 * Display stats for one concrete region when in a hierarchical region group.
 */
export interface RegionNonGroupStat {
  invariant: string;
  model?:
    | {
        [k: string]: string;
      }
    | string
    | null;
  model_eval?: string | null;
}
export interface TaskEntry {
  id: string;
  kind: string;
  artifacts: ArtifactEntry[];
}
export interface ArtifactEntry {
  kind: string;
  text: string;
  icon: string | null;
}

/* tslint:disable */
/* eslint-disable */
/**
/* This file was automatically generated from pydantic models by running pydantic2ts.
/* Do not modify it by hand - just update the pydantic models and then re-run the script
*/

export type JSONValue = string | number | boolean | JSONObject | JSONArray | null;
export type JSONArray = JSONValue[];

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
 * the header fields let the graph place and style the node
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
 * A parent -> child link between region nodes.
 */
export interface Edge {
  src_id: number;
  dst_id: number;
}
/**
 * RegionGroup but with `region` replaced with `region_stat`
 */
export interface RegionGroupView {
  /**
   * Full accumulated constraint path from root to this node (root-first).`constraints[-1]` is the constraint introduced at this node's own level.
   */
  constraints: string[];
  /**
   * Positional index path from root to this node (root-first, 1-indexed).Each element is the sibling index at that depth. Displayed as e.g. `1.2.3`.Levels where a constraint applies to all regions are skipped, so the pathlength may be shorter than the tree depth.
   */
  label_path: number[];
  /**
   * Number of regions in the partition at this node's level.
   */
  weight: number;
  region?: RegionNonGroupStat | null;
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
/**
 * Repr for one single task
 */
export interface TaskEntry {
  idx?: number | null;
  id: string;
  kind: string;
  artifacts: ArtifactEntry[];
  other?: JSONObject;
}
export interface ArtifactEntry {
  kind: string;
  /**
   * Pretty-printed imandrax_api.lib value
   */
  repr: string;
}
export interface JSONObject {
  [k: string]: JSONValue;
}

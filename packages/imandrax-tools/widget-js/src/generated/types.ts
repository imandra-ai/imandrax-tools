/* tslint:disable */
/* eslint-disable */
/**
/* This file was automatically generated from pydantic models by running pydantic2ts.
/* Do not modify it by hand - just update the pydantic models and then re-run the script
*/

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
/**
 * The front-end contract for one task and its artifacts.
 */
export interface TaskEntry {
  id: string;
  kind: string;
  artifacts: ArtifactEntry[];
}
/**
 * The front-end contract for one artifact (see `widget-js/src/task`).
 */
export interface ArtifactEntry {
  kind: string;
  text: string;
}

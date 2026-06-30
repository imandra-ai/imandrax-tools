// Transform raw EnrichedDecomposeRes output into the forest `draw` consumes.
//
// The Python side serializes the *raw* region groups — `{constraints,
// label_path, weight, region, children}`, where `region` is null on interior
// nodes. `draw` instead wants each node pre-enriched with the fields computed by
// `RegionGroup.describe()` (imandrax_api_models/region_decomp/__init__.py):
// `introduced_constraint`, `n_children_regions`, `n_leaf_regions`,
// `n_descendant_regions`, and the flattened `invariant`/`example_input`/
// `example_output`.

// Accepts either a bare region-group list or a whole EnrichedDecomposeRes object
// (from which `.region_groups` is taken), so callers can hand it either.
export function toForest(input) {
  return normalizeGroups(input).map(toForestNode);
}

function normalizeGroups(input) {
  return Array.isArray(input)
    ? input
    : input && Array.isArray(input.region_groups)
      ? input.region_groups
      : [];
}

// Mirror of RegionGroup.describe() + to_json_dict(): enrich one raw node and
// recurse into its children.
function toForestNode(raw) {
  const constraints = raw.constraints || [];
  const children = raw.children || [];
  const node = {
    label_path: (raw.label_path || []).map(String).join("."),
    constraints,
    introduced_constraint: constraints.length
      ? constraints[constraints.length - 1]
      : "",
    weight: raw.weight,
    n_children: children.length,
    n_leaf_regions: nLeafRegions(raw),
  };
  if (raw.region != null) {
    // constraints info is already shown in group-level
    node.invariant = raw.region.invariant_str;
    node.example_input = raw.region.model_str;
    node.example_output = raw.region.model_eval_str;
  }
  if (children.length) {
    node.children = children.map(toForestNode);
  }
  return node;
}

// Total leaf regions in this subtree, excluding self.
function nLeafRegions(raw) {
  const children = raw.children || [];
  if (!children.length) return 0;
  return children.reduce((s, c) => s + nLeafRegions(c), 0);
}

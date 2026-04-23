import imandrax_api
from imandrax_api.lib import RegionStr
from inline_snapshot import snapshot

from imandrax_api_models.region_decomp import HumDecomposeRes, RegionGroup


def trust():
    import os
    from typing import NoReturn

    import dotenv

    from imandrax_api_models.client import ImandraXClient

    dotenv.load_dotenv()
    c = ImandraXClient(
        url=imandrax_api.url_prod,
        auth_token=os.environ['IMANDRAX_API_KEY'],
    )
    IML_CODE = """\
    let classify (x : int) (y : int) : int =
    if x > 0 then
        if y > 0 then
        if x > y then 1
        else 2
        else
        if y < -10 then 3
        else 4
    else
        if y > 0 then 5
        else 6"""
    _eval_res = c.eval_src(IML_CODE)
    decomp_res = c.decompose(name='classify')

    def raise_(exc: BaseException) -> NoReturn:
        raise exc

    regions: list[RegionStr] = (
        decomp_res.regions_str
        if (decomp_res.regions_str)
        else (raise_(ValueError('No regions')))
    )
    return regions


def test():
    # regions = trust()
    # assert regions == snapshot()
    regions = [
        RegionStr(
            constraints_str=['x <= 0', 'y <= 0'],
            invariant_str='6',
            model_str={'x': '0', 'y': '0'},
            model_eval_str='6',
        ),
        RegionStr(
            constraints_str=['y >= 1', 'x <= 0'],
            invariant_str='5',
            model_str={'x': '0', 'y': '1'},
            model_eval_str='5',
        ),
        RegionStr(
            constraints_str=['x >= 1', 'y >= (-10)', 'y <= 0'],
            invariant_str='4',
            model_str={'x': '1', 'y': '0'},
            model_eval_str='4',
        ),
        RegionStr(
            constraints_str=['x >= 1', 'y <= (-11)'],
            invariant_str='3',
            model_str={'x': '1', 'y': '(-11)'},
            model_eval_str='3',
        ),
        RegionStr(
            constraints_str=['x <= y', 'x >= 1', 'y >= 1'],
            invariant_str='2',
            model_str={'x': '1', 'y': '1'},
            model_eval_str='2',
        ),
        RegionStr(
            constraints_str=['not (x <= y)', 'x >= 1', 'y >= 1'],
            invariant_str='1',
            model_str={'x': '2', 'y': '1'},
            model_eval_str='1',
        ),
    ]

    hdr = HumDecomposeRes.from_regions(regions)
    assert hdr.to_tree_str() == snapshot("""\
├── [1] constraint='x <= 0' invariant=None (w=2, n_children=2, n_descendants=2)
│   ├── [1.1] constraint='y <= 0' invariant='6' (w=1, n_children=0, n_descendants=0)
│   └── [1.1.2] constraint='y >= 1' invariant='5' (w=1, n_children=0, n_descendants=0)
├── [1.2.1.1] constraint='y >= (-10)' invariant='4' (w=1, n_children=0, n_descendants=0)
├── [1.2.3] constraint='y >= 1' invariant=None (w=2, n_children=2, n_descendants=2)
│   ├── [1.2.3.1.1] constraint='x >= 1' invariant='2' (w=1, n_children=0, n_descendants=0)
│   └── [1.2.3.1.2.1] constraint='not (x <= y)' invariant='1' (w=1, n_children=0, n_descendants=0)
└── [1.2.3.4.1] constraint='y <= (-11)' invariant='3' (w=1, n_children=0, n_descendants=0)\
""")
    hdr_dict_hierarchy = hdr.to_dict_hierarchical()
    hdr_dict_flat = hdr.to_dict_flat()
    label_path_n_children_map_flat: list[tuple[str, int]] = [
        ('.'.join(map(str, irg.label_path)), len(irg.children))
        for irg in hdr_dict_flat['region_groups']
    ]

    # ::: test-child-count
    def _collect_hierarchy(groups: list[RegionGroup]) -> list[tuple[str, int]]:
        result: list[tuple[str, int]] = []
        for rg in groups:
            result.append(('.'.join(map(str, rg.label_path)), len(rg.children)))
            if rg.children:
                result.extend(_collect_hierarchy(rg.children))
        return result

    label_path_n_children_map_hierarchy: list[tuple[str, int]] = _collect_hierarchy(
        hdr_dict_hierarchy['region_groups']
    )
    assert sorted(label_path_n_children_map_flat) == sorted(
        label_path_n_children_map_hierarchy
    )
    # :::

    assert (hdr.dumper_func()(hdr_dict_hierarchy)) == snapshot("""\
summary:
  n_regions: 8
  max_depth: 6
region_groups:
- !RegionGroup
  label_path: '1'
  constraints:
  - x <= 0
  introduced_constraint: x <= 0
  weight: 2
  n_children_regions: 2
  n_descendant_regions: 2
  children:
  - !RegionGroup
    label_path: '1.1'
    constraints:
    - x <= 0
    - y <= 0
    introduced_constraint: y <= 0
    weight: 1
    n_children_regions: 0
    n_descendant_regions: 0
    invariant: '6'
    example_input:
      x: '0'
      y: '0'
    example_output: '6'
  - !RegionGroup
    label_path: 1.1.2
    constraints:
    - x <= 0
    - y <= 0
    - y >= 1
    introduced_constraint: y >= 1
    weight: 1
    n_children_regions: 0
    n_descendant_regions: 0
    invariant: '5'
    example_input:
      x: '0'
      y: '1'
    example_output: '5'
- !RegionGroup
  label_path: 1.2.1.1
  constraints:
  - x <= 0
  - y <= 0
  - x >= 1
  - y >= (-10)
  introduced_constraint: y >= (-10)
  weight: 1
  n_children_regions: 0
  n_descendant_regions: 0
  invariant: '4'
  example_input:
    x: '1'
    y: '0'
  example_output: '4'
- !RegionGroup
  label_path: 1.2.3
  constraints:
  - x <= 0
  - y <= 0
  - y >= 1
  introduced_constraint: y >= 1
  weight: 2
  n_children_regions: 2
  n_descendant_regions: 2
  children:
  - !RegionGroup
    label_path: 1.2.3.1.1
    constraints:
    - x <= 0
    - y <= 0
    - y >= 1
    - x <= y
    - x >= 1
    introduced_constraint: x >= 1
    weight: 1
    n_children_regions: 0
    n_descendant_regions: 0
    invariant: '2'
    example_input:
      x: '1'
      y: '1'
    example_output: '2'
  - !RegionGroup
    label_path: 1.2.3.1.2.1
    constraints:
    - x <= 0
    - y <= 0
    - y >= 1
    - x <= y
    - x >= 1
    - not (x <= y)
    introduced_constraint: not (x <= y)
    weight: 1
    n_children_regions: 0
    n_descendant_regions: 0
    invariant: '1'
    example_input:
      x: '2'
      y: '1'
    example_output: '1'
- !RegionGroup
  label_path: 1.2.3.4.1
  constraints:
  - x <= 0
  - y <= 0
  - y >= 1
  - x >= 1
  - y <= (-11)
  introduced_constraint: y <= (-11)
  weight: 1
  n_children_regions: 0
  n_descendant_regions: 0
  invariant: '3'
  example_input:
    x: '1'
    y: (-11)
  example_output: '3'
""")

    assert (hdr.dumper_func()(hdr_dict_flat)) == snapshot("""\
summary:
  n_regions: 8
  max_depth: 6
region_groups:
- id: '0'
  label_path: '1'
  depth: 1
  weight: 2
  constraints:
  - x <= 0
  introduced_constraint: x <= 0
  n_children_regions: 2
  children:
  - '4'
  - '5'
- id: '4'
  label_path: '1.1'
  depth: 2
  weight: 1
  constraints:
  - x <= 0
  - y <= 0
  introduced_constraint: y <= 0
  invariant: '6'
  example_input:
    x: '0'
    y: '0'
  example_output: '6'
  n_children_regions: 0
  children: []
- id: '5'
  label_path: 1.1.2
  depth: 2
  weight: 1
  constraints:
  - x <= 0
  - y <= 0
  - y >= 1
  introduced_constraint: y >= 1
  invariant: '5'
  example_input:
    x: '0'
    y: '1'
  example_output: '5'
  n_children_regions: 0
  children: []
- id: '1'
  label_path: 1.2.1.1
  depth: 1
  weight: 1
  constraints:
  - x <= 0
  - y <= 0
  - x >= 1
  - y >= (-10)
  introduced_constraint: y >= (-10)
  invariant: '4'
  example_input:
    x: '1'
    y: '0'
  example_output: '4'
  n_children_regions: 0
  children: []
- id: '2'
  label_path: 1.2.3
  depth: 1
  weight: 2
  constraints:
  - x <= 0
  - y <= 0
  - y >= 1
  introduced_constraint: y >= 1
  n_children_regions: 2
  children:
  - '6'
  - '7'
- id: '6'
  label_path: 1.2.3.1.1
  depth: 2
  weight: 1
  constraints:
  - x <= 0
  - y <= 0
  - y >= 1
  - x <= y
  - x >= 1
  introduced_constraint: x >= 1
  invariant: '2'
  example_input:
    x: '1'
    y: '1'
  example_output: '2'
  n_children_regions: 0
  children: []
- id: '7'
  label_path: 1.2.3.1.2.1
  depth: 2
  weight: 1
  constraints:
  - x <= 0
  - y <= 0
  - y >= 1
  - x <= y
  - x >= 1
  - not (x <= y)
  introduced_constraint: not (x <= y)
  invariant: '1'
  example_input:
    x: '2'
    y: '1'
  example_output: '1'
  n_children_regions: 0
  children: []
- id: '3'
  label_path: 1.2.3.4.1
  depth: 1
  weight: 1
  constraints:
  - x <= 0
  - y <= 0
  - y >= 1
  - x >= 1
  - y <= (-11)
  introduced_constraint: y <= (-11)
  invariant: '3'
  example_input:
    x: '1'
    y: (-11)
  example_output: '3'
  n_children_regions: 0
  children: []
""")

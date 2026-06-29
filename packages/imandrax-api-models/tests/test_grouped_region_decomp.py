import os

import imandrax_api
from imandrax_api.lib import RegionStr
from inline_snapshot import snapshot

from imandrax_api_models.proto_models import DecomposeRes
from imandrax_api_models.region_decomp import (
    EnrichedDecomposeRes,
    RegionGroup,
    get_leaf_groups,
)


def trust() -> DecomposeRes:
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
    return c.decompose(name='classify', string_results=True, prune=True)


def test():
    decomp_res = trust()
    edr = EnrichedDecomposeRes.from_decomp_res(decomp_res)

    assert edr.regions_str
    leaf_groups = get_leaf_groups(edr.region_groups)
    assert len(leaf_groups) == len(edr.regions_str)
    for leaf_group in leaf_groups:
        assert len(leaf_group.children) == 0
        assert leaf_group.region, 'Leaf group must be concrete'
        assert leaf_group.region.constraints_str
        assert set(leaf_group.constraints) == set(leaf_group.region.constraints_str)

    assert leaf_groups == snapshot(
        [
            RegionGroup(
                constraints=['x >= 1', 'y >= 1', 'x <= y'],
                label_path=[1, 1, 1],
                weight=1,
                region=RegionStr(
                    constraints_str=['x <= y', 'y >= 1', 'x >= 1'],
                    invariant_str='2',
                    model_str={'x': '1', 'y': '1'},
                    model_eval_str='2',
                ),
            ),
            RegionGroup(
                constraints=['x >= 1', 'y >= 1', 'x > y'],
                label_path=[1, 1, 2],
                weight=1,
                region=RegionStr(
                    constraints_str=['x > y', 'y >= 1', 'x >= 1'],
                    invariant_str='1',
                    model_str={'x': '2', 'y': '1'},
                    model_eval_str='1',
                ),
            ),
            RegionGroup(
                constraints=['x >= 1', 'y <= (-11)'],
                label_path=[1, 2],
                weight=1,
                region=RegionStr(
                    constraints_str=['y <= (-11)', 'x >= 1'],
                    invariant_str='3',
                    model_str={'x': '1', 'y': '(-11)'},
                    model_eval_str='3',
                ),
            ),
            RegionGroup(
                constraints=['x >= 1', 'y <= 0', 'y >= (-10)'],
                label_path=[1, 3, 1],
                weight=1,
                region=RegionStr(
                    constraints_str=['y >= (-10)', 'y <= 0', 'x >= 1'],
                    invariant_str='4',
                    model_str={'x': '1', 'y': '0'},
                    model_eval_str='4',
                ),
            ),
            RegionGroup(
                constraints=['y >= 1', 'x <= 0'],
                label_path=[2, 1],
                weight=1,
                region=RegionStr(
                    constraints_str=['y >= 1', 'x <= 0'],
                    invariant_str='5',
                    model_str={'x': '0', 'y': '1'},
                    model_eval_str='5',
                ),
            ),
            RegionGroup(
                constraints=['x <= 0', 'y <= 0'],
                label_path=[3, 1],
                weight=1,
                region=RegionStr(
                    constraints_str=['y <= 0', 'x <= 0'],
                    invariant_str='6',
                    model_str={'x': '0', 'y': '0'},
                    model_eval_str='6',
                ),
            ),
        ]
    )

    # region_groups is auto-populated on validation from regions_str.
    assert edr.to_tree_str() == snapshot("""\
├── [1] new_constraint='x >= 1' n_leaf_regions=4
│   ├── [1.1] new_constraint='y >= 1' n_leaf_regions=2
│   │   ├── [1.1.1] new_constraint='x <= y' invariant='2' is_leaf=True
│   │   └── [1.1.2] new_constraint='x > y' invariant='1' is_leaf=True
│   ├── [1.2] new_constraint='y <= (-11)' invariant='3' is_leaf=True
│   └── [1.3.1] new_constraint='y >= (-10)' invariant='4' is_leaf=True
├── [2.1] new_constraint='x <= 0' invariant='5' is_leaf=True
└── [3.1] new_constraint='y <= 0' invariant='6' is_leaf=True\
""")

    # (label_path, full constraint path) for every node, depth-first.
    def _walk(groups: list[RegionGroup]) -> list[tuple[str, list[str]]]:
        out: list[tuple[str, list[str]]] = []
        for g in groups:
            out.append(('.'.join(map(str, g.label_path)), g.constraints))
            out.extend(_walk(g.children))
        return out

    assert _walk(edr.region_groups) == snapshot(
        [
            ('1', ['x >= 1']),
            ('1.1', ['x >= 1', 'y >= 1']),
            ('1.1.1', ['x >= 1', 'y >= 1', 'x <= y']),
            ('1.1.2', ['x >= 1', 'y >= 1', 'x > y']),
            ('1.2', ['x >= 1', 'y <= (-11)']),
            ('1.3.1', ['x >= 1', 'y <= 0', 'y >= (-10)']),
            ('2.1', ['y >= 1', 'x <= 0']),
            ('3.1', ['x <= 0', 'y <= 0']),
        ]
    )

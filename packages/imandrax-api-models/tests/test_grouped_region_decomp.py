# ruff: noqa: F401
import copy
import os
from pathlib import Path

import imandrax_api
import pytest
from inline_snapshot import external_file, snapshot

from imandrax_api_models.client import ImandraXClient
from imandrax_api_models.pp.xtype import to_string as xtype_to_string
from imandrax_api_models.proto_models import DecomposeRes
from imandrax_api_models.region_decomp import (
    EnrichedDecomposeRes,
    RegionGroup,
    get_leaf_groups,
)

fence_py = lambda s: f'```python\n{s}\n```'


@pytest.fixture
def decomp_res_classify() -> DecomposeRes:
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


@pytest.fixture
def decomp_res_six_swiss() -> DecomposeRes:
    c = ImandraXClient(
        url=imandrax_api.url_dev,
        auth_token=os.environ['IMANDRAX_API_KEY'],
    )
    IML_CODE = (Path(__file__).parent / 'data/six_swiss.iml').read_text()
    _eval_res = c.eval_src(IML_CODE)
    return c.decompose(name='match_price', string_results=True, prune=True)


# (label_path, full constraint path) for every node, depth-first.
def _walk(groups: list[RegionGroup]) -> list[tuple[str, list[str]]]:
    out: list[tuple[str, list[str]]] = []
    for g in groups:
        out.append(('.'.join(map(str, g.label_path)), g.constraints))
        out.extend(_walk(g.children))
    return out


def enrich_decomp_res_props(edr: EnrichedDecomposeRes) -> None:
    assert edr.regions_str, 'Non-err decomp should have regions_str'
    leaf_groups = get_leaf_groups(edr.region_groups)
    assert len(leaf_groups) == len(edr.regions_str), 'Leaf groups ARE concrete regions'
    for leaf_group in leaf_groups:
        assert len(leaf_group.children) == 0, 'Leaf group must not have children'
        assert leaf_group.region, 'Leaf group must be concrete'
        assert leaf_group.region.constraints_str
        assert set(leaf_group.constraints) == set(leaf_group.region.constraints_str)


def test_complex_decomp(decomp_res_six_swiss):
    edr = EnrichedDecomposeRes.from_decomp_res(decomp_res_six_swiss)
    enrich_decomp_res_props(edr)

    # leaf_groups = get_leaf_groups(edr.region_groups)
    # leaf_groups = [g.model_dump() for g in leaf_groups]
    # assert fence_py(xtype_to_string(leaf_groups)) == external_file(
    #     'data/test_complex_decomp.leaf_groups.expected', format='.txt'
    # )
    assert edr.to_tree_str() == snapshot("""\
├── [1] new_constraint='ob.buys <> []' n_leaf_regions=43
│   ├── [1.1] new_constraint='ob.sells <> []' n_leaf_regions=42
│   │   ├── [1.1.1] new_constraint='(List.hd ob.buys).order_type <> Limit' n_leaf_regions=34
│   │   │   ├── [1.1.1.1] new_constraint='(List.hd ob.sells).order_type <> Limit' n_leaf_regions=28
│   │   │   │   ├── [1.1.1.1.1] new_constraint='(List.hd ob.buys).order_type = Market' n_leaf_regions=21
│   │   │   │   │   ├── [1.1.1.1.1.1] new_constraint='(List.hd ob.buys).order_qty = (List.hd ob.sells).order_qty' n_leaf_regions=17
│   │   │   │   │   │   ├── [1.1.1.1.1.1.1] new_constraint='(List.hd ob.sells).order_type = Market' n_leaf_regions=15
│   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1] new_constraint='List.tl ob.buys <> []' n_leaf_regions=11
│   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.1] new_constraint='List.tl ob.sells <> []' n_leaf_regions=8
│   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.1.1] new_constraint='(List.hd (List.tl ob.buys)).order_type <> Market' n_leaf_regions=5
│   │   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.1.1.1] new_constraint='(List.hd (List.tl ob.buys)).order_price <=. ref_price' n_leaf_regions=3
│   │   │   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.1.1.1.1] new_constraint='(List.hd (List.tl ob.sells)).order_type <> Market' n_leaf_regions=2
│   │   │   │   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.1.1.1.1.1] new_constraint='ref_price <=. (List.hd (List.tl ob.sells)).order_price' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.1.1.1.1.1.2] new_constraint='ref_price >. (List.hd (List.tl ob.sells)).order_price' invariant='Some (List.hd (List.tl ob.sells)).order_price' is_leaf=True
│   │   │   │   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.1.1.1.1.2] new_constraint='(List.hd (List.tl ob.sells)).order_type = Market' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.1.1.2.1] new_constraint='(List.hd (List.tl ob.buys)).order_price >. ref_price' invariant='Some (List.hd (List.tl ob.buys)).order_price' is_leaf=True
│   │   │   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.1.1.1.3.1] new_constraint='(List.hd (List.tl ob.sells)).order_type = Market' invariant='Some (List.hd (List.tl ob.buys)).order_price' is_leaf=True
│   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.1.2] new_constraint='(List.hd (List.tl ob.buys)).order_type = Market' n_leaf_regions=2
│   │   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.1.2.1] new_constraint='ref_price <=. (List.hd (List.tl ob.sells)).order_price' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.1.1.2.2] new_constraint='ref_price >. (List.hd (List.tl ob.sells)).order_price' invariant='Some (List.hd (List.tl ob.sells)).order_price' is_leaf=True
│   │   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.1.1.3.1] new_constraint='(List.hd (List.tl ob.sells)).order_type = Market' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.2] new_constraint='List.tl ob.sells = []' n_leaf_regions=2
│   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.2.1] new_constraint='(List.hd (List.tl ob.buys)).order_price <=. ref_price' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.1.2.2] new_constraint='(List.hd (List.tl ob.buys)).order_price >. ref_price' invariant='Some (List.hd (List.tl ob.buys)).order_price' is_leaf=True
│   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.1.3.1] new_constraint='List.tl ob.sells = []' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.2] new_constraint='List.tl ob.buys = []' n_leaf_regions=3
│   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.2.1] new_constraint='(List.hd (List.tl ob.sells)).order_type <> Market' n_leaf_regions=2
│   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.2.1.1] new_constraint='ref_price <=. (List.hd (List.tl ob.sells)).order_price' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.2.1.2] new_constraint='ref_price >. (List.hd (List.tl ob.sells)).order_price' invariant='Some (List.hd (List.tl ob.sells)).order_price' is_leaf=True
│   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.2.2] new_constraint='(List.hd (List.tl ob.sells)).order_type = Market' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.3.1] new_constraint='List.tl ob.sells = []' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   ├── [1.1.1.1.1.1.2.1.1.1] new_constraint='(List.hd ob.sells).order_type <> Market' invariant='Some (List.hd (List.tl ob.buys)).order_price' is_leaf=True
│   │   │   │   │   │   └── [1.1.1.1.1.1.3.1.1.1] new_constraint='(List.hd ob.sells).order_type <> Market' invariant='Some (List.hd ob.sells).order_price' is_leaf=True
│   │   │   │   │   ├── [1.1.1.1.1.2.1] new_constraint='(List.hd ob.buys).order_qty <> (List.hd ob.sells).order_qty' invariant='None' is_leaf=True
│   │   │   │   │   └── [1.1.1.1.1] new_constraint='(List.hd ob.sells).order_type <> Market' n_leaf_regions=3
│   │   │   │   │       ├── [1.1.1.1.1.1] new_constraint='(List.hd ob.buys).order_time > (List.hd ob.sells).order_time' n_leaf_regions=2
│   │   │   │   │       │   ├── [1.1.1.1.1.1.1.1] new_constraint='(List.hd ob.buys).order_qty <> (List.hd ob.sells).order_qty' invariant='None' is_leaf=True
│   │   │   │   │       │   └── [1.1.1.1.1.1.2] new_constraint='(List.hd ob.buys).order_qty > (List.hd ob.sells).order_qty' invariant='Some (List.hd ob.sells).order_price' is_leaf=True
│   │   │   │   │       └── [1.1.1.1.1.2] new_constraint='(List.hd ob.buys).order_time <= (List.hd ob.sells).order_time' invariant='Some (List.hd ob.sells).order_price' is_leaf=True
│   │   │   │   ├── [1.1.1.1.2] new_constraint='(List.hd ob.buys).order_type <> Market' n_leaf_regions=5
│   │   │   │   │   ├── [1.1.1.1.2.1] new_constraint='(List.hd ob.buys).order_time > (List.hd ob.sells).order_time' n_leaf_regions=4
│   │   │   │   │   │   ├── [1.1.1.1.2.1.1] new_constraint='(List.hd ob.sells).order_qty <= (List.hd ob.buys).order_qty' n_leaf_regions=3
│   │   │   │   │   │   │   ├── [1.1.1.1.2.1.1.1] new_constraint='(List.hd ob.buys).order_qty = (List.hd ob.sells).order_qty' n_leaf_regions=2
│   │   │   │   │   │   │   │   ├── [1.1.1.1.2.1.1.1.1] new_constraint='List.tl ob.sells <> []' invariant='Some (List.hd (List.tl ob.sells)).order_price' is_leaf=True
│   │   │   │   │   │   │   │   └── [1.1.1.1.2.1.1.1.2] new_constraint='List.tl ob.sells = []' invariant='Some (List.hd ob.buys).order_price' is_leaf=True
│   │   │   │   │   │   │   └── [1.1.1.1.2.1.1.2] new_constraint='(List.hd ob.buys).order_qty <> (List.hd ob.sells).order_qty' invariant='None' is_leaf=True
│   │   │   │   │   │   └── [1.1.1.1.2.1.2] new_constraint='(List.hd ob.sells).order_qty > (List.hd ob.buys).order_qty' invariant='Some (List.hd ob.sells).order_price' is_leaf=True
│   │   │   │   │   └── [1.1.1.1.2.2] new_constraint='(List.hd ob.buys).order_time <= (List.hd ob.sells).order_time' invariant='Some (List.hd ob.buys).order_price' is_leaf=True
│   │   │   │   ├── [1.1.1.1.3.1.1] new_constraint='(List.hd ob.sells).order_type <> Market' invariant='Some (List.hd ob.sells).order_price' is_leaf=True
│   │   │   │   └── [1.1.1.1.4.1.1] new_constraint='(List.hd ob.sells).order_type <> Market' invariant='Some (List.hd ob.buys).order_price' is_leaf=True
│   │   │   ├── [1.1.1.2.1] new_constraint='(List.hd ob.sells).order_type = Limit' invariant='Some (List.hd ob.sells).order_price' is_leaf=True
│   │   │   ├── [1.1.1.3] new_constraint='(List.hd ob.sells).order_type = Limit' n_leaf_regions=2
│   │   │   │   ├── [1.1.1.3.1] new_constraint='List.tl ob.sells <> []' invariant='Some (List.hd (List.tl ob.sells)).order_price' is_leaf=True
│   │   │   │   └── [1.1.1.3.2] new_constraint='List.tl ob.sells = []' invariant='Some (List.hd ob.buys).order_price' is_leaf=True
│   │   │   ├── [1.1.1.4] new_constraint='(List.hd ob.sells).order_type = Limit' n_leaf_regions=2
│   │   │   │   ├── [1.1.1.4.1.1] new_constraint='(List.hd ob.sells).order_qty <= (List.hd ob.buys).order_qty' invariant='None' is_leaf=True
│   │   │   │   └── [1.1.1.4.2] new_constraint='(List.hd ob.sells).order_qty > (List.hd ob.buys).order_qty' invariant='Some (List.hd ob.sells).order_price' is_leaf=True
│   │   │   └── [1.1.1.5.1.1] new_constraint='(List.hd ob.sells).order_type = Limit' invariant='Some (List.hd ob.buys).order_price' is_leaf=True
│   │   ├── [1.1.2] new_constraint='(List.hd ob.buys).order_type = Limit' n_leaf_regions=6
│   │   │   ├── [1.1.2.1] new_constraint='(List.hd ob.sells).order_type <> Market' n_leaf_regions=5
│   │   │   │   ├── [1.1.2.1.1] new_constraint='(List.hd ob.buys).order_time > (List.hd ob.sells).order_time' n_leaf_regions=4
│   │   │   │   │   ├── [1.1.2.1.1.1] new_constraint='(List.hd ob.buys).order_qty <= (List.hd ob.sells).order_qty' n_leaf_regions=3
│   │   │   │   │   │   ├── [1.1.2.1.1.1.1] new_constraint='(List.hd ob.sells).order_qty = (List.hd ob.buys).order_qty' n_leaf_regions=2
│   │   │   │   │   │   │   ├── [1.1.2.1.1.1.1.1] new_constraint='List.tl ob.buys <> []' invariant='Some (List.hd (List.tl ob.buys)).order_price' is_leaf=True
│   │   │   │   │   │   │   └── [1.1.2.1.1.1.1.2] new_constraint='List.tl ob.buys = []' invariant='Some (List.hd ob.sells).order_price' is_leaf=True
│   │   │   │   │   │   └── [1.1.2.1.1.1.2] new_constraint='(List.hd ob.sells).order_qty <> (List.hd ob.buys).order_qty' invariant='None' is_leaf=True
│   │   │   │   │   └── [1.1.2.1.1.2] new_constraint='(List.hd ob.buys).order_qty > (List.hd ob.sells).order_qty' invariant='Some (List.hd ob.buys).order_price' is_leaf=True
│   │   │   │   └── [1.1.2.1.2] new_constraint='(List.hd ob.buys).order_time <= (List.hd ob.sells).order_time' invariant='Some (List.hd ob.sells).order_price' is_leaf=True
│   │   │   └── [1.1.2.2] new_constraint='(List.hd ob.sells).order_type = Market' invariant='Some (List.hd ob.buys).order_price' is_leaf=True
│   │   ├── [1.1.3.1.1] new_constraint='(List.hd ob.sells).order_type = Limit' invariant='Some (List.hd ob.sells).order_price' is_leaf=True
│   │   └── [1.1.4.1.1] new_constraint='(List.hd ob.sells).order_type = Limit' invariant='Some (List.hd ob.buys).order_price' is_leaf=True
│   └── [1.2] new_constraint='ob.sells = []' invariant='None' is_leaf=True
└── [2] new_constraint='ob.buys = []' invariant='None' is_leaf=True\
""")


def test_simple_decomp(decomp_res_classify):
    edr = EnrichedDecomposeRes.from_decomp_res(decomp_res_classify)
    enrich_decomp_res_props(edr)

    # leaf_groups = get_leaf_groups(edr.region_groups)
    # assert fence_py(xtype_to_string(leaf_groups)) == external_file(
    #     'data/test_simple_decomp.leaf_groups.expected', format='.txt'
    # )

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


@pytest.mark.parametrize(
    'decomp_res_fixture',
    ['decomp_res_classify', 'decomp_res_six_swiss'],
)
def test_region_group_constr_equivalence(decomp_res_fixture: str, request):
    decomp_res: DecomposeRes = request.getfixturevalue(decomp_res_fixture)
    erd1 = EnrichedDecomposeRes.from_decomp_res(decomp_res)

    decomp_res2 = copy.deepcopy(decomp_res)
    decomp_res2.regions_str = None
    erd2 = EnrichedDecomposeRes.from_decomp_res(decomp_res2)

    assert _walk(erd1.region_groups) == _walk(erd2.region_groups)

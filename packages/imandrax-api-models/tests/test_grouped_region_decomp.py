import collections
import dataclasses
import os
import warnings
from functools import partial, reduce
from pathlib import Path
from typing import Any

import imandrax_api
import imandrax_api.lib as xtype
import pytest
from imandrax_api_models.client import ImandraXClient
from imandrax_api_models.proto_models import DecomposeRes
from imandrax_api_models.region_decomp import (
    EnrichedDecomposeRes,
    ForTesting,
    OverlappingRegionsWarning,
    Region,
    RegionGroup,
    get_leaf_groups,
    group_regions,
    mir_regions_of_fun_decomp_artifact,
    rgs_of_mir_fun_decomp,
)
from imandrax_api_models.region_decomp._common import term_to_string
from inline_snapshot import snapshot

pytestmark = pytest.mark.vcr


def fence_py(s: str) -> str: return f'```python\n{s}\n```'  # fmt: skip


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
        url=imandrax_api.url_prod,
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
        assert leaf_group.region.constraints
        # assert set(leaf_group.constraints) == set(leaf_group.region.constraints), (
        #     "Leaf group's constraints IS region's constraints"
        # )
    label_paths: list[list[int]] = [leaf_group.label_path for leaf_group in leaf_groups]
    label_paths_str = ['.'.join(map(str, path)) for path in label_paths]
    assert len(label_paths_str) == len(set(label_paths_str)), (
        'Label paths must be unique'
    )


def test_complex_decomp(decomp_res_six_swiss: DecomposeRes):
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
│   │   ├── [1.1.1] new_constraint='hd ob.buys.order_type <> Limit' n_leaf_regions=34
│   │   │   ├── [1.1.1.1] new_constraint='hd ob.sells.order_type <> Limit' n_leaf_regions=28
│   │   │   │   ├── [1.1.1.1.1] new_constraint='hd ob.buys.order_type = Market' n_leaf_regions=21
│   │   │   │   │   ├── [1.1.1.1.1.1] new_constraint='hd ob.buys.order_qty = hd ob.sells.order_qty' n_leaf_regions=17
│   │   │   │   │   │   ├── [1.1.1.1.1.1.1] new_constraint='hd ob.sells.order_type = Market' n_leaf_regions=15
│   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1] new_constraint='tl ob.buys <> []' n_leaf_regions=11
│   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.1] new_constraint='tl ob.sells <> []' n_leaf_regions=8
│   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.1.1] new_constraint='hd (tl ob.buys).order_type <> Market' n_leaf_regions=5
│   │   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.1.1.1] new_constraint='hd (tl ob.buys).order_price <=. ref_price' n_leaf_regions=3
│   │   │   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.1.1.1.1] new_constraint='hd (tl ob.sells).order_type <> Market' n_leaf_regions=2
│   │   │   │   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.1.1.1.1.1] new_constraint='ref_price <=. hd (tl ob.sells).order_price' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.1.1.1.1.1.2] new_constraint='ref_price >. hd (tl ob.sells).order_price' invariant='Some hd (tl ob.sells).order_price' is_leaf=True
│   │   │   │   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.1.1.1.1.2] new_constraint='hd (tl ob.sells).order_type = Market' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.1.1.2.1] new_constraint='hd (tl ob.buys).order_price >. ref_price' invariant='Some hd (tl ob.buys).order_price' is_leaf=True
│   │   │   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.1.1.1.3.1] new_constraint='hd (tl ob.sells).order_type = Market' invariant='Some hd (tl ob.buys).order_price' is_leaf=True
│   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.1.2] new_constraint='hd (tl ob.buys).order_type = Market' n_leaf_regions=2
│   │   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.1.2.1] new_constraint='ref_price <=. hd (tl ob.sells).order_price' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.1.1.2.2] new_constraint='ref_price >. hd (tl ob.sells).order_price' invariant='Some hd (tl ob.sells).order_price' is_leaf=True
│   │   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.1.1.3.1] new_constraint='hd (tl ob.sells).order_type = Market' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.2] new_constraint='tl ob.sells = []' n_leaf_regions=2
│   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.1.2.1] new_constraint='hd (tl ob.buys).order_price <=. ref_price' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.1.2.2] new_constraint='hd (tl ob.buys).order_price >. ref_price' invariant='Some hd (tl ob.buys).order_price' is_leaf=True
│   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.1.3.1] new_constraint='tl ob.sells = []' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.2] new_constraint='tl ob.buys = []' n_leaf_regions=3
│   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.2.1] new_constraint='hd (tl ob.sells).order_type <> Market' n_leaf_regions=2
│   │   │   │   │   │   │   │   │   ├── [1.1.1.1.1.1.1.2.1.1] new_constraint='ref_price <=. hd (tl ob.sells).order_price' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.2.1.2] new_constraint='ref_price >. hd (tl ob.sells).order_price' invariant='Some hd (tl ob.sells).order_price' is_leaf=True
│   │   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.2.2] new_constraint='hd (tl ob.sells).order_type = Market' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   │   └── [1.1.1.1.1.1.1.3.1] new_constraint='tl ob.sells = []' invariant='Some ref_price' is_leaf=True
│   │   │   │   │   │   ├── [1.1.1.1.1.1.2.1.1.1] new_constraint='hd ob.sells.order_type <> Market' invariant='Some hd (tl ob.buys).order_price' is_leaf=True
│   │   │   │   │   │   └── [1.1.1.1.1.1.3.1.1.1] new_constraint='hd ob.sells.order_type <> Market' invariant='Some hd ob.sells.order_price' is_leaf=True
│   │   │   │   │   ├── [1.1.1.1.1.2.1] new_constraint='hd ob.buys.order_qty <> hd ob.sells.order_qty' invariant='None' is_leaf=True
│   │   │   │   │   └── [1.1.1.1.1.3] new_constraint='hd ob.sells.order_type <> Market' n_leaf_regions=3
│   │   │   │   │       ├── [1.1.1.1.1.3.1] new_constraint='hd ob.buys.order_time > hd ob.sells.order_time' n_leaf_regions=2
│   │   │   │   │       │   ├── [1.1.1.1.1.3.1.1.1] new_constraint='hd ob.buys.order_qty <> hd ob.sells.order_qty' invariant='None' is_leaf=True
│   │   │   │   │       │   └── [1.1.1.1.1.3.1.2] new_constraint='hd ob.buys.order_qty > hd ob.sells.order_qty' invariant='Some hd ob.sells.order_price' is_leaf=True
│   │   │   │   │       └── [1.1.1.1.1.3.2] new_constraint='hd ob.buys.order_time <= hd ob.sells.order_time' invariant='Some hd ob.sells.order_price' is_leaf=True
│   │   │   │   ├── [1.1.1.1.2] new_constraint='hd ob.buys.order_type <> Market' n_leaf_regions=5
│   │   │   │   │   ├── [1.1.1.1.2.1] new_constraint='hd ob.buys.order_time > hd ob.sells.order_time' n_leaf_regions=4
│   │   │   │   │   │   ├── [1.1.1.1.2.1.1] new_constraint='hd ob.sells.order_qty <= hd ob.buys.order_qty' n_leaf_regions=3
│   │   │   │   │   │   │   ├── [1.1.1.1.2.1.1.1] new_constraint='hd ob.buys.order_qty = hd ob.sells.order_qty' n_leaf_regions=2
│   │   │   │   │   │   │   │   ├── [1.1.1.1.2.1.1.1.1] new_constraint='tl ob.sells <> []' invariant='Some hd (tl ob.sells).order_price' is_leaf=True
│   │   │   │   │   │   │   │   └── [1.1.1.1.2.1.1.1.2] new_constraint='tl ob.sells = []' invariant='Some hd ob.buys.order_price' is_leaf=True
│   │   │   │   │   │   │   └── [1.1.1.1.2.1.1.2] new_constraint='hd ob.buys.order_qty <> hd ob.sells.order_qty' invariant='None' is_leaf=True
│   │   │   │   │   │   └── [1.1.1.1.2.1.2] new_constraint='hd ob.sells.order_qty > hd ob.buys.order_qty' invariant='Some hd ob.sells.order_price' is_leaf=True
│   │   │   │   │   └── [1.1.1.1.2.2] new_constraint='hd ob.buys.order_time <= hd ob.sells.order_time' invariant='Some hd ob.buys.order_price' is_leaf=True
│   │   │   │   ├── [1.1.1.1.3.1.1] new_constraint='hd ob.sells.order_type <> Market' invariant='Some hd ob.sells.order_price' is_leaf=True
│   │   │   │   └── [1.1.1.1.4.1.1] new_constraint='hd ob.sells.order_type <> Market' invariant='Some hd ob.buys.order_price' is_leaf=True
│   │   │   ├── [1.1.1.2.1] new_constraint='hd ob.sells.order_type = Limit' invariant='Some hd ob.sells.order_price' is_leaf=True
│   │   │   ├── [1.1.1.3] new_constraint='hd ob.sells.order_type = Limit' n_leaf_regions=2
│   │   │   │   ├── [1.1.1.3.1] new_constraint='tl ob.sells <> []' invariant='Some hd (tl ob.sells).order_price' is_leaf=True
│   │   │   │   └── [1.1.1.3.2] new_constraint='tl ob.sells = []' invariant='Some hd ob.buys.order_price' is_leaf=True
│   │   │   ├── [1.1.1.4] new_constraint='hd ob.sells.order_type = Limit' n_leaf_regions=2
│   │   │   │   ├── [1.1.1.4.1.1] new_constraint='hd ob.sells.order_qty <= hd ob.buys.order_qty' invariant='None' is_leaf=True
│   │   │   │   └── [1.1.1.4.2] new_constraint='hd ob.sells.order_qty > hd ob.buys.order_qty' invariant='Some hd ob.sells.order_price' is_leaf=True
│   │   │   └── [1.1.1.5.1.1] new_constraint='hd ob.sells.order_type = Limit' invariant='Some hd ob.buys.order_price' is_leaf=True
│   │   ├── [1.1.2] new_constraint='hd ob.buys.order_type = Limit' n_leaf_regions=6
│   │   │   ├── [1.1.2.1] new_constraint='hd ob.sells.order_type <> Market' n_leaf_regions=5
│   │   │   │   ├── [1.1.2.1.1] new_constraint='hd ob.buys.order_time > hd ob.sells.order_time' n_leaf_regions=4
│   │   │   │   │   ├── [1.1.2.1.1.1] new_constraint='hd ob.buys.order_qty <= hd ob.sells.order_qty' n_leaf_regions=3
│   │   │   │   │   │   ├── [1.1.2.1.1.1.1] new_constraint='hd ob.sells.order_qty = hd ob.buys.order_qty' n_leaf_regions=2
│   │   │   │   │   │   │   ├── [1.1.2.1.1.1.1.1] new_constraint='tl ob.buys <> []' invariant='Some hd (tl ob.buys).order_price' is_leaf=True
│   │   │   │   │   │   │   └── [1.1.2.1.1.1.1.2] new_constraint='tl ob.buys = []' invariant='Some hd ob.sells.order_price' is_leaf=True
│   │   │   │   │   │   └── [1.1.2.1.1.1.2] new_constraint='hd ob.sells.order_qty <> hd ob.buys.order_qty' invariant='None' is_leaf=True
│   │   │   │   │   └── [1.1.2.1.1.2] new_constraint='hd ob.buys.order_qty > hd ob.sells.order_qty' invariant='Some hd ob.buys.order_price' is_leaf=True
│   │   │   │   └── [1.1.2.1.2] new_constraint='hd ob.buys.order_time <= hd ob.sells.order_time' invariant='Some hd ob.sells.order_price' is_leaf=True
│   │   │   └── [1.1.2.2] new_constraint='hd ob.sells.order_type = Market' invariant='Some hd ob.buys.order_price' is_leaf=True
│   │   ├── [1.1.3.1.1] new_constraint='hd ob.sells.order_type = Limit' invariant='Some hd ob.sells.order_price' is_leaf=True
│   │   └── [1.1.4.1.1] new_constraint='hd ob.sells.order_type = Limit' invariant='Some hd ob.buys.order_price' is_leaf=True
│   └── [1.2] new_constraint='ob.sells = []' invariant='None' is_leaf=True
└── [2] new_constraint='ob.buys = []' invariant='None' is_leaf=True\
""")


def test_simple_decomp(decomp_res_classify: DecomposeRes):
    edr = EnrichedDecomposeRes.from_decomp_res(decomp_res_classify)
    enrich_decomp_res_props(edr)

    # The following tests are commented out because we haven't found a way to stablize the representation of region groups
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
│   ├── [1.2] new_constraint='y <= -11' invariant='3' is_leaf=True
│   └── [1.3.1] new_constraint='y >= -10' invariant='4' is_leaf=True
├── [2.1] new_constraint='x <= 0' invariant='5' is_leaf=True
└── [3.1] new_constraint='y <= 0' invariant='6' is_leaf=True\
""")

    assert _walk(edr.region_groups) == snapshot(
        [
            ('1', ['x >= 1']),
            ('1.1', ['x >= 1', 'y >= 1']),
            ('1.1.1', ['x >= 1', 'y >= 1', 'x <= y']),
            ('1.1.2', ['x >= 1', 'y >= 1', 'x > y']),
            ('1.2', ['x >= 1', 'y <= -11']),
            ('1.3.1', ['x >= 1', 'y <= 0', 'y >= -10']),
            ('2.1', ['y >= 1', 'x <= 0']),
            ('3.1', ['x <= 0', 'y <= 0']),
        ]
    )


@pytest.mark.parametrize(
    'decomp_res_fixture',
    ['decomp_res_classify', 'decomp_res_six_swiss'],
)
def test_region_group_constr_equivalence(
    decomp_res_fixture: str, request: pytest.FixtureRequest
):

    decomp_res: DecomposeRes = request.getfixturevalue(decomp_res_fixture)
    assert decomp_res.artifact is not None

    mir_regions = mir_regions_of_fun_decomp_artifact(decomp_res.artifact)
    regions: list[Region] = [Region.from_mir_region(r) for r in mir_regions]

    stringified_term_map = reduce(
        lambda acc, r: ForTesting.stringified_term_map_of_region(r, acc),
        mir_regions,
        {},
    )

    rgs1 = group_regions(regions)  # default: memoized `eq_term_with_pp`
    rgs2 = group_regions(
        regions,
        eq_term=partial(
            ForTesting.eq_term_with_string_results,
            stringified_term_map=stringified_term_map,
        ),
    )
    rgs3 = group_regions(regions, eq_term=ForTesting.mk_eq_term_naive())

    assert _walk(rgs1) == _walk(rgs2)
    assert _walk(rgs1) == _walk(rgs3)


# Disjoint regions
# ====================


def _region_ids(g: RegionGroup) -> list[int]:
    """Identities of the input regions under `g`, one per leaf."""
    return [id(leaf.region) for leaf in get_leaf_groups([g])]


def _assert_partitioned_by(
    regions: list[int], parts: list[list[int]], *, where: str
) -> None:
    flat = [r for part in parts for r in part]
    assert len(flat) == len(set(flat)), f'{where}: parts share a region'
    assert sorted(flat) == sorted(regions), f'{where}: parts do not cover it'


@pytest.mark.parametrize(
    'decomp_res_fixture',
    ['decomp_res_classify', 'decomp_res_six_swiss'],
)
def test_disjoint_regions_form_a_hierarchy(
    decomp_res_fixture: str, request: pytest.FixtureRequest
):
    """
    With disjoint input regions, the groups form a strict hierarchy.

    - Every leaf holds one region, with weight 1.
    - The top-level groups partition the input regions, and each node's
      children partition the node's regions.
    - A child's constraint path and label path strictly extend its parent's.
    - A node's weight is the number of regions under it.
    """
    decomp_res: DecomposeRes = request.getfixturevalue(decomp_res_fixture)
    assert decomp_res.artifact is not None
    regions = [
        Region.from_mir_region(r)
        for r in mir_regions_of_fun_decomp_artifact(decomp_res.artifact)
    ]

    with warnings.catch_warnings():
        warnings.simplefilter('error')  # no overlap, so no warning
        groups = group_regions(regions)

    _assert_partitioned_by(
        [id(r) for r in regions], [_region_ids(g) for g in groups], where='top level'
    )

    def check(node: RegionGroup) -> None:
        where = '.'.join(map(str, node.label_path))
        assert node.weight == len(_region_ids(node)), where
        if not node.children:
            assert node.region is not None, where
            assert node.weight == 1, where
            return
        assert node.region is None, where
        _assert_partitioned_by(
            _region_ids(node), [_region_ids(c) for c in node.children], where=where
        )
        for c in node.children:
            n_constraints, n_labels = len(node.constraints), len(node.label_path)
            assert len(c.constraints) > n_constraints, where
            assert c.constraints[:n_constraints] == node.constraints, where
            assert len(c.label_path) > n_labels, where
            assert c.label_path[:n_labels] == node.label_path, where
            check(c)

    for g in groups:
        check(g)
    assert any(g.children for g in groups), 'expected at least one nested level'


# Overlapping regions
# ====================


class TestOverlappingRegions:
    """
    The server is expected to return disjoint regions. When it does not (e.g. two
    regions with the same constraint set), every region must still be in a leaf.
    """  # noqa: D205

    # ai-disclosure: ai-generated

    def _classify_regions(self, decomp_res: DecomposeRes) -> list[Region]:
        assert decomp_res.artifact is not None
        return [
            Region.from_mir_region(r)
            for r in mir_regions_of_fun_decomp_artifact(decomp_res.artifact)
        ]

    def _with_reversed_constraints(self, r: Region) -> Region:
        """A copy of `r` with the same constraint set, listed in reverse order."""
        mir = dataclasses.replace(
            r.mir_region, constraints=list(reversed(r.mir_region.constraints))
        )
        return dataclasses.replace(r, mir_region=mir, id=f'{r.id}-dup')

    def _assert_every_region_is_in_one_leaf(
        self, groups: list[RegionGroup], regions: list[Region]
    ) -> list[RegionGroup]:
        """Each leaf stands for all regions with its constraint set; weights add up."""
        leaves = get_leaf_groups(groups)
        assert all(g.region is not None for g in leaves), 'Leaf group must be concrete'
        label_paths = ['.'.join(map(str, g.label_path)) for g in leaves]
        assert len(label_paths) == len(set(label_paths)), 'Label paths must be unique'

        def key(r: Region) -> frozenset[str]:
            return frozenset(term_to_string(c) for c in r.constraints)

        weight_of_key = {key(g.region): g.weight for g in leaves if g.region}
        assert len(weight_of_key) == len(leaves), 'Copies must share one leaf'
        assert weight_of_key == dict(collections.Counter(key(r) for r in regions))

        def assert_weights_add_up(g: RegionGroup) -> None:
            if g.children:
                assert g.weight == sum(c.weight for c in g.children)
                for c in g.children:
                    assert_weights_add_up(c)

        for g in groups:
            assert_weights_add_up(g)
        return list(leaves)

    def test_distinct_regions_do_not_warn(self, decomp_res_classify: DecomposeRes):
        regions = self._classify_regions(decomp_res_classify)
        with warnings.catch_warnings():
            warnings.simplefilter('error')
            groups = group_regions(regions)
        leaves = self._assert_every_region_is_in_one_leaf(groups, regions)
        assert all(g.weight == 1 for g in leaves)

    def test_duplicate_constraint_sets_are_merged(
        self, decomp_res_classify: DecomposeRes
    ):
        base = self._classify_regions(decomp_res_classify)
        for original in base:
            dup = self._with_reversed_constraints(original)
            regions = [*base, dup]

            with pytest.warns(UserWarning, match='merged'):
                groups = group_regions(regions)

            leaves = self._assert_every_region_is_in_one_leaf(groups, regions)
            [merged] = [g for g in leaves if g.weight == 2]
            assert merged.region is original

    def test_nested_constraint_sets_are_kept(self, decomp_res_classify: DecomposeRes):
        """A region whose constraints are a strict subset of another region's."""
        base = self._classify_regions(decomp_res_classify)
        for r in base:
            if len(r.constraints) < 2:
                continue
            wider = dataclasses.replace(
                r,
                mir_region=dataclasses.replace(
                    r.mir_region, constraints=r.mir_region.constraints[:-1]
                ),
                id=f'{r.id}-wider',
            )
            regions = [*base, wider]

            with pytest.warns(UserWarning, match='contains'):
                groups = group_regions(regions)

            leaves = self._assert_every_region_is_in_one_leaf(groups, regions)
            assert all(g.weight == 1 for g in leaves)

    def test_same_region_given_twice(self, decomp_res_classify: DecomposeRes):
        r = self._classify_regions(decomp_res_classify)[0]
        with pytest.warns(UserWarning, match='merged'):
            groups = group_regions([r, r])
        assert [(g.region, g.label_path, g.weight) for g in groups] == [(r, [1], 2)]

    def test_copies_under_one_node_take_its_label(
        self, decomp_res_classify: DecomposeRes
    ):
        """Two copies of `{A}` next to `{B}`: the merged leaf is labelled `1`, not `1.1`."""
        base = self._classify_regions(decomp_res_classify)
        terms = {term_to_string(c): c for r in base for c in r.constraints}
        a, b = (terms[k] for k in sorted(terms)[:2])
        regions = [
            self._region_with(base[0], [a], 'a1'),
            self._region_with(base[0], [a], 'a2'),
            self._region_with(base[1], [b], 'b'),
        ]
        with pytest.warns(UserWarning, match='merged'):
            groups = group_regions(regions)
        self._assert_every_region_is_in_one_leaf(groups, regions)
        assert [(g.label_path, g.weight) for g in groups] == [([1], 2), ([2], 1)]

    def test_copies_with_different_invariants_raise(
        self, decomp_res_classify: DecomposeRes
    ):
        r, other = self._classify_regions(decomp_res_classify)[:2]
        mir = dataclasses.replace(r.mir_region, invariant=other.mir_region.invariant)
        dup = dataclasses.replace(r, mir_region=mir, id=f'{r.id}-dup')
        assert dup.non_group_stat().invariant != r.non_group_stat().invariant

        with (
            pytest.warns(OverlappingRegionsWarning, match='merged'),
            pytest.raises(ValueError, match='different invariants'),
        ):
            group_regions([r, dup])

    def test_copies_with_different_models_keep_the_first(
        self, decomp_res_classify: DecomposeRes
    ):
        """Each copy's model is only one witness of the region: warn, don't raise."""
        r = self._classify_regions(decomp_res_classify)[0]
        assert r.string_result is not None
        string_result = {**r.string_result, 'model': {'x': '12345'}}
        dup = dataclasses.replace(r, string_result=string_result, id=f'{r.id}-dup')
        assert dup.non_group_stat().model != r.non_group_stat().model

        with pytest.warns(OverlappingRegionsWarning, match='different model') as rec:
            groups = group_regions([r, dup])
        assert len(rec) == 1
        assert [(g.region, g.weight) for g in groups] == [(r, 2)]

    def test_warning_points_at_the_caller(self, decomp_res_classify: DecomposeRes):
        """Through a library wrapper, the warning names the caller's line."""
        assert decomp_res_classify.artifact is not None
        art = decomp_res_classify.artifact
        fun_decomp = xtype.read_artifact_data(data=art.data, kind=art.kind)
        assert isinstance(fun_decomp, xtype.Common_Fun_decomp_t_poly)
        r = fun_decomp.regions[0]
        doubled = dataclasses.replace(fun_decomp, regions=[r, r])

        with pytest.warns(OverlappingRegionsWarning) as rec:
            rgs_of_mir_fun_decomp(doubled)
        assert rec[0].filename == __file__

    def _region_with(self, base: Region, constraints: list[Any], tag: str) -> Region:
        mir = dataclasses.replace(base.mir_region, constraints=constraints)
        return dataclasses.replace(base, mir_region=mir, id=f'{base.id}-{tag}')

    def test_nested_regions_split_apart_still_warn(
        self, decomp_res_classify: DecomposeRes
    ):
        """
        {A} contains {A, B}, but the grouping never puts them together.

        B is the most frequent constraint, so {A, B} is split off with {B, C} and
        {B, D} first, and {A} is grouped on its own.
        """
        base = self._classify_regions(decomp_res_classify)
        terms = {term_to_string(c): c for r in base for c in r.constraints}
        a, b, c, d = (terms[k] for k in sorted(terms)[:4])
        regions = [
            self._region_with(base[0], [a], 'a'),
            self._region_with(base[0], [a, b], 'ab'),
            self._region_with(base[0], [b, c], 'bc'),
            self._region_with(base[0], [b, d], 'bd'),
        ]

        with warnings.catch_warnings(record=True) as caught:
            warnings.simplefilter('always')
            groups = group_regions(regions)

        assert len(caught) == 1
        assert 'overlapping regions' in str(caught[0].message)
        self._assert_every_region_is_in_one_leaf(groups, regions)

    def test_textual_overlaps(
        self,
    ):
        """Equal and nested constraint sets; `true` counts as no constraint."""
        from imandrax_api_models.region_decomp import (
            _textual_overlaps,  # pyright: ignore[reportPrivateUsage]
        )

        ov = _textual_overlaps(
            [['a'], ['b', 'a'], ['a'], ['true'], ['c'], ['a', 'true']]
        )
        # `['a', 'true']` is not a copy of `['a']` (grouping tells them apart), but
        # they cover the same inputs: one containment pair.
        assert ov.identical == [(0, 2)]
        assert sorted(ov.contained) == [
            (0, 1), (0, 5), (3, 0), (3, 1), (3, 4), (3, 5), (5, 1),
        ]  # fmt: skip

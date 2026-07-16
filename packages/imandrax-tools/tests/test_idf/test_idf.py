import functools
from dataclasses import dataclass
from datetime import date
from pathlib import Path
from typing import Any, NamedTuple, Self, cast

import pytest
import yaml
from imandrax_api_models import DecomposeRes
from imandrax_api_models.client import get_imandrax_async_client
from imandrax_tools.idf.decomposition_tree import RegionNode, build_idf_tree
from imandrax_tools.idf.gen_iml import Guard, Target, gen_decomp_funs
from imandrax_tools.idf.iter_decomp import Step, iter_decomp
from inline_snapshot import external_file, snapshot

pytestmark = pytest.mark.skipif(
    date.today() < date(2026, 8, 1),
    reason='Disabled until August 1, 2026 because merge_src is missing in prod.',
)

CURR_DIR = Path(__file__).parent


class Row(NamedTuple):
    name: str
    sm: str
    tpl: str
    message_flows: list[str]


@functools.cache
def inputs():
    INPUTS_DIR = CURR_DIR / 'data' / 'idf_inputs'

    rows: list[Row] = []
    for f in INPUTS_DIR.iterdir():
        data = yaml.safe_load(f.read_text())
        rows.append(
            Row(
                name=f.name,
                sm=data['sm'],
                tpl=data['tpl'],
                message_flows=data['message_flows'],
            )
        )
    return rows


@dataclass
class SingleError:
    path: str | None
    error: Any

    @classmethod
    def from_exception(cls, exc: Exception, path: str | None = None) -> Self:
        return cls(path=path, error=exc)


type Validate = list[SingleError]


class Props:
    @staticmethod
    def is_not_root(node: RegionNode) -> bool:
        return (
            node.id != 0
            and node.raw_id != 'root'
            and len(node.constraints) != 0
            and node.invariant is not None
        )

    @staticmethod
    def root_xor_not(node: RegionNode) -> Validate:
        try:
            is_root = RegionNode.is_root(node)
            is_not_root = Props.is_not_root(node)
            assert is_root ^ is_not_root, (
                f'Node should be root or not root: {is_root=}, {is_not_root=}, {node=}'
            )
            return []
        except AssertionError as e:
            return [SingleError.from_exception(e)]

    @staticmethod
    def tree_step_match(
        decomp_res_by_step: list[DecomposeRes],
        region_tree: RegionNode | None = None,
    ) -> Validate:
        region_tree = region_tree or build_idf_tree(decomp_res_by_step)
        step_count_1 = len(decomp_res_by_step)
        # + 1 because step_idx is 0-indexed
        step_count_2 = max(map(lambda n: n.step_idx, region_tree.iter_node())) + 1
        try:
            assert step_count_1 == step_count_2, (
                f'Tree step mismatch: decomp_res_by_step={step_count_1}, region_tree={step_count_2}'
            )
            return []
        except AssertionError as e:
            return [SingleError.from_exception(e)]


def test_gen_decomp_funcs():
    message_flows = ['TPL.OnlyX', 'TPL.Anything', 'TPL.OnlyY']
    decomp_funs: list[tuple[Guard, Target]] = gen_decomp_funs(message_flows)
    g0, t0 = decomp_funs[0]
    assert g0.iml() == snapshot("""\

let guard_0 events =
  match List.nth 0 events with
  | Some event_i when
    TPL.concrete TPL.OnlyX event_i (SM.initial_state) ->
    true && SM.is_valid event_i (SM.initial_state)
  | _ -> false
""")
    assert t0.iml() == snapshot("""\

let target_0 events =
  match List.nth 0 events with
  | Some event_i ->
    SM.step event_i (SM.initial_state)
  | _ -> SM.initial_state
""")

    g_last, t_last = decomp_funs[-1]
    assert g_last.iml() == snapshot("""\

let guard_2 events =
  match List.nth 2 events with
  | Some event_i when
    TPL.concrete TPL.OnlyY event_i (target_1 events) ->
    guard_1 events && SM.is_valid event_i (target_1 events)
  | _ -> false
""")
    assert t_last.iml() == snapshot("""\

let target_2 events =
  match List.nth 2 events with
  | Some event_i ->
    SM.step event_i (target_1 events)
  | _ -> SM.initial_state
""")


@pytest.mark.asyncio
@pytest.mark.vcr
@pytest.mark.parametrize('name, sm, tpl, message_flows', [*inputs()])
async def test_build_tree(name: str, sm: str, tpl: str, message_flows: list[str]):
    async with get_imandrax_async_client() as c:
        _tag, steps_or_err = await iter_decomp(
            c,
            sm=sm,
            tpl=tpl,
            message_flows=message_flows,
        )
        assert _tag == 'left', f'Decomp failed: {steps_or_err}'
        steps_or_err = cast(list[Step], steps_or_err)
        decomp_res_by_step = [step['decomp_res'] for step in steps_or_err]

    root = build_idf_tree(decomp_res_by_step)
    assert root.to_string() == external_file(
        CURR_DIR / 'data/snapshots' / f'{name}.expected', format='.txt'
    )

    for node in root.iter_node():
        assert Props.root_xor_not(node) == []

    assert Props.tree_step_match(decomp_res_by_step, root) == []

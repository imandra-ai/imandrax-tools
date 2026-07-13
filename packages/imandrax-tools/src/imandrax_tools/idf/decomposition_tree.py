from __future__ import annotations

from collections.abc import Iterator, MutableMapping, MutableSequence
from dataclasses import dataclass
from typing import Never, Self

import imandrax_api.lib as xtype
import imandrax_api_models.pp.pretty as Pp
from imandrax_api_models import DecomposeRes
from imandrax_api_models.region_decomp._region import (
    Region,
    mir_regions_of_fun_decomp_artifact,
    mk_stablized_id_map,
)

type JSONValue = str | int | float | bool | None | JSONObject | JSONArray
type JSONObject = MutableMapping[str, JSONValue]
type JSONArray = MutableSequence[JSONValue]


@dataclass
class RegionNode:
    """Region node in the decomposition tree"""

    id: int
    """Stablized integer ID"""
    raw_id: str
    """Raw UUID"""
    step_idx: int
    """Step index (0-based, -1 for root)"""
    region_idx: int
    """Region index in the step"""
    # <Region_info>
    constraints: list[str]
    """Empty iff root"""
    invariant: str | None
    """None iff root"""
    model: dict[str, str] | str | None
    model_eval: str | None
    # </Region info>
    children: list[Self]
    other: JSONObject
    """Miscellaneous metadata"""

    @classmethod
    def mk_root(cls) -> Self:
        return cls(
            id=0,
            raw_id='root',
            step_idx=-1,
            region_idx=0,
            constraints=[],
            invariant=None,
            model=None,
            model_eval=None,
            children=[],
            other={},
        )

    @classmethod
    def from_region(
        cls,
        region: Region,
        stablized_id_map: dict[str, int],
        step_idx: int,
        region_idx: int,
    ) -> Self:
        raw_id = region.id
        id = stablized_id_map[raw_id]
        region_stat = region.stat()
        constraints = region_stat['constraints']
        invariant = region_stat['invariant']
        model = region_stat['model']
        model_eval = region_stat['model_eval']

        assert invariant is not None, 'Region should have a stringified invariant'
        return cls(
            id=id,
            raw_id=raw_id,
            step_idx=step_idx,
            region_idx=region_idx,
            constraints=constraints,
            invariant=invariant,
            model=model,
            model_eval=model_eval,
            children=[],
            other=region.other,
        )

    def iter_node(self) -> Iterator[Self]:
        yield self
        for child in self.children:
            yield from child.iter_node()

    def is_root(self: RegionNode) -> bool:
        return (
            self.id == 0
            and self.raw_id == 'root'
            and len(self.constraints) == 0
            and self.invariant is None
        )

    def _header_doc(self) -> Pp.Doc:
        """Tree node header"""
        if self.is_root():
            return Pp.Text('root')

        box_header = f'id={self.id}, step_idx={self.step_idx}'
        if len(self.children) == 0:
            box_header += ' [LEAF]'
        body = Pp.punctuate(
            Pp.hcat(Pp.text(','), Pp.line),
            [
                Pp.hcat(
                    Pp.text('invariant='),
                    Pp.python_quote(Pp.multiline_text(f'{self.invariant}')),
                ),
                Pp.hcat(
                    Pp.text('constraints='),
                    Pp.list_doc(
                        [
                            Pp.python_quote(Pp.multiline_text(c))
                            for c in self.constraints
                        ]
                    ),
                ),
            ],
        )
        return Pp.box_left(body, box_header, extra_guard_len=10)

    def to_doc(self) -> Pp.Doc:
        children = [child.to_doc() for child in self.children]
        return Pp.tree(self._header_doc(), children, extra_bar_len=2, add_arrow=True)

    def to_string(self) -> str:
        return Pp.pretty(88, self.to_doc())


def build_idf_tree(decomp_res_by_step: list[DecomposeRes]) -> RegionNode:
    decomp_art_by_step = [res.artifact for res in decomp_res_by_step]

    def raise_() -> Never:
        assert False

    mir_regions_by_step: list[list[xtype.Mir_Region_Region]] = [
        (
            mir_regions_of_fun_decomp_artifact(art)
            if art is not None  #
            else raise_()
        )
        for art in decomp_art_by_step
    ]

    regions_by_step: list[list[Region]] = [
        [Region.from_mir_region(mir_region) for mir_region in mir_regions]
        for mir_regions in mir_regions_by_step
    ]

    root = RegionNode.mk_root()
    # raw_id -> node
    nodes: dict[str, RegionNode] = {root.raw_id: root}

    stablized_id_map = mk_stablized_id_map(
        [region for regions in regions_by_step for region in regions],
        {root.raw_id: root.id},
    )

    for step_idx, regions in enumerate(regions_by_step):
        for region_idx, region in enumerate(regions):
            curr_node = RegionNode.from_region(
                region, stablized_id_map, step_idx=step_idx, region_idx=region_idx
            )
            if step_idx == 0:
                parent_node = root
            else:
                merge_src = region.other['merge_src']
                last_ids = [
                    prev_region.id for prev_region in regions_by_step[step_idx - 1]
                ]
                if merge_src not in last_ids:
                    raise ValueError(
                        'Parent (merge_src) not found in last step, '
                        f'{step_idx=}, {id=}, {merge_src=}'
                    )
                parent_node = nodes[merge_src]
            parent_node.children.append(curr_node)

            nodes[curr_node.raw_id] = curr_node
    return nodes[root.raw_id]

"""Types to be used by (possibly JS) visualization. Contract layer between JS and Python."""

from __future__ import annotations

from dataclasses import dataclass, replace
from typing import Self

from .decomposition_tree import RegionNode, build_idf_tree
from .iter_decomp import Step


@dataclass(frozen=True)
class StepView:
    message: str
    step_idx: int
    n_regions: int
    guard_iml: str
    target_iml: str

    name: str
    assuming: str
    basis: list[str]
    merge_with: str | None

    regions: list[tuple[RegionNodeViewNodeHeader, RegionNodeViewNodeBody]]

    @classmethod
    def from_step(cls, step: Step, step_idx: int) -> Self:
        return cls(
            message=step['message'],
            step_idx=step_idx,
            n_regions=0,
            guard_iml=step['guard_iml'],
            target_iml=step['target_iml'],
            name=step['name'],
            assuming=step['assuming'],
            basis=step['basis'],
            merge_with=step['merge_with'],
            regions=[],
        )

    def add_region(self, region: RegionNode) -> Self:
        header, body = region_view(region)
        return replace(
            self,
            n_regions=self.n_regions + 1,
            regions=self.regions + [(header, body)],
        )


@dataclass(frozen=True)
class RegionNodeViewNodeHeader:
    id: int
    is_root: bool
    is_leaf: bool


@dataclass(frozen=True)
class RegionNodeViewNodeBody:
    raw_id: str
    constraints: list[str]
    invariant: str | None
    model: dict[str, str] | str | None
    model_eval: str | None


@dataclass(frozen=True)
class Edge:
    src_id: int
    dst_id: int


@dataclass(frozen=True)
class View:
    steps: list[StepView]
    edges: list[Edge]

    @classmethod
    def from_steps(cls, steps: list[Step], idf_tree: RegionNode | None = None) -> Self:
        if idf_tree is None:
            decomp_res_by_step = [s['decomp_res'] for s in steps]
            idf_tree = build_idf_tree(decomp_res_by_step)
        step_map: dict[int, StepView] = {
            step_idx: StepView.from_step(step, step_idx)
            for step_idx, step in enumerate(steps)
        }
        edges: list[Edge] = []
        for node in idf_tree.iter_node():
            step_idx = node.step_idx
            edges += [Edge(src_id=node.id, dst_id=child.id) for child in node.children]
            step_map[step_idx] = step_map[step_idx].add_region(node)
        return cls(steps=list(step_map.values()), edges=edges)


def region_view(
    region: RegionNode,
) -> tuple[RegionNodeViewNodeHeader, RegionNodeViewNodeBody]:
    header = RegionNodeViewNodeHeader(
        id=region.id,
        is_root=region.is_root(),
        is_leaf=len(region.children) == 0,
    )
    body = RegionNodeViewNodeBody(
        raw_id=region.raw_id,
        constraints=region.constraints,
        invariant=region.invariant,
        model=region.model,
        model_eval=region.model_eval,
    )
    return header, body

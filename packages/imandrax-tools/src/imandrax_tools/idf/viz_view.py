"""
Types to be used by (possibly JS) visualization. Contract layer between JS and Python.
"""

from __future__ import annotations

from typing import Self

from pydantic import BaseModel

from .decomposition_tree import RegionNode, build_idf_tree
from .iter_decomp import Step


class RegionNodeView(BaseModel):
    """
    One region node in the decomposition graph.

    the header fields let the graph place and style the node
    card; the body fields feed the detail panel when the node is clicked.
    """

    # Header: shown on the node card / used for layout.
    id: int
    step_idx: int
    is_root: bool
    is_leaf: bool

    # Body: shown in the detail panel.
    raw_id: str
    constraints: list[str]
    invariant: str | None
    model: dict[str, str] | str | None
    model_eval: str | None

    @classmethod
    def from_region(cls, region: RegionNode) -> Self:
        return cls(
            id=region.id,
            step_idx=region.step_idx,
            is_root=region.is_root(),
            is_leaf=len(region.children) == 0,
            raw_id=region.raw_id,
            constraints=region.constraints,
            invariant=region.invariant,
            model=region.model,
            model_eval=region.model_eval,
        )


class StepView(BaseModel):
    """One decomposition step: its request metadata plus the regions it produced."""

    message: str
    step_idx: int
    n_regions: int
    guard_iml: str
    target_iml: str

    name: str
    assuming: str
    basis: list[str]
    merge_with: str | None

    regions: list[RegionNodeView]

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
        return self.model_copy(
            update={
                'n_regions': self.n_regions + 1,
                'regions': [*self.regions, RegionNodeView.from_region(region)],
            }
        )


class Edge(BaseModel):
    """A parent -> child link between region nodes."""

    src_id: int
    dst_id: int


class View(BaseModel):
    """The IDF widget entry model: the layered region graph the JS renders."""

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
            # The synthetic root (step_idx == -1) contributes edges but has no
            # StepView of its own; the JS side synthesizes its node card.
            if step_idx >= 0:
                step_map[step_idx] = step_map[step_idx].add_region(node)
        return cls(steps=list(step_map.values()), edges=edges)

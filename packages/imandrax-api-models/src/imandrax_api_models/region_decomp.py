"""Post-processing (Hierarchical groupping) for region decomposition."""

# %%
from __future__ import annotations

from dataclasses import dataclass
from functools import reduce
from typing import NoReturn, TypedDict

from imandrax_api.lib import RegionStr
from rich.console import Console, RenderableType
from rich.text import Text
from rich.tree import Tree


@dataclass
class RegionGroup:
    rg_constraints: list[str]
    rg_label_path: list[int]
    rg_region: RegionStr | None
    rg_children: list[RegionGroup]
    rg_weight: int

    def n_descendant_regions(self) -> int:
        return sum(c.n_descendant_regions() for c in self.rg_children)


def group_regions(regions: list[RegionStr]) -> list[RegionGroup]:
    """Group regions hierarchically based on constraints."""
    return _loop_group_regions([], [], regions)


def _loop_group_regions(
    idx_path: list[int], constraint_path: list[str], regions: list[RegionStr]
) -> list[RegionGroup]:
    def raise_(exc: BaseException) -> NoReturn:
        raise exc

    # all_constraints_with_dup
    constraints_: list[list[str]] = [
        (
            r.constraints_str
            if r.constraints_str
            else raise_(ValueError(f'Region {r} has no constraint string'))
        )
        for r in regions
    ]
    constraints: list[str] = [c for cs in constraints_ for c in cs]
    all_constraints_with_dup: list[str] = [
        c for c in constraints if c not in constraint_path
    ]

    # constraints_by_most_frequent
    def mk_counter(ls: list[str]) -> dict[str, int]:
        counter: dict[str, int] = {}

        def update_counter(s: str) -> None:
            curr_count = counter.get(s)
            if curr_count is None:
                counter[s] = 1
            else:
                counter[s] = curr_count + 1

        for s in ls:
            update_counter(s)
        return counter

    counter = mk_counter(all_constraints_with_dup)
    assoc_list: list[tuple[str, int]] = [(k, v) for (k, v) in counter.items()]
    sorted(assoc_list, key=lambda kv: kv[1], reverse=True)
    constraints_by_most_frequent: list[str] = [kv[0] for kv in assoc_list]

    # grouped: tuple[list[RegionGroup], list[RegionStr]]
    class Acc(TypedDict):
        groups: list[RegionGroup]
        regions: list[RegionStr]
        idx_path: list[int]
        constraint_path: list[str]

    def loop(
        # 'acc
        acc: Acc,
        # groups: list[RegionGroup],
        # regions: list[RegionStr],
        # idx_path: list[int],
        # constraint_path: list[str],
        # 'a
        konstraint: str,
    ) -> Acc:
        # ) -> tuple[list[RegionGroup], list[RegionStr], list[int], list[str]]:
        groups = acc['groups']
        regions = acc['regions']
        idx_path = acc['idx_path']
        constraint_path = acc['constraint_path']

        has: list[RegionStr] = []
        without: list[RegionStr] = []
        for r in regions:
            assert r.constraints_str, 'region has no constraint_str'
            if konstraint in r.constraints_str:
                has.append(r)
            else:
                without.append(r)
        i = len(groups) + 1
        if len(without) == 0 and (not (len(has) == 1)):
            new_idx_path = idx_path
        else:
            new_idx_path: list[int] = [i, *idx_path]
        new_constraint_path: list[str] = [konstraint, *constraint_path]

        if len(has) > 0:
            rg_children = _loop_group_regions(new_idx_path, new_constraint_path, has)
            group: RegionGroup
            if len(rg_children) == 1:
                group = rg_children[0]
            else:
                rg_constraints = new_constraint_path[::-1]
                rg_region: RegionStr | None
                if len(has) == 1:
                    rg_region = has[0]
                else:
                    rg_region = None
                rg_weight = len(has)
                group = RegionGroup(
                    rg_constraints=rg_constraints,
                    rg_region=rg_region,
                    rg_children=rg_children,
                    rg_label_path=new_idx_path,
                    rg_weight=rg_weight,
                )
            res = [group, *groups], without
        else:
            res = groups, without
        return Acc(
            groups=res[0],
            regions=res[1],
            idx_path=new_idx_path,
            constraint_path=new_constraint_path,
        )

    init = Acc(
        groups=[], regions=regions, idx_path=idx_path, constraint_path=constraint_path
    )
    return reduce(loop, constraints_by_most_frequent, init)['groups']


def renderable_of_rg(rg: RegionGroup):
    buf = ''
    buf += f'{rg.rg_label_path}'
    buf += f'; {len(rg.rg_constraints)} constraints'
    n_children = len(rg.rg_children)
    if n_children != 0:
        buf += f', {n_children} children'
    return buf


def rich_tree_of_groups(region_groups: list[RegionGroup]) -> Tree:

    def add_children(tree: Tree, region_groups: list[RegionGroup]) -> Tree:
        top_regions: list[tuple[Tree, RegionGroup]] = [
            (tree.add(renderable_of_rg(rg)), rg) for rg in region_groups
        ]

        for tree, rg in top_regions:
            add_children(tree, rg.rg_children)
        return tree

    root = Tree('Region Decomposition')

    return add_children(root, region_groups)


def str_of_renderables(
    *renderables: RenderableType | object,
    plain: bool = True,
) -> str:
    console = Console(
        record=True,
        width=80,
        color_system='standard',
        force_terminal=False,
        force_interactive=False,
        force_jupyter=False,
    )
    with console.capture() as capture:
        for renderable in renderables:
            console.print(renderable)
    exported_text = capture.get()
    if plain:
        return Text.from_ansi(exported_text).plain
    else:
        return exported_text

"""Post-processing (hierarchical grouping) for region decomposition."""

from __future__ import annotations

from collections.abc import Callable
from functools import reduce
from typing import NoReturn, Self, TypedDict

from devtools import pformat
from imandrax_api.lib import RegionStr
from pydantic import BaseModel, Field, model_validator

from imandrax_api_models.proto_models import DecomposeRes

from .icicle_widget import mk_icicle_widget_html


class EnrichedDecomposeRes(DecomposeRes):
    """A `DecomposeRes` augmented with hierarchical region grouping."""

    region_groups: list[RegionGroup] = Field(
        default_factory=list,
        description='Region groups grouped by constraints, containing child groups recursively. Empty when no regions are available (decomposition error).',
    )

    @model_validator(mode='after')
    def _populate_region_groups(self) -> Self:
        if not self.region_groups and self.regions_str:
            self.region_groups = group_regions(self.regions_str)
        return self

    @classmethod
    def from_decomp_res(cls, v: DecomposeRes) -> EnrichedDecomposeRes:
        return cls.model_validate(v.model_dump())

    def to_tree_str(
        self,
        *,
        depth_limit: int | None = None,
        summarize: Callable[[RegionGroup], str] | None = None,
    ) -> str:
        if self.errors:
            return pformat(self.errors, indent=2)
        return render_region_groups(
            self.region_groups, depth_limit=depth_limit, tree_repr=summarize
        )

    def _repr_html_(self) -> str:
        if self.errors:
            return f'<pre>{pformat(self.errors, indent=2)}</pre>'
        return mk_icicle_widget_html(self.region_groups)


type JSONValue = str | int | float | bool | None | JSONObject | JSONArray
type JSONObject = dict[str, JSONValue]
type JSONArray = list[JSONValue]


class RegionGroup(BaseModel):
    """
    A hierarchical group of regions sharing constraints.

    Attributes:
        constraints:
            Full accumulated constraint path from root to this node (root-first).
            `constraints[-1]` is the constraint introduced at this node's own level.
        label_path:
            Positional index path from root to this node (root-first, 1-indexed).
            Each element is the sibling index at that depth. Displayed as e.g. `1.2.3`.
            Levels where a constraint applies to all regions are skipped, so the path
            length may be shorter than the tree depth.
        region:
            The concrete region, if this group contains exactly one.
        children:
            Sub-groups under this node.
        weight:
            Number of regions in the `has` partition at this node's level.

    """

    constraints: list[str]
    label_path: list[int]
    region: RegionStr | None = None
    children: list[RegionGroup] = []
    weight: int

    def n_regions(self) -> int:
        """Total regions in this subtree, including self."""
        return 1 + sum(c.n_regions() for c in self.children)

    def n_descendant_regions(self) -> int:
        """Total descendant regions in this subtree, excluding self."""
        return self.n_regions() - 1

    def n_leaf_regions(self) -> int:
        """Total leaf regions in this subtree, counting self if no children."""
        if not self.children:
            return 1
        return sum(c.n_leaf_regions() for c in self.children)

    def describe(self) -> JSONObject:
        d: JSONObject = {}
        d['label_path'] = '.'.join(map(str, self.label_path))
        d['constraints'] = self.constraints
        d['introduced_constraint'] = self.constraints[-1] if self.constraints else ''
        d['weight'] = self.weight
        d['n_children_regions'] = len(self.children)
        d['n_descendant_regions'] = self.n_descendant_regions()
        d['n_leaf_regions'] = self.n_leaf_regions()
        if (r := self.region) is not None:
            d['invariant'] = r.invariant_str
            d['example_input'] = r.model_str
            d['example_output'] = r.model_eval_str
        return d

    def repr_line(self) -> str:
        """One-line representation of the region group."""
        d = self.describe()
        parts: list[str] = []
        parts.append(f'[{d["label_path"]}]')
        parts.append(f"new_constraint='{d['introduced_constraint']}'")
        if d.get('invariant'):
            parts.append(f"invariant='{d['invariant']}'")
        n_leaf_regions = d['n_leaf_regions']
        if n_leaf_regions != 1:
            parts.append(f'n_leaf_regions={n_leaf_regions}')
        else:
            parts.append('is_leaf=True')
        return ' '.join(parts)


def group_regions(regions: list[RegionStr]) -> list[RegionGroup]:
    """Group regions hierarchically based on constraints."""
    return _loop_group_regions([], [], regions)


# Tree rendering
# ====================


def render_region_groups(
    groups: list[RegionGroup],
    *,
    depth_limit: int | None = None,
    tree_repr: Callable[[RegionGroup], str] | None = None,
) -> str:
    """Render a forest of `RegionGroup`s as a tree in text."""
    tree_repr_ = tree_repr or RegionGroup.repr_line
    lines: list[str] = []
    for i, group in enumerate(groups):
        is_last = i == len(groups) - 1
        _tree_lines(
            lines,
            group,
            prefix='',
            is_last=is_last,
            depth_limit=depth_limit,
            summarize=tree_repr_,
        )
    return '\n'.join(lines)


def _tree_lines(
    lines: list[str],
    group: RegionGroup,
    *,
    prefix: str,
    is_last: bool,
    depth_limit: int | None,
    summarize: Callable[[RegionGroup], str],
) -> None:
    connector = '└── ' if is_last else '├── '
    lines.append(f'{prefix}{connector}{summarize(group)}')

    child_prefix = prefix + ('    ' if is_last else '│   ')
    if depth_limit is not None and depth_limit <= 0 and group.children:
        lines.append(
            f'{child_prefix}└── ... ({len(group.children)} children, {group.n_descendant_regions()} descendants)'
        )
        return
    next_limit = None if depth_limit is None else depth_limit - 1
    for i, child in enumerate(group.children):
        _tree_lines(
            lines,
            child,
            prefix=child_prefix,
            is_last=i == len(group.children) - 1,
            depth_limit=next_limit,
            summarize=summarize,
        )


# Grouping algorithm
# ====================


def _loop_group_regions(
    idx_path: list[int], constraint_path: list[str], regions: list[RegionStr]
) -> list[RegionGroup]:
    """
    Recursively group regions by shared constraints.

    At each level, collects all constraints from the remaining regions (excluding
    those already in ``constraint_path``), then iterates over them by frequency.
    For each constraint, regions that contain it are split into a sub-group and
    grouped recursively.

    Preconditions:
    - Every region in `regions` must have a non-empty `constraints_str`.
    - `constraint_path` contains constraints already consumed by ancestor levels;
      these are excluded from grouping at this level.
    - `idx_path` is built in reverse (deepest-first); reversed to root-first
      when stored in `RegionGroup.label_path`.

    Postconditions:
    - Every input region appears in exactly one output `RegionGroup` (partition).
    - `RegionGroup.constraints` is the full accumulated constraint path from root
      to that node (root-first). `constraints[-1]` is the constraint introduced
      at that node's own level (its "own constraint").
    - `RegionGroup.label_path` is root-first, 1-indexed. A level is skipped
      (no index appended) when all remaining regions share the constraint and
      there is more than one — i.e. the constraint doesn't discriminate.
    - If recursion yields a single child group, that child is promoted (returned
      directly) rather than wrapped, collapsing trivial intermediate nodes.

    Invariants (across `reduce`/`loop` iterations):
    - `acc['regions']` shrinks monotonically: each iteration moves regions into
      `has` (grouped) or keeps them in `without` (remaining).
    - `acc['idx_path']` and `acc['constraint_path']` stay constant across
      iterations (they describe this level). The current `konstraint` is only
      prepended for the `has` branch's recursion, never carried into later
      iterations, which process `without` regions that lack `konstraint`.
    - `acc['groups']` only grows: new groups are prepended when `has` is non-empty.
    """

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
    # Most frequent first, ties broken alphabetically.
    assoc_list: list[tuple[str, int]] = sorted(
        counter.items(), key=lambda kv: (-kv[1], kv[0])
    )
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
                    constraints=rg_constraints,
                    region=rg_region,
                    children=rg_children,
                    label_path=new_idx_path[::-1],
                    weight=rg_weight,
                )
            res = [group, *groups], without
        else:
            res = groups, without
        # `idx_path` / `constraint_path` describe this level and must stay
        # constant across reduce iterations. Only the `has` branch (recursed
        # above) gets the extended `new_idx_path` / `new_constraint_path`; the
        # `without` regions handled by later iterations do not contain
        # `konstraint`, so it must not leak into their path.
        return Acc(
            groups=res[0],
            regions=res[1],
            idx_path=idx_path,
            constraint_path=constraint_path,
        )

    init = Acc(
        groups=[], regions=regions, idx_path=idx_path, constraint_path=constraint_path
    )
    return reduce(loop, constraints_by_most_frequent, init)['groups'][::-1]

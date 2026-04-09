"""Post-processing (Hierarchical groupping) for region decomposition."""

from __future__ import annotations

import itertools
from collections.abc import Callable
from dataclasses import dataclass
from functools import partial, reduce
from typing import Any, Literal, NoReturn, Protocol, Self, TypedDict

import yaml
from devtools import pformat
from imandrax_api.lib import RegionStr

from imandrax_api_models.proto_models import DecomposeRes, Error
from imandrax_api_models.yaml_utils import str_representer


@dataclass
class HumDecomposeRes:
    """Human readable decomp result"""

    _variants: (
        tuple[Literal['Success'], list[RegionGroup]]
        | tuple[Literal['Fail'], list[Error]]
    )

    @classmethod
    def mk_success(cls, pl: list[RegionGroup]) -> Self:
        return cls(_variants=('Success', pl))

    @classmethod
    def mk_fail(cls, pl: list[Error]) -> Self:
        return cls(_variants=('Fail', pl))

    @classmethod
    def from_decomp_res(cls, v: DecomposeRes) -> HumDecomposeRes:
        return hum_of_decomp_res(v)

    @classmethod
    def from_regions(cls, regions: list[RegionStr]) -> HumDecomposeRes:
        groups = group_regions(regions)
        return HumDecomposeRes.mk_success(groups)

    def to_tree_str(
        self,
        *,
        depth_limit: int | None = None,
        summarize: RegionGroupSummarizer | None = None,
    ) -> str:
        match self._variants:
            case ('Fail', errs):
                return pformat(errs, indent=2)
            case ('Success', groups):
                summarize_ = summarize or default_region_group_summary
                lines: list[str] = []
                for i, group in enumerate(groups):
                    is_last = i == len(groups) - 1
                    _tree_lines(
                        lines,
                        group,
                        prefix='',
                        is_last=is_last,
                        depth_limit=depth_limit,
                        summarize=summarize_,
                    )
                return '\n'.join(lines)

    def summary(self) -> dict[str, str | int] | None:
        match self._variants:
            case ('Fail', _):
                return None
            case ('Success', groups):
                n_regions = sum([rg.n_regions() for rg in groups])
                max_depth = _max_depth_of_groups(groups)

                return {'n_regions': n_regions, 'max_depth': max_depth}

    @staticmethod
    def dumper_func(depth_limit: int | None = None) -> Callable[..., str]:
        dumper_cls = mk_dumper(depth_limit=depth_limit)
        return partial(
            yaml.dump,
            Dumper=dumper_cls,
            default_flow_style=False,
            sort_keys=False,
        )

    def to_dict_hierarchical(
        self,
        depth_limit: int | None = None,
    ) -> dict[str, Any]:
        match self._variants:
            case ('Fail', errs):
                return {'errors': errs}
            case ('Success', groups):
                return {'summary': self.summary(), 'region_groups': groups}

    def to_dict_flat(
        self,
    ) -> dict[str, Any]:
        match self._variants:
            case ('Fail', errs):
                return {'errors': errs}
            case ('Success', groups):
                indexed_groups = indexed_of_region_groups(groups)
                return {'summary': self.summary(), 'region_groups': indexed_groups}


def hum_of_decomp_res(decomp_res: DecomposeRes) -> HumDecomposeRes:
    if decomp_res.err is None:
        regions = decomp_res.regions_str
        assert regions is not None
        groups = group_regions(regions)
        return HumDecomposeRes.mk_success(groups)

    else:
        return HumDecomposeRes.mk_fail(decomp_res.errors)


@dataclass
class RegionGroup:
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
    region: RegionStr | None
    children: list[RegionGroup]
    weight: int

    def n_regions(self) -> int:
        """Total regions in this subtree, including self."""
        return 1 + sum(c.n_regions() for c in self.children)

    def n_descendant_regions(self) -> int:
        return self.n_regions() - 1


def group_regions(regions: list[RegionStr]) -> list[RegionGroup]:
    """Group regions hierarchically based on constraints."""
    return _loop_group_regions([], [], regions)


@dataclass
class IndexedRegionGroup:
    """Equivalent to RegionGroup, but in flat structure."""

    id: str
    constraints: list[str]
    label_path: list[int]
    region: RegionStr | None
    children: list[str]
    weight: int
    depth: int  # 1-indexed depth


_counter = itertools.count()


def _gen_id() -> str:
    return str(next(_counter))


def indexed_of_region_groups(groups: list[RegionGroup]) -> list[IndexedRegionGroup]:
    def loop(
        group_and_id_lst: list[tuple[RegionGroup, str]],
        acc: list[IndexedRegionGroup],
        depth: int,
    ) -> None:
        for rg, id in group_and_id_lst:
            c_ids = [_gen_id() for _ in rg.children]

            irg = IndexedRegionGroup(
                id=id,
                constraints=rg.constraints,
                label_path=rg.label_path,
                region=rg.region,
                children=c_ids,
                weight=rg.weight,
                depth=depth,
            )
            acc.append(irg)

            children_group_and_id_lst = list(zip(rg.children, c_ids, strict=True))
            loop(children_group_and_id_lst, acc, depth + 1)

    initial_ids = [_gen_id() for _ in groups]
    group_and_id_lst = list(zip(groups, initial_ids, strict=True))
    acc: list[IndexedRegionGroup] = []
    loop(group_and_id_lst, acc, 1)
    return acc[::-1]


def _max_depth_of_groups(groups: list[RegionGroup]) -> int:
    init: int = 1

    def loop(groups: list[RegionGroup]):
        label_lengths: list[int] = [len(rg.label_path) for rg in groups]
        nonlocal init
        init = max(init, (max(label_lengths) if label_lengths else 0))
        for rg in groups:
            loop(rg.children)

    loop(groups)
    return init


class RegionGroupSummarizer(Protocol):
    def __call__(self, group: RegionGroup) -> str: ...


def default_region_group_summary(group: RegionGroup) -> str:
    label = '.'.join(map(str, group.label_path))
    # rg_constraints is the full path from root; [-1] is this node's own constraint.
    constraint = group.constraints[-1] if group.constraints else '?'
    invariant: str | None = None
    if (region := group.region) is not None:
        invariant = region.invariant_str
    parts = [
        f'[{label}]',
        f'{constraint=}',
        f'{invariant=}',
        f'(w={group.weight}, n_children={len(group.children)}, n_descendants={group.n_descendant_regions()})',
    ]
    return ' '.join(parts)


def _tree_lines(
    lines: list[str],
    group: RegionGroup,
    *,
    prefix: str,
    is_last: bool,
    depth_limit: int | None,
    summarize: RegionGroupSummarizer,
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
    - `acc['constraint_path']` grows by one element per iteration (the current
      `konstraint`), regardless of whether any regions matched.
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
                    constraints=rg_constraints,
                    region=rg_region,
                    children=rg_children,
                    label_path=new_idx_path[::-1],
                    weight=rg_weight,
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
    return reduce(loop, constraints_by_most_frequent, init)['groups'][::-1]


# YAML Dump
# ====================


def _region_str_representer(dumper: yaml.Dumper, data: RegionStr) -> yaml.Node:
    mapping: dict[str, object] = {}
    if data.constraints_str is not None:
        mapping['constraints'] = data.constraints_str
    if data.invariant_str is not None:
        mapping['invariant'] = data.invariant_str
    if data.model_str is not None:
        mapping['model'] = data.model_str
    if data.model_eval_str is not None:
        mapping['model_eval'] = data.model_eval_str
    return dumper.represent_mapping('!Region', mapping)


def _region_group_representer(
    dumper: yaml.Dumper, data: RegionGroup, *, depth_limit: int | None = None
) -> yaml.Node:
    mapping: dict[str, object] = {}

    mapping['label_path'] = '.'.join(map(str, data.label_path))
    mapping['weight'] = data.weight

    mapping['constraints'] = data.constraints
    mapping['introduced_constraint'] = data.constraints[-1]
    if (region := data.region) is not None:
        mapping['invariant'] = region.invariant_str
        mapping['example_input'] = region.model_str
        mapping['example_output'] = region.model_eval_str

    mapping['n_children_regions'] = len(data.children)
    mapping['n_descendant_regions'] = data.n_descendant_regions()
    if data.children:
        if depth_limit is None or depth_limit <= 0:
            mapping['children'] = data.children
    # return dumper.represent_mapping('tag:yaml.org,2002:map', mapping)
    return dumper.represent_mapping('!RegionGroup', mapping)


def _indexed_region_group_representer(
    dumper: yaml.Dumper, data: IndexedRegionGroup
) -> yaml.Node:
    mapping: dict[str, object] = {}

    mapping['id'] = data.id
    mapping['label_path'] = '.'.join(map(str, data.label_path))
    mapping['depth'] = data.depth
    mapping['weight'] = data.weight

    mapping['constraints'] = data.constraints
    mapping['introduced_constraint'] = data.constraints[-1]
    if (region := data.region) is not None:
        mapping['invariant'] = region.invariant_str
        mapping['example_input'] = region.model_str
        mapping['example_output'] = region.model_eval_str

    mapping['n_children_regions'] = len(data.children)
    mapping['children'] = data.children
    return dumper.represent_mapping('tag:yaml.org,2002:map', mapping)
    # return dumper.represent_mapping('!IndexedRegionGroup', mapping)


def mk_dumper(*, depth_limit: int | None = None) -> type[yaml.Dumper]:
    class RegionDecompDumper(yaml.Dumper):
        pass

    RegionDecompDumper.add_representer(
        RegionStr,
        _region_str_representer,
    )

    def _rg_representer(dumper: yaml.Dumper, data: RegionGroup) -> yaml.Node:
        next_limit = None if depth_limit is None else depth_limit - 1
        return _region_group_representer(dumper, data, depth_limit=next_limit)

    RegionDecompDumper.add_representer(RegionGroup, _rg_representer)
    RegionDecompDumper.add_representer(
        IndexedRegionGroup, _indexed_region_group_representer
    )
    RegionDecompDumper.add_representer(str, str_representer)
    return RegionDecompDumper

"""Post-processing (Hierarchical groupping) for region decomposition."""

from __future__ import annotations

from dataclasses import dataclass
from functools import reduce
from typing import NoReturn, Protocol, Self, TypedDict

import yaml
from imandrax_api.lib import RegionStr

from imandrax_api_models.yaml_utils import str_representer

__all__ = (
    'RegionGroup',
    'group_regions',
    'dump_region_groups_yaml',
    'GroupedRegionDecomposition',
)


@dataclass
class RegionGroup:
    constraints: list[str]
    label_path: list[int]
    region: RegionStr | None
    children: list[RegionGroup]
    weight: int

    def n_descendant_regions(self) -> int:
        return sum(c.n_descendant_regions() for c in self.children)


def group_regions(regions: list[RegionStr]) -> list[RegionGroup]:
    """Group regions hierarchically based on constraints."""
    return _loop_group_regions([], [], regions)


@dataclass
class GroupedRegionDecomposition:
    groups: list[RegionGroup]

    @classmethod
    def from_groups(cls, groups: list[RegionGroup]) -> Self:
        return cls(groups=groups)

    @classmethod
    def from_regions(cls, regions: list[RegionStr]) -> Self:
        return cls.from_groups(group_regions(regions))

    def to_tree_str(
        self,
        *,
        depth_limit: int | None = None,
        summarize: RegionGroupSummarizer | None = None,
    ) -> str:
        summarize_ = summarize or default_region_group_summary
        lines: list[str] = []
        for i, group in enumerate(self.groups):
            is_last = i == len(self.groups) - 1
            _tree_lines(
                lines,
                group,
                prefix='',
                is_last=is_last,
                depth_limit=depth_limit,
                summarize=summarize_,
            )
        return '\n'.join(lines)

    def to_yaml_str(
        self,
        depth_limit: int | None = None,
    ) -> str:
        return dump_region_groups_yaml(self.groups, depth_limit=depth_limit)


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
        f'(w={group.weight}, children={len(group.children)}, descendants={group.n_descendant_regions()})',
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

    Each resulting ``RegionGroup.rg_constraints`` is the **full accumulated path**
    of constraints from the root down to that node (chronological order). Therefore
    ``rg_constraints[-1]`` is the constraint introduced at that node's own level,
    while earlier elements are inherited from ancestors.
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
                    label_path=new_idx_path,
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
    return reduce(loop, constraints_by_most_frequent, init)['groups']


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


def _make_dumper(*, depth_limit: int | None = None) -> type[yaml.Dumper]:
    class RegionGroupsDumper(yaml.Dumper):
        pass

    RegionGroupsDumper.add_representer(
        RegionStr,
        _region_str_representer,
    )

    def _rg_representer(dumper: yaml.Dumper, data: RegionGroup) -> yaml.Node:
        next_limit = None if depth_limit is None else depth_limit - 1
        return _region_group_representer(dumper, data, depth_limit=next_limit)

    RegionGroupsDumper.add_representer(RegionGroup, _rg_representer)
    RegionGroupsDumper.add_representer(str, str_representer)
    return RegionGroupsDumper


def dump_region_groups_yaml(
    groups: list[RegionGroup],
    *,
    depth_limit: int | None = None,
) -> str:
    """
    Dump region groups to YAML.

    Args:
        groups: The region groups to dump.
        depth_limit: Max depth of children to expand. None means unlimited.

    """
    dumper_cls = _make_dumper(depth_limit=depth_limit)
    return yaml.dump(
        groups, Dumper=dumper_cls, default_flow_style=False, sort_keys=False
    )

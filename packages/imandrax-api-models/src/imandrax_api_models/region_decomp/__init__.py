"""Post-processing (hierarchical grouping) for region decomposition."""

from __future__ import annotations

from collections.abc import Callable, Mapping, Sequence
from dataclasses import asdict, dataclass, fields, is_dataclass
from functools import reduce
from typing import Annotated, Any, Self, TypedDict

import imandrax_api.lib as xtype
from devtools import pformat
from pydantic import BaseModel, Field, PlainSerializer, model_validator

from imandrax_api_models.pp.pretty import pretty
from imandrax_api_models.pp.term_formatter import term2doc
from imandrax_api_models.pp.xtype import to_string as xtype_to_string
from imandrax_api_models.proto_models import Art, DecomposeRes

type AssocList[T] = list[tuple[str, T]]
type JSONValue = (
    str | int | float | bool | None | Mapping[str, JSONValue] | Sequence[JSONValue]
)
type JSONObject = dict[str, JSONValue]
type JSONArray = list[JSONValue]

_PREFER_INVARIANT_FROM_PP_OVER_FROM_STRING_RESULT = True
"""Region group's constraints (list[str]) are from pp, so we align invariant's representation for leaf nodes"""


def term_to_string(t: xtype.Mir_Term) -> str:
    return pretty(88, term2doc(t))


def eq_term_with_pp(left: xtype.Mir_Term, right: xtype.Mir_Term) -> bool:
    return term_to_string(left) == term_to_string(right)


class StringResult(TypedDict):
    constraints: list[str]
    invariant: str
    model: dict[str, str] | None
    model_eval: str | None


# TODO: now we are ready to replace RegionStr with Region completely in simple_api,py
@dataclass
class Region:
    """
    A region represented by string forms of its constraints.

    Grouping keys off `constraints_str`; `data` supplies display fields.
    """

    mir_region: xtype.Mir_Region_Region
    string_result: StringResult | None
    id: str
    other: dict[str, Any]

    @property
    def constraints(self) -> list[xtype.Mir_Term_term]:
        return self.mir_region.constraints

    def to_jsonable(self) -> JSONObject:
        dct = asdict(self)
        dct.pop('mir_region')
        return dct

    def stat(self) -> JSONObject:
        out: JSONObject = {}
        out['constraints'] = [term_to_string(c) for c in self.constraints]
        out |= self.non_group_stat()

        return out

    def non_group_stat(self) -> JSONObject:
        out: JSONObject = {}

        # TODO: include id?
        out['invariant'] = term_to_string(self.mir_region.invariant)
        if self.string_result is not None:
            if not _PREFER_INVARIANT_FROM_PP_OVER_FROM_STRING_RESULT:
                out['invariant'] = self.string_result['invariant']
            if self.string_result['model'] is not None:
                out['model'] = self.string_result['model']
            if self.string_result['model_eval'] is not None:
                out['model_eval'] = self.string_result['model_eval']
        else:
            out['invariant'] = term_to_string(self.mir_region.invariant)

            # If model is not set (in the case of `string_results=False`)
            # try to get it from the feasible status
            status = self.mir_region.status
            match status:
                case xtype.Common_Region_status_Feasible(arg=model):
                    out['model'] = xtype_to_string(model)
                case _:
                    pass

        out |= self.other
        return out

    # @classmethod
    # def from_region_str(
    #     cls, region_str: RegionStr, mir_region: xtype.Mir_Region_Region
    # ) -> Self:
    #     meta_str = {
    #         'invariant_str': region_str.invariant_str,
    #         'model_str': region_str.model_str,
    #         'model_eval_str': region_str.model_eval_str,
    #     }
    #     return cls(
    #         constraints_str=region_str.constraints_str,
    #         mir_region=mir_region,
    #         data=meta_str,
    #     )

    @classmethod
    def from_mir_region(cls, region: xtype.Mir_Region_Region) -> Self:
        id, string_result, other = _parse_region(region)

        return cls(
            mir_region=region,
            id=id,
            string_result=string_result,
            other=other,
        )


def _parse_region(
    region: xtype.Mir_Region_Region,
) -> tuple[str, StringResult | None, JSONObject]:
    """
    A local replacement for xtype.unwrap_region_str.

    `region.meta` is an assoc-list whose values are wrapped in
    `Common_Region_meta_*` variants (`String`, `Assoc`, `List`, ...); the raw
    payload lives on their `.arg`. Mirror `xtype.unwrap_region_str` and unwrap
    at every level.
    """

    def src_of_meta_str(
        m: xtype.Common_Region_meta_String[xtype.Mir_Term_term] | Any,
    ) -> str:
        assert isinstance(m, xtype.Common_Region_meta_String)
        return m.arg

    meta = dict(region.meta)

    id = src_of_meta_str(meta.get('id'))

    other: JSONObject = {}
    if (merge_src := meta.get('merge_src')) is not None:
        other['merge_src'] = src_of_meta_str(merge_src)
    if (merge_tgt := meta.get('merge_tgt')) is not None:
        other['merge_tgt'] = src_of_meta_str(merge_tgt)
    other['status'] = type(region.status).__name__.removeprefix('Common_Region_status_')

    meta_str = meta.get('str')
    if meta_str is None:
        return id, None, other

    assert isinstance(meta_str, xtype.Common_Region_meta_Assoc)
    meta_str_dict = dict(meta_str.arg)

    constraints_ = meta_str_dict.get('constraints')
    assert isinstance(constraints_, xtype.Common_Region_meta_List)
    constraints: list[str] = [src_of_meta_str(c) for c in constraints_.arg]

    invariant = src_of_meta_str(meta_str_dict.get('invariant'))

    model: dict[str, str] | None = None
    if (model_meta := meta_str_dict.get('model')) is not None:
        assert isinstance(model_meta, xtype.Common_Region_meta_Assoc)
        model = {k: src_of_meta_str(v) for (k, v) in model_meta.arg}

    model_eval: str | None = None
    if (model_eval_meta := meta_str_dict.get('model_eval')) is not None:
        model_eval = src_of_meta_str(model_eval_meta)

    string_res = StringResult(
        constraints=constraints,
        invariant=invariant,
        model=model,
        model_eval=model_eval,
    )
    return id, string_res, other


class EnrichedDecomposeRes(DecomposeRes):
    """A `DecomposeRes` augmented with hierarchical region grouping."""

    region_groups: list[RegionGroup] = Field(
        default_factory=lambda: [],
        description='Region groups grouped by constraints, containing child groups recursively. Empty when no regions are available (decomposition error).',
    )

    @model_validator(mode='after')
    def populate_region_groups(self) -> Self:
        if self.region_groups or self.artifact is None:
            return self
        mir_regions = _mir_regions_of_fun_decomp_artifact(self.artifact)

        # if self.regions_str:
        #     regions = [
        #         Region.from_region_str(r_str, mir_r)
        #         for r_str, mir_r in zip(self.regions_str, mir_regions)
        #     ]
        # else:
        #     # `regions_str` is only populated when the decomposition embedded
        #     # string results in the artifact meta (`string_results=True`). When
        #     # it didn't, fall back to the raw regions in the decoded artifact,
        #     # stringifying their constraint terms via `repr`.
        #     regions = [Region.from_mir_region(mir_r) for mir_r in mir_regions]

        regions = [Region.from_mir_region(mir_r) for mir_r in mir_regions]

        if regions:
            self.region_groups = group_regions(regions)
        return self

    @classmethod
    def from_decomp_res(cls, v: DecomposeRes) -> EnrichedDecomposeRes:
        return cls.model_validate(v.model_dump())

    def regions_with_group_info(self) -> JSONArray:
        """Leaf region groups (concrete regions) with hierarchical grouping info."""
        leaf_groups = get_leaf_groups(self.region_groups)
        ds: JSONArray = []
        for leaf_group in leaf_groups:
            d: JSONObject = {}
            assert leaf_group.region is not None, 'Leaf group must be concrete'
            d['label_path'] = '.'.join(map(str, leaf_group.label_path))
            d['weight'] = leaf_group.weight
            d |= leaf_group.region.stat()
            ds.append(d)
        return ds

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


class RegionGroup(BaseModel):
    """
    A hierarchical group of regions sharing constraints.
    """

    # Obtained via pp
    constraints: list[str] = Field(
        description=(
            'Full accumulated constraint path from root to this node (root-first).'
            "`constraints[-1]` is the constraint introduced at this node's own level."
        )
    )
    label_path: list[int] = Field(
        description=(
            'Positional index path from root to this node (root-first, 1-indexed).'
            'Each element is the sibling index at that depth. Displayed as e.g. `1.2.3`.'
            'Levels where a constraint applies to all regions are skipped, so the path'
            'length may be shorter than the tree depth.'
        )
    )
    weight: int = Field(
        description="Number of regions in the partition at this node's level."
    )
    region: Annotated[
        Region | None,
        PlainSerializer(
            lambda x: x.to_jsonable() if x is not None else None, return_type=JSONObject
        ),
    ] = Field(
        default=None,
        description='The concrete region. Present iff at leaf nodes.',
    )
    children: list[RegionGroup] = Field(
        default_factory=lambda: [], description='Sub-groups under this node.'
    )

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
        d['n_leaf_regions'] = self.n_leaf_regions()
        if (r := self.region) is not None:
            d['region'] = r.non_group_stat()
        return d

    def to_json_dict(self) -> JSONObject:
        """Serialize to a d3-hierarchy-compatible dict, recursing into children."""
        d = self.describe()
        if self.children:
            d['children'] = [c.to_json_dict() for c in self.children]
        return d

    def repr_line(self) -> str:
        """One-line representation of the region group."""
        d = self.describe()
        parts: list[str] = []
        parts.append(f'[{d["label_path"]}]')
        parts.append(f"new_constraint='{d['introduced_constraint']}'")
        region_info = d.get('region', None)
        if region_info is not None:
            parts.append(f"invariant='{region_info['invariant']}'")  # type: ignore

        n_leaf_regions = d['n_leaf_regions']
        if n_leaf_regions != 1:
            parts.append(f'n_leaf_regions={n_leaf_regions}')
        else:
            parts.append('is_leaf=True')
        return ' '.join(parts)


def get_leaf_groups(groups: list[RegionGroup]) -> list[RegionGroup]:
    leaves: list[RegionGroup] = []
    for group in groups:
        if not group.children:
            leaves.append(group)
        else:
            leaves.extend(get_leaf_groups(group.children))
    return leaves


def group_regions(
    regions: Sequence[Region],
    eq_term: Callable[
        [xtype.Mir_Term_term, xtype.Mir_Term_term], bool
    ] = eq_term_with_pp,
) -> list[RegionGroup]:
    """Group regions hierarchically based on constraints."""
    return _loop_group_regions([], [], regions, eq_term)


def rgs_of_mir_fun_decomp(fun_decomp: xtype.Mir_Fun_decomp) -> list[RegionGroup]:

    # regions_str: list[RegionStr] | None = None
    # try:
    #     regions_str = [xtype.unwrap_region_str(r) for r in fun_decomp.regions]
    # except Exception:
    #     pass

    # if regions_str is None:
    #     regions = [Region.from_mir_region(r) for r in fun_decomp.regions]
    # else:
    #     regions = [
    #         Region.from_region_str(r_str, mir_r)
    #         for r_str, mir_r in zip(regions_str, fun_decomp.regions)
    #     ]

    regions = [Region.from_mir_region(r) for r in fun_decomp.regions]
    return group_regions(regions)


def _mir_regions_of_fun_decomp_artifact(artifact: Art) -> list[xtype.Mir_Region_Region]:
    import imandrax_api.lib as xtype

    xval = xtype.read_artifact_data(data=artifact.data, kind=artifact.kind)
    assert isinstance(xval, xtype.Common_Fun_decomp_t_poly)
    return xval.regions


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
    idx_path: list[int],
    constraint_path: list[xtype.Mir_Term_term],
    regions: Sequence[Region],
    eq_term: Callable[[xtype.Mir_Term_term, xtype.Mir_Term_term], bool],
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

    def term_in_list(term: xtype.Mir_Term, terms: list[xtype.Mir_Term]) -> bool:
        return any(eq_term(term, t) for t in terms)

    # all_constraints_with_dup
    constraints_: list[list[xtype.Mir_Term_term]] = [r.constraints for r in regions]
    constraints: list[xtype.Mir_Term_term] = [c for cs in constraints_ for c in cs]
    all_constraints_with_dup: list[xtype.Mir_Term_term] = [
        # c for c in constraints if c not in constraint_path
        c
        for c in constraints
        if not term_in_list(c, constraint_path)
    ]

    # constraints_by_most_frequent
    # Count occurrences of each distinct constraint (distinctness per `eq_term`).
    # MIR terms are unhashable (they contain lists) and unorderable, so we can't
    # key a dict on them or sort on them directly: dedup via `eq_term` into an
    # assoc list, and use the pretty-printed form only as the sort tiebreak.
    def mk_counter(
        ls: list[xtype.Mir_Term_term],
    ) -> list[tuple[xtype.Mir_Term_term, int]]:
        counter: list[list[Any]] = []  # [representative_term, count]
        for s in ls:
            for entry in counter:
                if eq_term(entry[0], s):
                    entry[1] += 1
                    break
            else:
                counter.append([s, 1])
        return [(rep, n) for rep, n in counter]

    counter = mk_counter(all_constraints_with_dup)
    # Most frequent first, ties broken alphabetically.
    assoc_list: list[tuple[xtype.Mir_Term_term, int]] = sorted(
        counter, key=lambda kv: (-kv[1], term_to_string(kv[0]))
    )
    constraints_by_most_frequent: list[xtype.Mir_Term_term] = [
        kv[0] for kv in assoc_list
    ]

    # grouped: tuple[list[RegionGroup], list[RegionStr_]]
    class Acc(TypedDict):
        groups: list[RegionGroup]
        regions: list[Region]
        idx_path: list[int]
        constraint_path: list[xtype.Mir_Term_term]

    def loop(
        # 'acc
        acc: Acc,
        # groups: list[RegionGroup],
        # regions: list[RegionStr],
        # idx_path: list[int],
        # constraint_path: list[str],
        # 'a
        konstraint: xtype.Mir_Term_term,
    ) -> Acc:
        # ) -> tuple[list[RegionGroup], list[RegionStr], list[int], list[str]]:
        groups = acc['groups']
        regions = acc['regions']
        idx_path = acc['idx_path']
        constraint_path = acc['constraint_path']

        has: list[Region] = []
        without: list[Region] = []
        for r in regions:
            # if konstraint in r.constraints:
            if term_in_list(konstraint, r.constraints):
                has.append(r)
            else:
                without.append(r)
        i = len(groups) + 1
        if len(without) == 0 and (not (len(has) == 1)):
            new_idx_path = idx_path
        else:
            new_idx_path: list[int] = [i, *idx_path]
        new_constraint_path: list[xtype.Mir_Term_term] = [konstraint, *constraint_path]

        if len(has) > 0:
            rg_children = _loop_group_regions(
                new_idx_path, new_constraint_path, has, eq_term
            )
            group: RegionGroup
            if len(rg_children) == 1:
                group = rg_children[0]
            else:
                rg_constraints = new_constraint_path[::-1]
                rg_region: Region | None
                if len(has) == 1:
                    rg_region = has[0]
                else:
                    rg_region = None
                rg_weight = len(has)
                group = RegionGroup(
                    constraints=[term_to_string(c) for c in rg_constraints],
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
        groups=[],
        regions=list(regions),
        idx_path=idx_path,
        constraint_path=constraint_path,
    )
    return reduce(loop, constraints_by_most_frequent, init)['groups'][::-1]


# For test
# ====================


def _stringified_term_map_of_region(
    r: xtype.Mir_Region_Region, base: dict[int, str] | None = None
) -> dict[int, str]:
    # MIR terms are unhashable (they contain lists), so key the map by object
    # identity (`id`). This maps each concrete term instance to its server-side
    # string, keeping this grouping strategy independent of the structural
    # `_term_key` used by `eq_term_naive`.
    out = base or {}

    constraints_str = xtype.unwrap_region_str(r).constraints_str
    assert constraints_str is not None
    for c, s in zip(r.constraints, constraints_str):
        out[id(c)] = s

    return out


def _eq_term_with_string_results(
    left: xtype.Mir_Term,
    right: xtype.Mir_Term,
    stringified_term_map: dict[int, str],
) -> bool:
    l = stringified_term_map[id(left)]
    r = stringified_term_map[id(right)]
    return l == r


def _eq_term_naive(
    left: xtype.Mir_Term,
    right: xtype.Mir_Term,
) -> bool:
    return _term_key(left) == _term_key(right)


def _term_key(obj: object) -> str:
    """
    A canonical structural string key for a decoded term.

    Recursively serializes `obj`, skipping `_TERM_KEY_DROP_FIELDS` at every
    level. Terms that differ only in type annotations or source anchors produce
    the same key, so regions sharing a constraint group together even though
    their raw `repr`s differ.
    """
    # Term fields carrying no logical identity: `ty` is the (redundant, given the
    # fully-resolved view) type annotation, `sub_anchor` is a source-position
    # anchor. Both vary between structurally-identical constraints, so they are
    # skipped when deriving a grouping key from a raw term.
    term_key_drop_fields = frozenset({'ty', 'sub_anchor'})
    if is_dataclass(obj) and not isinstance(obj, type):
        inner = ','.join(
            f'{f.name}={_term_key(getattr(obj, f.name))}'
            for f in fields(obj)
            if f.name not in term_key_drop_fields
        )
        return f'{type(obj).__name__}({inner})'
    if isinstance(obj, (list, tuple)):
        return '[' + ','.join(_term_key(x) for x in obj) + ']'  # pyright: ignore
    return repr(obj)

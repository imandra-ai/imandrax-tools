from __future__ import annotations

from dataclasses import asdict, dataclass
from typing import Any, Self, TypedDict

import imandrax_api.lib as xtype
from pydantic import BaseModel, Field

from imandrax_api_models.pp.xtype import to_string as xtype_to_string
from imandrax_api_models.proto_models import Art

from ._common import JSONObject, term_to_string

# NOTE: can be `ContextVar` once it's needs to be mutated
_PREFER_INVARIANT_FROM_PP_OVER_FROM_STRING_RESULT = True
"""Region group's constraints (list[str]) are from pp, so we align invariant's representation for leaf nodes"""
_IGNORE_REGION_OTHER_FIELDS = True
"""When converting to JSON, ignore the `other` field of `Region`, which currently contains `status`"""


class StringResult(TypedDict):
    constraints: list[str]
    invariant: str
    model: dict[str, str] | None
    model_eval: str | None


class RegionNonGroupStat(BaseModel):
    """
    Display stats for one concrete region when in a hierarchical region group.
    """

    invariant: str = Field()
    model: dict[str, str] | str | None = Field(default=None)
    model_eval: str | None = Field(default=None)


# TODO: now we are ready to replace RegionStr with Region completely in simple_api,py
@dataclass
class Region:
    """
    MIR region with derived results
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
        """Region stats without the hierarchical info"""
        out: JSONObject = {}
        out['constraints'] = [term_to_string(c) for c in self.constraints]
        out |= self.non_group_stat().model_dump(exclude_none=True)

        if not _IGNORE_REGION_OTHER_FIELDS:
            out |= self.other

        return out

    def non_group_stat(self) -> RegionNonGroupStat:
        """
        Display stats beyond the hierarchical info (constraints are used during grouping, so they live on `RegionGroup`, not here).
        """
        # TODO: include id?
        invariant = term_to_string(self.mir_region.invariant)
        model: dict[str, str] | str | None = None
        model_eval: str | None = None

        if self.string_result is not None:
            if not _PREFER_INVARIANT_FROM_PP_OVER_FROM_STRING_RESULT:
                invariant = self.string_result['invariant']
            if self.string_result['model'] is not None:
                model = self.string_result['model']
            if self.string_result['model_eval'] is not None:
                model_eval = self.string_result['model_eval']
        else:
            # If model is not set (in the case of `string_results=False`)
            # try to get it from the feasible status
            match self.mir_region.status:
                case xtype.Common_Region_status_Feasible(arg=feasible_model):
                    model = xtype_to_string(feasible_model)
                case _:
                    pass

        return RegionNonGroupStat(
            invariant=invariant, model=model, model_eval=model_eval
        )

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
    A local replacement for xtype.unwrap_region_str, extracting info from MIR region.

    `region.meta` is an assoc-list whose values are wrapped in
    `Common_Region_meta_*` variants (`String`, `Assoc`, `List`, ...); the raw
    payload lives on their `.arg`. Mirror `xtype.unwrap_region_str` and unwrap
    at every level.

    Returns:
        A tuple of region id, string results (optional), and other metadata.

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


def _mir_regions_of_fun_decomp_artifact(artifact: Art) -> list[xtype.Mir_Region_Region]:
    import imandrax_api.lib as xtype

    xval = xtype.read_artifact_data(data=artifact.data, kind=artifact.kind)
    assert isinstance(xval, xtype.Common_Fun_decomp_t_poly)
    return xval.regions

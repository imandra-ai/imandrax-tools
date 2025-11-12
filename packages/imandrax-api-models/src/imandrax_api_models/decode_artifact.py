from __future__ import annotations

import base64
from dataclasses import dataclass
from typing import Any, assert_never

from imandrax_api.lib import (
    Artifact,
    Common_Applied_symbol_t_poly,
    Common_Fun_decomp_t_poly,
    Common_Model_t_poly,
    Common_Region_meta_Assoc,
    Common_Region_meta_String,
    Common_Region_meta_Term,
    Common_Region_t_poly,
    Common_Var_t_poly,
    Mir_Fun_decomp,
    Mir_Term,
    Mir_Term_view_Const,
    Mir_Term_view_Construct,
    Mir_Type,
    Uid,
    read_artifact_data,
)


@dataclass
class RegionStr:
    constraints_str: list[str] | None
    invariant_str: str
    model_str: dict[str, str]
    model_eval_str: str | None


class ArtifactDecodeError(Exception):
    pass


def decode_artifact(
    data: bytes | str,
    kind: str,
) -> list[RegionStr] | dict[str, str] | None:
    data_b: bytes = base64.b64decode(data) if isinstance(data, str) else data
    art: Artifact = read_artifact_data(data=data_b, kind=kind)
    match (art, kind):
        case (
            Common_Fun_decomp_t_poly(
                f_id=_f_id,
                f_args=_f_args,
                regions=regions,
            ) as _fun_decomp,
            'mir.fun_decomp',
        ):
            _fun_decomp: Mir_Fun_decomp
            _f_id: Uid
            _f_args: list[Common_Var_t_poly[Mir_Type]]
            regions: list[Common_Region_t_poly[Mir_Term, Mir_Type]]

            return unwrap_region_str(regions)
        case (
            Common_Model_t_poly(
                # list[tuple[Mir_Type, Common_Model_ty_def[Mir_Term, Mir_Type]]]
                tys=_,
                consts=consts,
                # list[
                #     tuple[
                #         Common_Applied_symbol_t_poly[Mir_Type],
                #         Common_Model_fi[Mir_Term, Mir_Type],
                #     ]
                # ]
                funs=_,
                # bool
                representable=_,
                # bool
                completed=_,
                # list[tuple[Uid, Mir_Type]]
                ty_subst=_,
            ) as _mir_model,
            'mir.model',
        ):
            consts: list[tuple[Common_Applied_symbol_t_poly[Mir_Type], Mir_Term]]
            consts_d: dict[str, Any] = unwrap_model_constants(consts)
            return consts_d
        case _:
            raise ArtifactDecodeError(
                f'Unknown artifact type: {type(art)}, with {kind = }'
            )


def unwrap_model_constants(
    consts: list[tuple[Common_Applied_symbol_t_poly[Mir_Type], Mir_Term]],
) -> dict[str, Any]:
    constants: dict[str, Any] = {}

    for applied_symbol, term in consts:
        match applied_symbol, term:
            case (
                Common_Applied_symbol_t_poly(
                    sym=applied_symbol_sym,
                    args=_,
                    ty=_,
                ),
                Mir_Term(view=term_view, ty=_, sub_anchor=_),
            ):
                var_name = applied_symbol_sym.id.name

                # Extract the value from the term
                match term_view:
                    case Mir_Term_view_Const(arg=term_view_const):
                        constants[var_name] = term_view_const.arg
                    case Mir_Term_view_Construct():
                        raise NotImplementedError(
                            'Term view type of Mir_Term_view_Construct is not supported'
                        )
                    case _:
                        raise NotImplementedError(
                            f'Unexpected term view type: {type(term_view)}'
                        )

    return constants


type region_meta_value = (
    Common_Region_meta_Assoc[Mir_Term]
    | Common_Region_meta_Term[Mir_Term]
    | Common_Region_meta_String[Mir_Term]
)


def unwrap_region_str(
    regions: list[Common_Region_t_poly[Mir_Term, Mir_Type]],
) -> list[RegionStr]:
    """
    Get `RegionStr`s from a list of `Region.t`.

    A region object looks like:
    {
        "constraints": [...],
        "invariant": ...,
        "meta": [
            ("str", ...)  # What we want
            ("model", ...)
            ("model_eval", ...)
            ("id", "...")
        ]
        "status": ...
    }.
    """
    regions_str: list[RegionStr] = []
    for region in regions:
        match region:
            case Common_Region_t_poly(
                constraints=_constraints,
                invariant=_invariant,
                meta=meta,
                status=_status,
            ):
                # Convert meta list to dict
                meta_d: dict[Any, Any] = dict(meta)

                # get `str` dict
                meta_str_raw = meta_d.get('str')
                assert meta_str_raw is not None, "Never: no 'str' in meta"

                # meta_str should be Common_Region_meta_Assoc[Mir_Term]
                if not isinstance(meta_str_raw, Common_Region_meta_Assoc):
                    raise ValueError(
                        f'Expected Common_Region_meta_Assoc, got {type(meta_str_raw)}'
                    )

                meta_str_d: dict[Any, Any] = dict(meta_str_raw.arg)  # type: ignore[arg-type]

                # Extract constraints
                constraints_raw = meta_str_d.get('constraints')
                constraints: list[str] | None
                if constraints_raw is not None:
                    constraints = [c.arg for c in constraints_raw.arg]
                else:
                    constraints = None

                # Extract invariant
                invariant_raw = meta_str_d.get('invariant')
                invariant: str = invariant_raw.arg if invariant_raw is not None else ''

                # Extract model
                model_raw = meta_str_d.get('model')
                model: dict[str, str] = {}
                if model_raw is not None:
                    model = {k: v.arg for (k, v) in model_raw.arg}

                # Extract model_eval (optional)
                model_eval: str | None = None
                if 'model_eval' in meta_str_d:
                    model_eval_raw = meta_str_d['model_eval']
                    if model_eval_raw is not None:
                        model_eval = model_eval_raw.arg

                region_str = RegionStr(
                    invariant_str=invariant,
                    constraints_str=constraints,
                    model_str=model,
                    model_eval_str=model_eval,
                )
                regions_str.append(region_str)
            case _:
                assert_never(region)
    return regions_str

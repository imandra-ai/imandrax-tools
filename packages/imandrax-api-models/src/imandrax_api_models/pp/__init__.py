# ruff: noqa: RUF100, F401, F841
import imandrax_api.lib as xtype
from imandrax_api.lib.twine import Decoder
from imandrax_api_models import DecomposeRes
from imandrax_api_models.proto_models.artmsg import Art

from ._common import FunDecomp, Region, RegionMeta, RegionStatus, Term, Type
from .term_formatter import prettify


def parse_region(
    region: Region,
):
    constraints: list[Term] = region.constraints
    invariant: Term = region.invariant
    meta: list[tuple[str, RegionMeta]] = region.meta
    status: RegionStatus = region.status


def _decode_fun_decomp(art: Art) -> FunDecomp:
    """
    Decode an `Art` payload into the typed `Mir_Fun_decomp`.

    Mirrors `imandrax_api.lib.get_region_str_from_decomp_artifact` but returns
    the structured `Common_Fun_decomp_t_poly[Mir_Term, Mir_Type]` (with
    `.regions: list[Region]`) instead of pp-stringified `RegionStr`s.
    """
    parsed = xtype.read_artifact_data(data=art.data, kind=art.kind)
    if not isinstance(parsed, xtype.Common_Fun_decomp_t_poly):
        raise TypeError(
            f'Expected mir.fun_decomp artifact, got {type(parsed).__name__} '
            f'(kind={art.kind!r})'
        )
    return parsed


def parse_decomp_res(decomp_res: DecomposeRes) -> list[Region]:
    """Decode `decomp_res.artifact` and return its `regions`."""
    if decomp_res.artifact is None:
        raise ValueError('DecomposeRes has no artifact')
    return _decode_fun_decomp(decomp_res.artifact).regions

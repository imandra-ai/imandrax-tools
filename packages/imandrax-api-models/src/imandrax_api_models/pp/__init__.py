# NOTE: alias must NOT be `xtype`: this package has a submodule `pp/xtype.py`,
# and any `from imandrax_api_models.pp.xtype import ...` (e.g. in region_decomp)
# rebinds the `pp.xtype` attribute to that submodule, clobbering this alias.
import imandrax_api.lib as _xlib

from imandrax_api_models import DecomposeRes
from imandrax_api_models.proto_models.artmsg import Art

from ._common import FunDecomp, Region


def _decode_fun_decomp(art: Art) -> FunDecomp:
    """
    Decode an `Art` payload into the typed `Mir_Fun_decomp`.

    Mirrors `imandrax_api.lib.get_region_str_from_decomp_artifact` but returns
    the structured `Common_Fun_decomp_t_poly[Mir_Term, Mir_Type]` (with
    `.regions: list[Region]`) instead of pp-stringified `RegionStr`s.
    """
    parsed = _xlib.read_artifact_data(data=art.data, kind=art.kind)
    if not isinstance(parsed, _xlib.Common_Fun_decomp_t_poly):
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

# ruff: noqa: RUF100, F401
from imandrax_api_models.context_utils import format_code_snippet_with_loc

from .common import BaseDiag, NoLoc


def format_diagnostics(diags: list[BaseDiag], code: str | None) -> str:
    s = ''
    for i, diag in enumerate(diags, 1):
        s += f'{i}. '
        s += diag.format_error_message()
        if code is not None and not isinstance(diag.loc, NoLoc):
            loc = diag.loc
            s += format_code_snippet_with_loc(code, loc.start_point, loc.end_point)
        s += '\n'
    return s

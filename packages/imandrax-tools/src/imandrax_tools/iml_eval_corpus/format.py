from .common import BaseDiag


def format_diagnostics(diags: list[BaseDiag], code: str | None) -> str:
    s = ''
    for i, diag in enumerate(diags, 1):
        s += f'{i}. '
        s += diag.format(code=code)
        s += '\n\n'
    return s

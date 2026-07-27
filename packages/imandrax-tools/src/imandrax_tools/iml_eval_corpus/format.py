from .common import BaseDiag


def format_diagnostics(
    diags: list[BaseDiag],
    code: str | None,
    header: bool = False,
) -> str:
    s = ''
    if header:
        s += f'Found {len(diags)} diagnostics:\n'
    for i, diag in enumerate(diags, 1):
        s += f'{i}. '
        s += diag.format(code=code)
        s += '\n\n'
    return s

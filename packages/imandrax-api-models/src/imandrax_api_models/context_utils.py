"""Utility functions for formatting ImandraX models to LLM context."""

from typing import cast

from imandrax_api_models import (
    Error,
    ErrorMessage,
    EvalOutput,
    EvalRes,
    Location,
    PO_Res,
    Position,
)


def format_code_snippet_with_error(
    src: str,
    start_pos: tuple[int, int],
    end_pos: tuple[int, int],
    message: str,
    context_line: int = 2,
):
    """Format a code snippet with highlighted error range."""
    lines = src.split('\n')
    start_line, start_col = start_pos
    end_line, end_col = end_pos

    # Determine visible range
    visible_start = max(0, start_line - context_line - 1)
    visible_end = min(len(lines), end_line + context_line)

    output: list[str] = []
    output.append(f'Error: {message}')
    output.append('')

    # Calculate max line number width for alignment
    max_line_no = visible_end
    line_no_width = len(str(max_line_no))

    for i in range(visible_start, visible_end):
        line_no = i + 1

        # Mark lines within error range
        if start_line <= line_no <= end_line:
            marker = '*'
        else:
            marker = ' '

        output.append(f'{marker} {line_no:{line_no_width}} | {lines[i]}')

        # Add underline/pointer - padding is marker(1) + space(1) + line_no_width + space(1)
        underline_prefix = ' ' * (2 + line_no_width)

        # Add underline/pointer
        if line_no == start_line == end_line:
            # Single line error
            underline = ' ' * start_col + '^' * (end_col - start_col)
            output.append(f'{underline_prefix} | {underline}')
        elif line_no == start_line:
            # Start of multi-line error
            underline = ' ' * start_col + '^' + '~' * (len(lines[i]) - start_col)
            output.append(f'{underline_prefix} | {underline}')
        elif line_no == end_line:
            # End of multi-line error
            underline = '~' * end_col
            output.append(f'{underline_prefix} | {underline}')
        elif start_line < line_no < end_line:
            # Middle of multi-line error
            output.append(f'{underline_prefix} | {"~" * len(lines[i])}')

    return '\n'.join(output)


def format_error_msg(
    error_msg: ErrorMessage,
    iml_src: str | None = None,
    max_backtrace_len: int = 0,
) -> str:
    locs: list[Location] = error_msg.locs or []
    locs = [loc for loc in locs if (loc.start is not None and loc.stop is not None)]

    assert len(locs) <= 1

    loc_str: str | None = None
    error_src: str | None = None
    if locs:
        start, stop = locs[0].start, locs[0].stop
        start = cast(Position, start)
        stop = cast(Position, stop)
        loc_str = f'Lines: {start.line}:{start.col}-{stop.line}:{stop.col}'

        error_src: str | None = None
        if iml_src is not None:
            start_pos = (start.line, start.col)
            end_pos = (stop.line, stop.col)
            error_src = format_code_snippet_with_error(
                iml_src, start_pos, end_pos, error_msg.msg
            )

    res = ''
    if loc_str:
        res += loc_str
    if error_src:
        res += '\n'
        res += error_src
    if max_backtrace_len > 0 and error_msg.backtrace:
        res += f'\nbacktrace: {error_msg.backtrace[:max_backtrace_len]}'
    return res


def format_error(
    error: Error, iml_src: str | None = None, max_stack_depth: int = 3
) -> str:
    err_kind = error.kind
    top_msg: str | None = None

    if error.msg is not None:
        top_msg = format_error_msg(error.msg, iml_src)

    stack_strs = (
        [format_error_msg(msg, iml_src) for msg in error.stack[:max_stack_depth]]
        if error.stack
        else []
    )
    stack_str = '\n'.join(stack_strs)

    s = ''
    # Top message
    s += f'{top_msg}' if top_msg else ''
    s += f'\n<kind>{err_kind}</kind>' if err_kind else ''

    # Stack trace
    s += f'\n<stack>\n{stack_str}\n</stack>' if stack_str else ''

    return s


def format_eval_res_errors(
    eval_res: EvalRes,
    iml_src: str | None = None,
    max_errors: int = 3,
) -> str | None:
    if not eval_res.errors and not eval_res.po_errors:
        return None

    # If non-PO error exist, ignore PO errors
    is_po_error: bool = False
    if len(eval_res.errors) > 0:
        errs = eval_res.errors[:max_errors]
    else:
        is_po_error = True
        errs = eval_res.po_errors[:max_errors]

    err_strs: list[str] = [format_error(err, iml_src) for err in errs]

    def add_tag(s: str, i: int) -> str:
        if is_po_error:
            return f'<po_error_{i}>\n{s}\n</po_error_{i}>'
        else:
            return f'<error_{i}>\n{s}\n</error_{i}>'

    res = ''
    if is_po_error:
        res += 'Proof obligation errors (including termination proving errors):\n\n'
    for i, err_str in enumerate(err_strs, 1):
        res += add_tag(err_str, i)
    return res


def format_eval_output(eval_output: EvalOutput) -> str: ...


def format_po_res(po_res: PO_Res) -> str: ...


def format_eval_res(eval_res: EvalRes, iml_src: str | None = None) -> str:
    if not eval_res.has_errors:
        s = 'Success!'
        if eval_res.eval_results:
            s += '\n'
        for i, eval_result in enumerate(eval_res.eval_results, 1):
            s += f'\nEval result #{i}:\n'
            success = eval_result.success
            s += f'- success: {success}\n'
            s += f'- value as ocaml: {eval_result.value_as_ocaml}\n'
        return s

    else:
        s = ''
        s += 'Evaluation errors:\n\n'
        s += cast(str, format_eval_res_errors(eval_res, iml_src))
        return s

"""Utility functions for formatting ImandraX models to LLM context."""

from __future__ import annotations

from collections.abc import Collection
from typing import Any, cast

from imandrax_api_models import (
    Error,
    ErrorMessage,
    EvalOutput,
    EvalRes,
    InstanceRes,
    Location,
    Position,
    VerifyRes,
)

type JSONValue = str | int | float | bool | None | JSONObject | JSONArray
type JSONObject = dict[str, JSONValue]
type JSONArray = list[JSONValue]


def format_code_snippet_with_loc(
    src: str,
    start_pos: tuple[int, int],
    end_pos: tuple[int, int],
    context_line: int = 2,
):
    r"""
    Format a code snippet with a highlighted span.

    `start_pos`/`end_pos` are 1-indexed `(line, col)`; `end_col` is exclusive.

    Single-line spans get a caret underline. Multi-line spans get a rustc-style
    frame in an extra gutter column: `/` on the start line, `|` through the
    middle, `\` on the end line.
    """
    lines = src.split('\n')
    start_line, start_col = start_pos
    end_line, end_col = end_pos

    visible_start = max(0, start_line - context_line - 1)
    visible_end = min(len(lines), end_line + context_line)

    line_no_width = len(str(visible_end))
    gutter_pad = ' ' * line_no_width
    is_multiline = end_line > start_line

    output: list[str] = []
    for i in range(visible_start, visible_end):
        line_no = i + 1
        line = lines[i]
        in_span = start_line <= line_no <= end_line

        if is_multiline:
            if not in_span:
                frame = '  '
            elif line_no == start_line:
                frame = '/ '
            else:
                # `|` carries through middle and end lines; the end is marked by
                # a `|____^` underscore row below.
                frame = '| '
            output.append(f'{line_no:{line_no_width}} | {frame}{line}')
            if in_span and line_no == end_line:
                underscores = '_' * max(1, end_col - 1)
                output.append(f'{gutter_pad} | |{underscores}^')
        else:
            output.append(f'{line_no:{line_no_width}} | {line}')
            if line_no == start_line:
                caret_width = max(1, end_col - start_col)
                underline = ' ' * (start_col - 1) + '^' * caret_width
                output.append(f'{gutter_pad} | {underline}')

    return '\n'.join(output)


# ====================


def format_error_msg(
    error_msg: ErrorMessage,
    iml_src: str | None = None,
    max_backtrace_len: int = 0,
) -> str:
    locs: list[Location] = error_msg.locs or []
    locs = [loc for loc in locs if (loc.start is not None and loc.stop is not None)]

    loc_str: str | None = None
    error_src: str | None = None
    if locs:
        # TODO: handle multiple locations
        start, stop = locs[0].start, locs[0].stop
        start = cast(Position, start)
        stop = cast(Position, stop)
        loc_str = f'Lines: {start.line}:{start.col}-{stop.line}:{stop.col}'

        error_src = f'Error: {error_msg.msg}\n'
        if iml_src is not None:
            start_pos = (start.line, start.col)
            end_pos = (stop.line, stop.col)
            error_src = format_code_snippet_with_loc(iml_src, start_pos, end_pos)
    else:
        # No location information
        error_src = error_msg.msg

    res = ''
    if loc_str:
        res += loc_str
        res += '\n'
    if error_src:
        res += error_src
    if max_backtrace_len > 0 and error_msg.backtrace:
        res += f'\nbacktrace: {error_msg.backtrace[:max_backtrace_len]}'
    return res


def format_error(
    error: Error,
    iml_src: str | None = None,
    max_stack_depth: int = 3,
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


def format_errors(
    non_po_errors: list[Error],
    po_errors: list[Error],
    iml_src: str | None = None,
    max_errors: int = 3,
) -> str | None:
    # If non-PO error exist, ignore PO errors
    is_po_error: bool = False
    if len(non_po_errors) > 0:
        errs = non_po_errors[:max_errors]
    else:
        is_po_error = True
        errs = po_errors[:max_errors]

    err_strs: list[str] = [format_error(err, iml_src) for err in errs]

    def add_tag(s: str, i: int) -> str:
        if is_po_error:
            return f'<po_error_{i}>\n{s}\n</po_error_{i}>'
        else:
            return f'<error_{i}>\n{s}\n</error_{i}>'

    res = ''
    if is_po_error:
        res += 'Proof obligation errors (inc. termination proving):\n'
    for i, err_str in enumerate(err_strs, 1):
        res += add_tag(err_str, i)
        res += '\n'
    return res


def format_eval_output(eval_output: EvalOutput) -> str:
    buf = ''
    if not eval_output.success or len(eval_output.errors) > 0:
        buf += 'Eval failed:\n'
        for err in eval_output.errors[:1]:
            buf += format_error(err)
        buf += '\n'
    else:
        buf += 'Eval Output:\n'
        buf += f'value_as_ocaml: {eval_output.value_as_ocaml!r}\n'
    return buf


def _extract_internal_error(msg: str, max_len: int = 300) -> str:
    """
    Extract error message from internal error, truncating if necessary.

    Looks for patterns like:
        Error{ Kind.name = "LowerRirError" }:
          Lower-RIR.Term: Cannot make closure...

    Returns the error kind and message, truncated before backtrace/context.
    Falls back to truncated raw message if extraction fails.
    """
    import re

    # Match Error{ Kind.name = "..." }: followed by the error message
    pattern = (
        r'Error\{\s*Kind\.name\s*=\s*"([^"]+)"\s*\}:\s*(.+?)(?=backtrace:|Context:|$)'
    )
    match = re.search(pattern, msg, re.DOTALL)
    if match:
        kind = match.group(1)
        error_text = match.group(2).strip()
        # Clean up whitespace (the original has lots of padding spaces)
        error_text = re.sub(r'\s+', ' ', error_text)
        result = f'[{kind}] {error_text}'
    else:
        # Fallback: just clean up and truncate the raw message
        result = re.sub(r'\s+', ' ', msg).strip()

    if len(result) > max_len:
        result = result[: max_len - 3] + '...'
    return result


def _format_unstructured_msg_errors(
    errs_in_eval_msg: list[str],
    max_msgs: int = 2,
    max_len_per_msg: int = 500,
) -> str:
    """
    Render extracted error string blurbs from `eval_res.messages`.

    Up to `max_msgs` are shown, deduped by extracted text. Appends a
    truncation hint if more were dropped.
    """
    seen: set[str] = set()
    extracted: list[str] = []
    for msg in errs_in_eval_msg:
        e = _extract_internal_error(msg, max_len=max_len_per_msg)
        if e in seen:
            continue
        seen.add(e)
        extracted.append(e)

    shown = extracted[:max_msgs]
    s = '\n'.join(f'- {e}' for e in shown)
    hidden = len(extracted) - len(shown)
    if hidden > 0:
        s += f'\n- ... ({hidden} more similar message(s) omitted)'
    return s


def format_eval_res(eval_res: EvalRes, iml_src: str | None = None) -> str:
    # Check additional error in message (internal errors)
    errs_in_eval_msg: list[str] = [
        msg for msg in eval_res.messages if 'error' in msg.lower()
    ]
    has_structured_err = eval_res.has_errors
    has_err_in_eval_msg = len(errs_in_eval_msg) > 0
    match (has_structured_err, has_err_in_eval_msg):
        case True, _:
            s = ''
            s += 'Evaluation errors:\n'
            s += cast(str, format_errors(eval_res.errors, eval_res.po_errors, iml_src))
            if has_err_in_eval_msg:
                s += '\nAdditional unstructured errors:\n'
                s += _format_unstructured_msg_errors(errs_in_eval_msg)
            return s
        case False, True:
            s = 'ImandraX internal error:\n'
            s += _format_unstructured_msg_errors(errs_in_eval_msg)
            return s
        case False, False:
            s = 'Eval success!'
            if eval_res.eval_results:
                s += '\n'
            for i, eval_result in enumerate(eval_res.eval_results, 1):
                s += f'\nEval result #{i}:\n'
                success = eval_result.success
                s += f'- success: {success}\n'
                s += f'- value as ocaml: {eval_result.value_as_ocaml}\n'
            return s


def format_error_msg_v2(
    error_msg: ErrorMessage,
    iml_src: str | None = None,
    max_backtrace_len: int = 0,
) -> dict[str, Any]:
    locs: list[Location] = error_msg.locs or []
    locs = [loc for loc in locs if (loc.start is not None and loc.stop is not None)]

    res: dict[str, Any] = {}
    if locs:
        # TODO: handle multiple locations
        start, stop = locs[0].start, locs[0].stop
        start = cast(Position, start)
        stop = cast(Position, stop)
        location = {
            'start': start.model_dump(),
            'stop': stop.model_dump(),
        }
        res['location'] = location

        if iml_src is not None:
            start_pos = (start.line, start.col)
            end_pos = (stop.line, stop.col)
            error_src = format_code_snippet_with_loc(iml_src, start_pos, end_pos)
            res['error_src'] = error_src
    else:
        # No location information
        res['error_msg'] = error_msg.msg

    if max_backtrace_len > 0 and error_msg.backtrace:
        res['backtrace'] = error_msg.backtrace[:max_backtrace_len]
    return res


def format_error_v2(
    error: Error,
    iml_src: str | None = None,
    max_stack_depth: int = 3,
) -> dict[str, Any]:
    err_kind = error.kind
    top_msg: dict[str, Any] | None = None

    if error.msg is not None:
        top_msg = format_error_msg_v2(error.msg, iml_src)

    stack_strs = (
        [format_error_msg_v2(msg, iml_src) for msg in error.stack[:max_stack_depth]]
        if error.stack
        else []
    )

    out: dict[str, Any] = {}
    # Top message
    if top_msg:
        out['top_msg'] = top_msg
    out['err_kind'] = err_kind
    out['stack'] = stack_strs

    return out


def format_errors_v2(
    non_po_errors: list[Error],
    po_errors: list[Error],
    iml_src: str | None = None,
    max_errors: int = 3,
) -> dict[str, Any]:
    # If non-PO error exist, ignore PO errors
    is_po_error: bool = False
    if len(non_po_errors) > 0:
        errs = non_po_errors[:max_errors]
    else:
        is_po_error = True
        errs = po_errors[:max_errors]

    err_infos: list[dict[str, Any]] = [format_error_v2(err, iml_src) for err in errs]

    out: dict[str, Any] = {}
    if is_po_error:
        out['error_type'] = 'proof_obligation_error'
    for i, err_info in enumerate(err_infos, 1):
        out[f'error_{i}'] = err_info
    return out


def format_eval_res_v2(eval_res: EvalRes, iml_src: str | None = None) -> JSONObject:
    # Check additional error in messages
    errs_in_eval_msg: list[str] = [
        msg for msg in eval_res.messages if 'error' in msg.lower()
    ]
    has_structured_err = eval_res.has_errors
    has_err_in_eval_msg = len(errs_in_eval_msg) > 0

    out: JSONObject = {}
    match (has_structured_err, has_err_in_eval_msg):
        case True, _:
            out['error'] = format_errors_v2(
                eval_res.errors, eval_res.po_errors, iml_src
            )
            if has_err_in_eval_msg:
                out['msg_errors'] = _format_unstructured_msg_errors(errs_in_eval_msg)
            return out
        case False, True:
            return {'msg_errors': _format_unstructured_msg_errors(errs_in_eval_msg)}
        case False, False:
            out['description'] = 'Eval success!'
            for i, eval_result in enumerate(eval_res.eval_results, 1):
                data: JSONObject = {
                    'success': eval_result.success,
                    'value_as_ocaml': eval_result.value_as_ocaml,
                }
                out[f'eval_result_{i}'] = data
            return out


# ====================


def remove_fields_rec(
    data: dict[str, Any], remove_fields: Collection[str] = ('artifact', 'task')
) -> dict[str, Any]:
    """Resursively look inside a dict for certain keys and remove it."""
    data = data.copy()
    for k in list(data.keys()):
        v = data[k]
        if k in remove_fields:
            data.pop(k)
        elif isinstance(v, dict):
            v = cast(dict[str, Any], v)
            data[k] = remove_fields_rec(v)
    return data


def format_vg_res(vg_res: VerifyRes | InstanceRes) -> JSONObject:
    out: JSONObject = {}
    if vg_res.errors:
        out['description'] = 'VG has errors'
        for i, err in enumerate(vg_res.errors, 1):
            out[f'error_{i}'] = format_error_v2(err)
    else:
        out['res_type'] = vg_res.res_type
        out['res'] = vg_res.res.model_dump(mode='json')

    return out

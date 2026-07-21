"""
Utility functions for formatting ImandraX API models to LLM context.

Most functions returns a JSON-serializable value, suitable to be further dumped as YAML (as a human-readable format)
"""

from __future__ import annotations

from collections.abc import Collection, MutableMapping, MutableSequence
from typing import Any, Literal, assert_never, cast, get_args

from imandrax_api_models import (
    DecomposeRes,
    Error,
    ErrorKind,
    ErrorKindParsingError,
    ErrorMessage,
    EvalOutput,
    EvalRes,
    InstanceRes,
    Location,
    Position,
    VerifyRes,
)
from imandrax_api_models.region_decomp import EnrichedDecomposeRes

type JSONValue = str | int | float | bool | None | JSONObject | JSONArray
type JSONObject = MutableMapping[str, JSONValue]
type JSONArray = MutableSequence[JSONValue]


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


def format_eval_output(eval_output: EvalOutput, error_limit: int = 1) -> JSONObject:
    out: JSONObject = {}
    if not eval_output.success or len(eval_output.errors) > 0:
        out['description'] = 'Eval failed'
        errors = eval_output.errors
        for i, err in enumerate(errors[:error_limit]):
            out[f'error_{i + 1}'] = format_error(err)
        if n_omitted := len(errors) - error_limit > 0:
            out['n_error_omitted'] = n_omitted

    else:
        out['description'] = 'Eval succeeded'
        out['value_as_ocaml'] = eval_output.value_as_ocaml
    return out


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
) -> JSONArray:
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

    out: JSONArray = [*extracted[:max_msgs]]
    if (n_hidden := len(extracted) - len(out)) > 0:
        out.append(f'{n_hidden} error messages omitted')
    return out


def format_location(loc: Location) -> str:
    start = loc.start
    end = loc.stop

    def string_of_pos(pos_opt: Position | None) -> str:
        if pos_opt is None:
            return '?'
        else:
            return f'{pos_opt.line}:{pos_opt.col}'

    return f'{string_of_pos(start)}-{string_of_pos(end)}'


def format_error_msg(
    error_msg: ErrorMessage,
    iml_src: str | None = None,
    max_backtrace_len: int = 0,
) -> JSONObject:
    res: dict[str, Any] = {'msg': error_msg.msg}

    locs: list[Location] = error_msg.locs or []
    locs = [loc for loc in locs if (loc.start is not None and loc.stop is not None)]

    if locs:
        # TODO: handle multiple locations
        start, stop = locs[0].start, locs[0].stop
        start = cast(Position, start)
        stop = cast(Position, stop)
        res['location'] = format_location(locs[0])

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


def format_error(
    error: Error,
    iml_src: str | None = None,
    max_stack_depth: int = 3,
) -> JSONObject:
    out: dict[str, Any] = {}

    err_kind: str = error.kind
    # '{ Kind.name = "TacticEvalErr" }' -> 'TacticEvalErr'
    try:
        err_kind = ErrorKind.from_proto_kind_exn(err_kind).value
    except ErrorKindParsingError:
        pass
    out['kind'] = err_kind

    if error.msg is not None:
        out['msg'] = format_error_msg(error.msg, iml_src)

    stack_strs = (
        [format_error_msg(msg, iml_src) for msg in error.stack[:max_stack_depth]]
        if error.stack
        else []
    )
    if stack_strs:
        out['stack'] = stack_strs

    return out


def format_errors(
    non_po_errors: list[Error],
    po_errors: list[Error],
    iml_src: str | None = None,
    max_errors: int = 3,
) -> JSONArray:
    """
    Format errors from non-PO and PO error lists.

    If non-PO errors exist, return them; otherwise, return PO errors.
    The total number of errors returned is limited to `max_errors`.
    Omitted errors are counted and reported in the output.
    """
    n_non_po = len(non_po_errors)
    n_po = len(po_errors)

    out: list[JSONValue] = []
    omitted_count = {'non_po': 0, 'po': 0}

    def fmt_omitted(omitted_count: dict[str, int]) -> str:
        if omitted_count['non_po'] == 0 and omitted_count['po'] == 0:
            return ''
        if omitted_count['non_po'] == 0:
            return f'(omitted {omitted_count["po"]} PO errors)'
        if omitted_count['po'] == 0:
            return f'(omitted {omitted_count["non_po"]} non-PO errors)'
        return f'(omitted {omitted_count["non_po"]} non-PO and {omitted_count["po"]} PO errors)'

    for err in non_po_errors[:max_errors]:
        out.append(format_error(err, iml_src))
    if n_non_po > 0:
        # Early return if non-PO errors exist
        omitted_count['non_po'] = max(0, n_non_po - max_errors)
        omitted_count['po'] = n_po
        if omitted_str := fmt_omitted(omitted_count):
            out.append(omitted_str)
        return out
    else:
        for err in po_errors[:max_errors]:
            err_disp = format_error(err, iml_src)
            err_disp['is_po'] = True
            out.append(err_disp)
        if n_po > max_errors:
            omitted_count['po'] = n_po - max_errors
            if omitted_str := fmt_omitted(omitted_count):
                out.append(omitted_str)
        return out


def format_eval_res(
    eval_res: EvalRes,
    iml_src: str | None = None,
    process_decomp: bool = True,
) -> JSONObject | str:
    """
    Format an EvalRes object into a JSON-serializable value.

    Args:
        iml_src: IML source used to resolve error locations
        process_decomp: whether to populate decomp grouping information

    Returns:
        A JSON-serializable object (Mapping) or a string if "description" is the only field

    """
    # Check additional error in messages
    errs_in_eval_msg: list[str] = [
        msg for msg in eval_res.messages if 'error' in msg.lower()
    ]
    has_structured_err = eval_res.has_errors
    has_err_in_eval_msg = len(errs_in_eval_msg) > 0

    out: JSONObject = {}
    match (has_structured_err, has_err_in_eval_msg):
        case True, _:
            desc = 'Eval: '
            if n_no_po_err := len(eval_res.errors):
                desc += f'{n_no_po_err} non-PO errors; '
            if n_po_err := len(eval_res.po_errors):
                desc += f'{n_po_err} PO errors'
            out['desc'] = desc.rstrip('; ')
            out['error'] = format_errors(eval_res.errors, eval_res.po_errors, iml_src)
            if has_err_in_eval_msg:
                out['err_in_msg'] = _format_unstructured_msg_errors(errs_in_eval_msg)

        case False, True:
            out['desc'] = 'Eval: error in eval messages'
            out['err_in_msg'] = _format_unstructured_msg_errors(errs_in_eval_msg)
        case False, False:
            out['desc'] = 'Eval succeed'

    # Pack .eval_results and .decomp_results
    for i, eval_result in enumerate(eval_res.eval_results, 1):
        data: JSONObject = {
            'success': eval_result.success,
            'value_as_ocaml': eval_result.value_as_ocaml,
        }
        if eval_result.errors:
            data['errors'] = format_errors(
                eval_result.errors, [], iml_src, max_errors=1
            )
        out[f'eval_result_{i}'] = data
    for i, decomp_res in enumerate(eval_res.decomp_results, 1):
        if not process_decomp:
            out[f'decomp_result_{i}'] = remove_fields_rec(
                decomp_res.model_dump(mode='json'), replace_with=('right', '<hidden>')
            )
        else:
            out[f'decomp_result_{i}'] = format_enriched_decomp_res(
                EnrichedDecomposeRes.from_decomp_res(decomp_res)
            )

    if len(out.keys()) == 1 and 'desc' in out:
        return out['desc']
    return out


# ====================

type Either[A, B] = tuple[Literal['left'], A] | tuple[Literal['right'], B]


def remove_fields_rec(
    data: dict[str, Any],
    remove_fields: Collection[str] = ('artifact', 'task'),
    replace_with: Either[None, Any] = ('left', None),
) -> dict[str, Any]:
    """
    Resursively look inside a dict for certain keys and remove / replace it.

    Args:
        data: the dictionary to process.
        remove_fields: the keys to remove or hide.
        replace_with (Either[None, Any]): If provided, the field is replaced with this value instead of being removed.

    """
    data = data.copy()
    for k in list(data.keys()):
        v = data[k]
        if k in remove_fields:
            if replace_with[0] == 'left':
                data.pop(k)
            else:
                data[k] = replace_with[1]
        elif isinstance(v, dict):
            v = cast(dict[str, Any], v)
            data[k] = remove_fields_rec(v)
    return data


def format_vg_res(vg_res: VerifyRes | InstanceRes) -> JSONObject:
    out: JSONObject = {}
    if vg_res.errors:
        out['description'] = f'VG has {len(vg_res.errors)} error(s)'
        for i, err in enumerate(vg_res.errors, 1):
            out[f'error_{i}'] = format_error(err)
    else:
        out['res_type'] = vg_res.res_type
        out['res'] = remove_fields_rec(
            vg_res.res.model_dump(mode='json'), replace_with=('right', '<hidden>')
        )

    return out


def format_decomp_res(decomp_res: DecomposeRes | EnrichedDecomposeRes) -> JSONObject:
    d: dict[str, Any] = {}
    if len(decomp_res.errors) > 0:
        d['description'] = 'Decomp failed'
        d['errors'] = [format_error(e) for e in decomp_res.errors]
    elif decomp_res.regions_str is None:
        d['description'] = (
            'Decomp succeeded with no parsed region:'
            'Please investigate artifact and task details'
        )
    else:
        from pydantic_core import to_jsonable_python

        d['description'] = (
            f'Decomp succeeded with {len(decomp_res.regions_str)} regions'
        )
        # NOTE: this will be deprecated soon with the removal of RegionStr
        d['stringified_regions'] = [
            to_jsonable_python(r) for r in decomp_res.regions_str
        ]
    return d


def format_enriched_decomp_res(decomp_res: EnrichedDecomposeRes) -> JSONObject:
    d: dict[str, Any] = {}
    if len(decomp_res.region_groups) > 0:
        enriched_regions = decomp_res.regions_with_group_info()
        d['description'] = f'Decomp succeeded with {len(enriched_regions)} regions'
        d['regions'] = enriched_regions
    else:
        # NOTE: this path will no longer be needed once we consolidate EnrichedDecomposeRes and DecomposeRes
        d |= format_decomp_res(decomp_res)
    return d


type FormattableModel = (
    EvalOutput
    | VerifyRes
    | InstanceRes
    | Error
    | ErrorMessage
    | EvalRes
    | DecomposeRes
    | EnrichedDecomposeRes
)

# ====================


def jsonable_of_model(model: FormattableModel) -> JSONValue:
    """Dispatch to the appropriate string representation for the given model."""
    match model:
        case EvalOutput():
            return format_eval_output(model)
        case VerifyRes() | InstanceRes():
            return format_vg_res(model)
        case Error():
            return format_error(model)
        case EvalRes():
            return format_eval_res(model)
        case EnrichedDecomposeRes():
            return format_enriched_decomp_res(
                EnrichedDecomposeRes.from_decomp_res(model)
            )
        case DecomposeRes():
            try:
                return jsonable_of_model(EnrichedDecomposeRes.from_decomp_res(model))
            except Exception:
                return format_decomp_res(model)
        case ErrorMessage():
            return format_error_msg(model)
        case _:
            assert_never(model)
            return None


def string_of_model(model: FormattableModel) -> str:
    import yaml

    return yaml.dump(
        jsonable_of_model(model), sort_keys=False, default_flow_style=False
    )


def show_model(model: FormattableModel) -> None:
    print(string_of_model(model))


def register_model_repr() -> None:
    for model_cls in get_args(FormattableModel.__value__):
        model_cls.__repr__ = string_of_model

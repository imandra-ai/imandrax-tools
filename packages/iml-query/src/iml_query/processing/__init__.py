from typing import Any

from iml_query.queries import (
    OPAQUE_QUERY_SRC,
    EvalCapture,
)
from iml_query.tree_sitter_utils import (
    get_parser,
    mk_query,
    run_query,
    unwrap_bytes,
)

from .base import Nesting, resolve_nesting_definitions
from .decomp import extract_decomp_reqs, insert_decomp_req
from .update_top_def import update_top_definition
from .vg import (
    extract_instance_reqs,
    extract_verify_reqs,
    insert_instance_req,
    insert_verify_req,
)

__all__ = [
    'Nesting',
    'resolve_nesting_definitions',
    'update_top_definition',
    'extract_verify_reqs',
    'extract_instance_reqs',
    'extract_decomp_reqs',
    'insert_verify_req',
    'insert_instance_req',
    'insert_decomp_req',
    'iml_outline',
    'eval_capture_to_src',
]


def iml_outline(iml: str) -> dict[str, Any]:
    outline: dict[str, Any] = {}
    tree = get_parser().parse(bytes(iml, encoding='utf8'))
    outline['verify_req'] = extract_verify_reqs(iml, tree)[2]
    outline['instance_req'] = extract_instance_reqs(iml, tree)[2]
    outline['decompose_req'] = extract_decomp_reqs(iml, tree)[2]
    outline['opaque_function'] = extract_opaque_function_names(iml)
    return outline


def extract_opaque_function_names(iml: str) -> list[str]:
    opaque_functions: list[str] = []
    matches = run_query(
        mk_query(OPAQUE_QUERY_SRC),
        code=iml,
    )
    for _, capture in matches:
        value_name_node = capture['function_name'][0]
        func_name = unwrap_bytes(value_name_node.text).decode('utf-8')
        opaque_functions.append(func_name)

    return opaque_functions


def eval_capture_to_src(capture: EvalCapture) -> str:
    """Extract str from an eval statement node."""
    src = unwrap_bytes(capture.eval_expr.text).decode('utf-8').strip()
    # Remove parentheses
    if src.startswith('(') and src.endswith(')'):
        src = src[1:-1].strip()
    return src

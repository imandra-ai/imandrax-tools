"""verify and instance requests related."""

from typing import NotRequired, Required, TypedDict, cast

from tree_sitter import Range, Tree

from iml_query.queries import (
    INSTANCE_QUERY_SRC,
    VERIFY_QUERY_SRC,
    InstanceCapture,
    VerifyCapture,
)
from iml_query.tree_sitter_utils import (
    delete_nodes,
    insert_lines,
    mk_query,
    run_query,
    unwrap_bytes,
)


class VerifyReqArgs(TypedDict):
    src: Required[str]
    hints: NotRequired[str | None]


def verify_capture_to_req(
    capture: VerifyCapture,
) -> tuple[VerifyReqArgs, Range]:
    """Extract ImandraX request from a verify statement node."""
    node = capture.verify_expr
    req: dict[str, str] = {}
    src_raw = unwrap_bytes(node.text).decode('utf-8')

    # Trim and remove parentheses
    src_trimmed = src_raw.strip()
    if src_trimmed.startswith('(') and src_trimmed.endswith(')'):
        src = src_trimmed[1:-1].strip()
    else:
        src = src_trimmed

    req['src'] = src
    return (cast(VerifyReqArgs, req), node.range)


def instance_capture_to_req(
    capture: InstanceCapture,
) -> tuple[VerifyReqArgs, Range]:
    """Extract ImandraX request from an instance statement node."""
    node = capture.instance_statement
    req: dict[str, str] = {}
    assert node.type == 'instance_statement', 'not instance_statement'
    instance_src = (
        unwrap_bytes(capture.instance_expr.text).decode('utf-8').strip()
    )
    # Remove parentheses
    if instance_src.startswith('(') and instance_src.endswith(')'):
        instance_src = instance_src[1:-1].strip()
    req['src'] = instance_src
    return (cast(VerifyReqArgs, req), node.range)


def _remove_verify_reqs(
    iml: str,
    tree: Tree,
    captures: list[VerifyCapture],
) -> tuple[str, Tree]:
    """Remove verify requests from IML code."""
    verify_stmt_nodes = [capture.verify_statement for capture in captures]
    new_iml, new_tree = delete_nodes(iml, tree, nodes=verify_stmt_nodes)
    return new_iml, new_tree


def extract_verify_reqs(
    iml: str, tree: Tree
) -> tuple[str, Tree, list[VerifyReqArgs], list[Range]]:
    root = tree.root_node
    matches = run_query(
        mk_query(VERIFY_QUERY_SRC),
        node=root,
    )

    verify_captures = [
        VerifyCapture.from_ts_capture(capture) for _, capture in matches
    ]
    req_and_range: list[tuple[VerifyReqArgs, Range]] = [
        verify_capture_to_req(capture) for capture in verify_captures
    ]
    if not req_and_range:
        return iml, tree, [], []
    else:
        reqs, ranges = zip(*req_and_range)
    new_iml, new_tree = _remove_verify_reqs(iml, tree, verify_captures)
    return new_iml, new_tree, list(reqs), list(ranges)


def _remove_instance_reqs(
    iml: str,
    tree: Tree,
    captures: list[InstanceCapture],
) -> tuple[str, Tree]:
    """Remove instance requests from IML code."""
    instance_nodes = [capture.instance_statement for capture in captures]
    new_iml, new_tree = delete_nodes(iml, tree, nodes=instance_nodes)
    return new_iml, new_tree


def extract_instance_reqs(
    iml: str, tree: Tree
) -> tuple[str, Tree, list[VerifyReqArgs], list[Range]]:
    root = tree.root_node
    matches = run_query(
        mk_query(INSTANCE_QUERY_SRC),
        node=root,
    )

    instance_captures = [
        InstanceCapture.from_ts_capture(capture) for _, capture in matches
    ]

    req_and_range: list[tuple[VerifyReqArgs, Range]] = [
        instance_capture_to_req(capture) for capture in instance_captures
    ]
    if not req_and_range:
        return iml, tree, [], []
    else:
        reqs, ranges = zip(*req_and_range)
    new_iml, new_tree = _remove_instance_reqs(iml, tree, instance_captures)
    return new_iml, new_tree, list(reqs), list(ranges)


def insert_verify_req(
    iml: str,
    tree: Tree,
    verify_src: str,
) -> tuple[str, Tree]:
    if not (verify_src.startswith('(') and verify_src.endswith(')')):
        verify_src = f'({verify_src})'
    to_insert = f'verify {verify_src}'

    file_end_row = tree.root_node.end_point[0]

    new_iml, new_tree = insert_lines(
        iml,
        tree,
        lines=[to_insert],
        insert_after=file_end_row,
    )
    return new_iml, new_tree


def insert_instance_req(
    iml: str,
    tree: Tree,
    instance_src: str,
) -> tuple[str, Tree]:
    if not (instance_src.startswith('(') and instance_src.endswith(')')):
        instance_src = f'({instance_src})'
    to_insert = f'instance {instance_src}'

    file_end_row = tree.root_node.end_point[0]

    new_iml, new_tree = insert_lines(
        iml,
        tree,
        lines=[to_insert],
        insert_after=file_end_row,
    )
    return new_iml, new_tree

"""test requests related."""

from typing import Required, TypedDict, cast

from tree_sitter import Range, Tree

from iml_query.queries import (
    TEST_QUERY_SRC,
    TestCapture,
)
from iml_query.tree_sitter_utils import (
    delete_nodes,
    insert_lines,
    mk_query,
    run_query,
    unwrap_bytes,
)


class TestReqArgs(TypedDict):
    src: Required[str]
    hints: Required[str | None]


def test_capture_to_req(
    capture: TestCapture,
) -> tuple[TestReqArgs, Range]:
    req: dict[str, str | None] = {}
    if capture.test_attr is not None:
        req['hints'] = unwrap_bytes(capture.test_attr.text).decode('utf-8')
    else:
        req['hints'] = None

    expr_node = capture.test_expr
    src_raw = unwrap_bytes(expr_node.text).decode('utf-8')
    # Trim and remove parentheses in src
    src_trimmed = src_raw.strip()
    if src_trimmed.startswith('(') and src_trimmed.endswith(')'):
        src = src_trimmed[1:-1].strip()
    else:
        src = src_trimmed

    req['src'] = src
    return (cast(TestReqArgs, req), expr_node.range)


def _remove_test_reqs(
    iml: str,
    tree: Tree,
    captures: list[TestCapture],
) -> tuple[str, Tree]:
    """Remove test requests from IML code."""
    test_stmt_nodes = [capture.test_statement for capture in captures]
    new_iml, new_tree = delete_nodes(iml, tree, nodes=test_stmt_nodes)
    return new_iml, new_tree


def extract_test_reqs(
    iml: str, tree: Tree
) -> tuple[str, Tree, list[TestReqArgs], list[Range]]:
    root = tree.root_node
    matches = run_query(
        mk_query(TEST_QUERY_SRC),
        node=root,
    )

    test_captures = [
        TestCapture.from_ts_capture(capture) for _, capture in matches
    ]
    req_and_range: list[tuple[TestReqArgs, Range]] = [
        test_capture_to_req(capture) for capture in test_captures
    ]
    if not req_and_range:
        return iml, tree, [], []
    else:
        reqs, ranges = zip(*req_and_range)
    new_iml, new_tree = _remove_test_reqs(iml, tree, test_captures)
    return new_iml, new_tree, list(reqs), list(ranges)


def insert_test_req(
    iml: str,
    tree: Tree,
    test_src: str,
) -> tuple[str, Tree]:
    if not (test_src.startswith('(') and test_src.endswith(')')):
        test_src = f'({test_src})'
    to_insert = f'test {test_src}'

    file_end_row = tree.root_node.end_point[0]

    new_iml, new_tree = insert_lines(
        iml,
        tree,
        lines=[to_insert],
        insert_after=file_end_row,
    )
    return new_iml, new_tree

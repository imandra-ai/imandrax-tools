"""verify, instance, and decomp requests."""

from typing import Any

from tree_sitter import Tree

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


def verify_capture_to_req(capture: VerifyCapture) -> dict[str, str]:
    """Extract ImandraX request from a verify statement node."""
    node = capture.verify
    req: dict[str, str] = {}
    assert node.type == 'verify_statement', 'not verify_statement'
    assert node.text, 'None text'
    verify_src = (
        unwrap_bytes(node.text)
        .decode('utf-8')
        .strip()
        .removeprefix('verify')
        .strip()
    )
    # Remove parentheses
    if verify_src.startswith('(') and verify_src.endswith(')'):
        verify_src = verify_src[1:-1].strip()

    req['src'] = verify_src
    return req


def instance_capture_to_req(capture: InstanceCapture) -> dict[str, str]:
    """Extract ImandraX request from an instance statement node."""
    node = capture.instance
    req: dict[str, str] = {}
    assert node.type == 'instance_statement', 'not instance_statement'
    assert node.text, 'None text'
    instance_src = (
        unwrap_bytes(node.text)
        .decode('utf-8')
        .strip()
        .removeprefix('instance')
        .strip()
    )
    # Remove parentheses
    if instance_src.startswith('(') and instance_src.endswith(')'):
        instance_src = instance_src[1:-1].strip()
    req['src'] = instance_src
    return req


def remove_verify_reqs(
    iml: str,
    tree: Tree,
    captures: list[VerifyCapture],
) -> tuple[str, Tree]:
    """Remove verify requests from IML code."""
    verify_nodes = [capture.verify for capture in captures]
    new_iml, new_tree = delete_nodes(iml, tree, nodes=verify_nodes)
    return new_iml, new_tree


def extract_verify_reqs(
    iml: str, tree: Tree
) -> tuple[str, Tree, list[dict[str, Any]]]:
    root = tree.root_node
    matches = run_query(
        mk_query(VERIFY_QUERY_SRC),
        node=root,
    )

    verify_captures = [
        VerifyCapture.from_ts_capture(capture) for _, capture in matches
    ]
    reqs: list[dict[str, Any]] = [
        verify_capture_to_req(capture) for capture in verify_captures
    ]
    new_iml, new_tree = remove_verify_reqs(iml, tree, verify_captures)
    return new_iml, new_tree, reqs


def remove_instance_reqs(
    iml: str,
    tree: Tree,
    captures: list[InstanceCapture],
) -> tuple[str, Tree]:
    """Remove instance requests from IML code."""
    instance_nodes = [capture.instance for capture in captures]
    new_iml, new_tree = delete_nodes(iml, tree, nodes=instance_nodes)
    return new_iml, new_tree


def extract_instance_reqs(
    iml: str, tree: Tree
) -> tuple[str, Tree, list[dict[str, Any]]]:
    root = tree.root_node
    matches = run_query(
        mk_query(INSTANCE_QUERY_SRC),
        node=root,
    )

    instance_captures = [
        InstanceCapture.from_ts_capture(capture) for _, capture in matches
    ]

    reqs: list[dict[str, Any]] = [
        instance_capture_to_req(capture) for capture in instance_captures
    ]
    new_iml, new_tree = remove_instance_reqs(iml, tree, instance_captures)
    return new_iml, new_tree, reqs


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

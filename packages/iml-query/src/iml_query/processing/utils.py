"""Post-processing and manipulation functions for IML queries."""

from tree_sitter import Node, Tree

from iml_query.queries import (
    VALUE_DEFINITION_QUERY_SRC,
)
from iml_query.tree_sitter_utils import (
    mk_query,
    run_query,
    unwrap_bytes,
)


def find_func_definition(tree: Tree, function_name: str) -> Node | None:
    """
    Find the function definition node for a given function name.

    Args:
        tree: syntax tree of IML code
        function_name: name of function to find

    Returns:
        Node representing the function definition node, or None if not found

    """
    matches = run_query(
        mk_query(VALUE_DEFINITION_QUERY_SRC),
        node=tree.root_node,
    )

    func_def: Node | None = None
    for _, capture in matches:
        function_name_node = capture['function_name'][0]
        function_name_rhs = unwrap_bytes(function_name_node.text).decode(
            'utf-8'
        )
        if function_name_rhs == function_name:
            func_def = capture['function_definition'][0]
            break

    return func_def

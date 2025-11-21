from tree_sitter import Tree

from iml_query.queries import (
    VALUE_DEFINITION_QUERY_SRC,
    ValueDefCapture,
)
from iml_query.tree_sitter_utils import (
    delete_nodes,
    insert_lines,
    run_queries,
    unwrap_bytes,
)


def update_top_definition(
    iml: str,
    tree: Tree,
    top_def_name: str,
    new_definition: str,
    keep_previous_definition: bool = False,
) -> tuple[str, Tree]:
    """
    Update the definition of a top-level function.

    Append new definition without removing any existing definitions can be done
    by setting keep_previous_definition to True.

    Removing an existing definition can be done by using an empty string for
    new_definition and setting keep_previous_definition to True.

    Args:
        iml: input IML code
        tree: syntax tree of input IML code
        top_def_name: name of top-level function to update
        new_definition: new definition of top-level function
        keep_previous_definition: whether to keep the previous definition
            by default, the previous definition is replaced by the new
            definition

    Raise: ValueError
        - if input IML is invalid
        - if top-level function definition is not found
        - if updated IML is invalid

    """
    matches = run_queries(
        queries={'value_def': VALUE_DEFINITION_QUERY_SRC}, node=tree.root_node
    )
    value_defs: list[ValueDefCapture] = [
        ValueDefCapture.from_ts_capture(capture)
        for capture in matches['value_def']
    ]
    top_defs = [c for c in value_defs if c.is_top_level]
    matched_defs = [
        top_def
        for top_def in top_defs
        if (
            top_def_name
            == unwrap_bytes(top_def.function_name.text).decode('utf-8')
        )
    ]

    if len(matched_defs) == 0:
        raise ValueError(f'Function {top_def_name} not found in syntax tree')

    val_def: ValueDefCapture = matched_defs[0]

    func_def_node = val_def.function_definition
    func_def_start_row = func_def_node.start_point[0]
    func_def_end_row = func_def_node.end_point[0]

    # Remove previous definition if keep_previous_definition is False
    if keep_previous_definition:
        iml_1 = iml
        tree_1 = tree
        insert_after_line = func_def_end_row
        # When keeping previous definition, add trailing newline to separate
        add_trailing_newline = True
    else:
        iml_1, tree_1 = delete_nodes(iml, tree, nodes=[func_def_node])
        insert_after_line = func_def_start_row - 1
        # When replacing, add trailing newline if this will be the last
        # definition in the file (i.e., file will end with this definition)
        # Check if we're inserting after the last content
        last_line_with_content = -1
        for i, line in enumerate(iml_1.split('\n')):
            if line.strip():
                last_line_with_content = i
        add_trailing_newline = insert_after_line >= last_line_with_content

    if new_definition == '':
        return iml_1, tree_1
    else:
        iml_2, tree_2 = insert_lines(
            iml_1,
            tree_1,
            lines=[new_definition],
            insert_after=insert_after_line,
            ensure_trailing_newline=add_trailing_newline,
        )
        return iml_2, tree_2

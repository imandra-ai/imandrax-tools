"""Post-processing and manipulation functions for IML queries."""

from typing import Any

from tree_sitter import Node

from iml_query.tree_sitter_utils import (
    get_nesting_relationship,
    mk_query,
    run_query,
    unwrap_bytes,
)


def find_nested_measures(root_node: Node) -> list[dict[str, Any]]:
    """
    Find nested measures.

    Reurns:
        list of dicts with keys:
            function_name: name of top-level function
            level: nesting level of top-level function
            node: top-level function node
            range: range of top-level function node
            nested_measures: list of dicts with keys:
                function_name: name of nested function
                level: nesting level of nested function
                node: nested function node
                range: range of nested function node
    """
    # Query that finds both top-level functions and all functions with a measure
    combined_query = mk_query(r"""
    ; Find top-level functions
    (compilation_unit
        (value_definition
            (let_binding
                pattern: (value_name) @top_func_name
            )
        ) @top_function
    )

    ; Find functions with a measure attribute
    (value_definition
        (let_binding
            pattern: (value_name) @nested_func_name
            (item_attribute
                "[@@"
                (attribute_id) @_measure_id
                (#eq? @_measure_id "measure")
            )
        )
    ) @nested_function
    """)

    matches = run_query(combined_query, node=root_node)

    # Separate top-level functions from nested functions with measures
    top_level_functions: list[dict[str, Any]] = []
    nested_functions_with_measures: list[dict[str, Any]] = []

    for pattern_idx, capture in matches:
        if pattern_idx == 0:  # Top-level function pattern
            func_def = capture['top_function'][0]
            func_name = unwrap_bytes(capture['top_func_name'][0].text).decode(
                'utf-8'
            )
            top_level_functions.append(
                {
                    'name': func_name,
                    'node': func_def,
                    'range': func_def.range,
                }
            )
        elif pattern_idx == 1:  # Nested function with measure pattern
            nested_func = capture['nested_function'][0]
            nested_name = unwrap_bytes(
                capture['nested_func_name'][0].text
            ).decode('utf-8')
            nested_functions_with_measures.append(
                {
                    'name': nested_name,
                    'node': nested_func,
                    'range': nested_func.range,
                }
            )

    # Now match nested functions to their containing top-level functions
    problematic_functions: list[dict[str, Any]] = []

    for top_func_info in top_level_functions:
        top_func_node = top_func_info['node']
        nested_measures: list[dict[str, Any]] = []

        for nested_info in nested_functions_with_measures:
            nested_node = nested_info['node']

            # Get nesting relationship in a single traversal
            nesting_level = get_nesting_relationship(nested_node, top_func_node)

            # Only include if it's truly nested (level > 0)
            if nesting_level > 0:
                nested_measures.append(
                    {
                        'function_name': nested_info['name'],
                        'level': nesting_level,
                        'range': nested_info['range'],
                        'node': nested_node,
                    }
                )

        if nested_measures:
            problematic_functions.append(
                {
                    'top_level_function_name': top_func_info['name'],
                    'node': top_func_info['node'],
                    'range': top_func_info['range'],
                    'nested_measures': nested_measures,
                }
            )

    return problematic_functions

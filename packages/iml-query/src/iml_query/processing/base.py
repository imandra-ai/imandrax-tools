from typing import TypedDict

from iml_query.queries import (
    VALUE_DEFINITION_QUERY_SRC,
    ValueDefCapture,
)
from iml_query.tree_sitter_utils import (
    get_nesting_relationship,
    get_parser,
    run_queries,
)


class Nesting(TypedDict):
    """Represents a nesting relationship between two value definitions."""

    parent: ValueDefCapture
    child: ValueDefCapture
    nesting_level: int


def resolve_nesting_definitions(
    value_defs: list[ValueDefCapture],
) -> list[Nesting]:
    """Get nesting relationship between value definitions."""
    top_levels = [c for c in value_defs if c.is_top_level]
    non_top_levels = [c for c in value_defs if not c.is_top_level]

    nestings: list[Nesting] = []

    for non_top in non_top_levels:
        for top in top_levels:
            nesting_level = get_nesting_relationship(
                non_top.function_definition,
                top.function_definition,
            )
            match nesting_level:
                case -1:
                    pass  # No nesting
                case i if i > 0:
                    nestings.append(
                        Nesting(
                            parent=top,
                            child=non_top,
                            nesting_level=nesting_level,
                        )
                    )
                case 0:
                    raise AssertionError(
                        'Never: non-top level definition cannot be the same as '
                        'top level'
                    )
                case _ as unreachable:
                    raise AssertionError(f'Never: unreachable {unreachable}')
    return nestings


def find_nested_rec(iml: str) -> list[Nesting]:
    """
    Find nested recursive function definitions in IML code.

    Returns:
        a list of dictionary for the name and location of each function

    """
    tree = get_parser().parse(bytes(iml, 'utf-8'))
    queries = {
        'value_def': VALUE_DEFINITION_QUERY_SRC,
    }
    captures_map = run_queries(queries, tree.root_node)
    val_captures: list[ValueDefCapture] = [
        ValueDefCapture.from_ts_capture(capture)
        for capture in captures_map.get('value_def', [])
    ]
    nestings = resolve_nesting_definitions(val_captures)
    nestings = [n for n in nestings if n['child'].is_rec]
    return nestings

"""Tests for IML import statement parsing."""

from inline_snapshot import snapshot
from tree_sitter import Node

from iml_query.queries import (
    IMPORT_NAMED_PATH_QUERY_SRC,
    IMPORT_PATH_ONLY_QUERY_SRC,
    ImportCapture,
)
from iml_query.tree_sitter_utils import run_queries, unwrap_bytes


def parse(code: str) -> list[ImportCapture]:
    queries = {
        'path_only': IMPORT_PATH_ONLY_QUERY_SRC,
        'named': IMPORT_NAMED_PATH_QUERY_SRC,
    }
    matches = run_queries(queries, code=code)
    ts_captures: list[dict[str, list[Node]]] = [
        item for sublist in matches.values() for item in sublist
    ]
    return [
        ImportCapture.from_ts_capture(ts_capture) for ts_capture in ts_captures
    ]


def pp_ts_node(node: Node) -> str:
    return str(unwrap_bytes(node.text), encoding='utf-8')


def pp_import_capture(capture: ImportCapture) -> dict[str, str | None]:
    return {
        'import_stmt': pp_ts_node(capture.import_stmt),
        'import_path': pp_ts_node(capture.import_path),
        'import_name': pp_ts_node(capture.import_name)
        if capture.import_name
        else None,
    }


def test():
    iml = """
    [@@@import "utils.iml"]
    [@@@import "utils2.iml"]
    [@@@import Helpers, "helpers.iml"]
    [@@@import Foo, "findlib:foo.bar"]
    [@@@import Foo, "dune:foo.bar"]
    """

    res = [pp_import_capture(capture) for capture in parse(iml)]

    assert res == snapshot(
        [
            {
                'import_stmt': '[@@@import "utils.iml"]',
                'import_path': 'utils.iml',
                'import_name': None,
            },
            {
                'import_stmt': '[@@@import "utils2.iml"]',
                'import_path': 'utils2.iml',
                'import_name': None,
            },
            {
                'import_stmt': '[@@@import Helpers, "helpers.iml"]',
                'import_path': 'helpers.iml',
                'import_name': 'Helpers',
            },
            {
                'import_stmt': '[@@@import Foo, "findlib:foo.bar"]',
                'import_path': 'findlib:foo.bar',
                'import_name': 'Foo',
            },
            {
                'import_stmt': '[@@@import Foo, "dune:foo.bar"]',
                'import_path': 'dune:foo.bar',
                'import_name': 'Foo',
            },
        ]
    )

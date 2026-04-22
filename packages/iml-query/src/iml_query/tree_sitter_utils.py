"""General tree-sitter utilities (language-agnostic)."""

from collections import defaultdict
from functools import cache
from typing import Literal, cast, overload

import structlog
import tree_sitter_iml
from tree_sitter import Language, Node, Parser, Query, QueryCursor, Tree

logger = structlog.get_logger(__name__)


@cache
def _get_language(lang: Literal['ocaml', 'iml'] = 'iml') -> Language:
    """Get the tree-sitter language for the given language."""
    if lang == 'ocaml':
        language_capsule = tree_sitter_iml.language_ocaml()
    else:
        language_capsule = tree_sitter_iml.language_iml()
    return Language(language_capsule)


def create_parser(ocaml: bool = False) -> Parser:
    """Get a parser for the given language."""
    language = _get_language('ocaml' if ocaml else 'iml')

    parser = Parser()
    parser.language = language
    return parser


_parser: Parser | None = None


def get_parser() -> Parser:
    """Get the global IML tree-sitter parser instance."""
    global _parser
    if _parser is None:
        _parser = create_parser()
    return _parser


def mk_query(query_src: str) -> Query:
    """Create a Tree-sitter query from the given source."""
    return Query(_get_language('iml'), query_src)


def run_query(
    query: Query | str,
    *,
    code: str | bytes | None = None,
    node: Node | None = None,
) -> list[tuple[int, dict[str, list[Node]]]]:
    """
    Run a Tree-sitter query on the given code or node.

    Return:
        A list of tuples where
            - 0: the first element is the pattern index
            - 1: the second element is a dictionary that maps capture names to nodes.

    """
    if isinstance(query, str):
        query = mk_query(query)

    if code is None == node is None:
        raise ValueError('Exactly one of code or node must be provided')
    if code is not None:
        if isinstance(code, str):
            code = bytes(code, 'utf8')
        parser = create_parser(ocaml=False)
        tree = parser.parse(code)
        node = tree.root_node
    node = cast(Node, node)

    cursor = QueryCursor(query=query)
    return cursor.matches(node)


def _merge_queries(queries: dict[str, str]) -> str:
    """
    Merge multiple queries into one query.

    Args:
        queries (dict[str, str]): A dictionary of query names to queries.
            Each query can only contains one pattern.

    Returns:
        str: The merged query.

    """
    s = ''
    for name, query in queries.items():
        if __debug__:
            assert mk_query(query).pattern_count == 1, (
                'expected exactly one pattern'
            )
        s += f'; {name}\n'
        s += query
        s += '\n\n'
    return s


def run_queries(
    queries: dict[str, str],
    *,
    code: str | bytes | None = None,
    node: Node | None = None,
) -> dict[str, list[dict[str, list[Node]]]]:
    """
    Run multiple queries.

    Returns:
        (dict[str, list[dict[str, list[Node]]]]): map from query names to
            a list of captures. Each capture is a dictionary of capture names to
            capture values.

    """
    if code is None == node is None:
        raise ValueError('Exactly one of code or node must be provided')
    if code is not None:
        if isinstance(code, str):
            code = bytes(code, 'utf8')
        parser = create_parser(ocaml=False)
        tree = parser.parse(code)
        node = tree.root_node
    node = cast(Node, node)

    matches = run_query(
        mk_query(_merge_queries(queries)),
        node=node,
    )

    # query name -> list of captures
    captures_map: dict[str, list[dict[str, list[Node]]]] = defaultdict(list)
    query_names = list(queries.keys())
    for patten_idx, capture in matches:
        query_name = query_names[patten_idx]
        captures_map[query_name].append(capture)

    return dict(captures_map)


def unwrap_bytes(node_text: bytes | None) -> bytes:
    if node_text is None:
        raise ValueError('Node text is None')
    return node_text


def get_nesting_relationship(nested_node: Node, top_level_node: Node) -> int:
    """
    Get nesting relationship between two nodes.

    Returns:
        -1: nested_node is not contained within top_level_node
         0: nested_node is the same as top_level_node
         n > 0: nested_node is nested within top_level_node at level n

    """
    if nested_node == top_level_node:
        return 0

    # Early exit if the nested node is not contained within the top level node
    if (
        nested_node.end_byte < top_level_node.start_byte
        or nested_node.start_byte > top_level_node.end_byte
    ):
        return -1

    level = 0
    current = nested_node.parent

    while current:
        if current == top_level_node:
            return level
        # Count let_expressions as nesting levels
        if current.type == 'let_expression':
            level += 1
        current = current.parent

    return -1  # Range contains but not satisfies let structure


# Delete and insert nodes
# ====================


@overload
def delete_nodes(
    iml: str,
    old_tree: Tree,
    *,
    nodes: list[Node],
) -> tuple[str, Tree]: ...


@overload
def delete_nodes(
    iml: str,
    *,
    nodes: list[Node],
) -> tuple[str, None]: ...


def delete_nodes(
    iml: str,
    old_tree: Tree | None = None,
    *,
    nodes: list[Node],
) -> tuple[str, Tree | None]:
    """
    Delete nodes from IML string and return updated string and tree.

    Return new tree if old_tree is provided.

    Arguments:
        nodes: list of nodes to delete
        iml: old IML code
        old_tree: old parsed tree

    """
    if not nodes:
        return iml, old_tree

    # Extract byte ranges from nodes
    edits = [node.byte_range for node in nodes]

    # Check for overlapping edits
    sorted_edits = sorted(edits, key=lambda x: x[0])
    for i in range(len(sorted_edits) - 1):
        curr_end = sorted_edits[i][1]
        next_start = sorted_edits[i + 1][0]
        if curr_end > next_start:
            raise ValueError(
                f'Overlapping nodes: positions {sorted_edits[i]} and '
                f'{sorted_edits[i + 1]}'
            )

    # Apply deletions to text in reverse order to avoid offset issues
    edits_reversed = sorted(edits, key=lambda x: x[0], reverse=True)
    iml_b = bytes(iml, encoding='utf8')
    for start, end in edits_reversed:
        iml_b = iml_b[:start] + iml_b[end:]
    iml = iml_b.decode('utf8')

    # Get new tree
    # Apply tree edits if we have an old tree
    if old_tree is not None:
        old_tree = old_tree.copy()

        # Sort nodes by start position for tree editing
        sorted_nodes = sorted(nodes, key=lambda x: x.start_byte)

        # Apply tree edits in forward order
        for node in sorted_nodes:
            old_tree.edit(
                start_byte=node.start_byte,
                old_end_byte=node.end_byte,
                new_end_byte=node.start_byte,
                start_point=node.start_point,
                old_end_point=node.end_point,
                new_end_point=node.start_point,
            )

        parser = create_parser(ocaml=False)
        new_tree = parser.parse(iml_b, old_tree=old_tree)
    else:
        new_tree = None

    return iml, new_tree


def insert_lines(
    code: str,
    tree: Tree,
    lines: list[str],
    insert_after: int,
    ensure_trailing_newline: bool = True,
) -> tuple[str, Tree]:
    r"""
    Insert lines of code after the given line number.

    AI: this is implemented by AI

    Arguments:
        code: old code
        tree: old parsed tree
        lines: list of lines to insert (without trailing newlines)
        insert_after: line number to insert after
            (0-based, must be < len(lines))
        ensure_trailing_newline: whether to ensure that the there's a trailing
            newline at the end of the code.

    Returns:
        new, modified code and new tree

    Implementation notes:
        Leading newline handling:
            When using splitlines(keepends=True), only the last line may lack
            a trailing newline (if the original string doesn't end with '\n').
            If inserting after such a line, we must prepend '\n' to separate
            the existing line from the inserted content.

        Tree edit point calculation:
            Edit points are (row, col) tuples where col is the byte offset.

            Case 1 - Last line without trailing newline:
                start_point = (insert_after, byte_len_of_line)
                Insertion happens at the end of the current line.

            Case 2 - Normal line with trailing newline:
                start_point = (insert_after + 1, 0)
                Insertion happens at the beginning of the next line.

            For both cases:
                old_end_point = start_point (zero-width insertion)
                new_end_point = (start_row + num_newlines, 0)

    """
    if not lines:
        return code, tree

    tree = tree.copy()

    # Split into lines to find insertion point
    # TODO: use line info in tree to determine line number
    iml_lines = code.splitlines(keepends=True)

    # Validate line number
    # Allow insert_after == len(iml_lines) when last line ends with \n
    # (tree.root_node.end_point can point to the line after the last)
    max_insert_after = len(iml_lines) - 1
    if iml_lines and iml_lines[-1].endswith('\n'):
        max_insert_after = len(iml_lines)

    if insert_after < -1 or insert_after > max_insert_after:
        raise ValueError(
            f'Line number {insert_after} out of range (0-{max_insert_after})'
        )

    # Calculate byte position for insertion
    # Find the end of the line we're inserting after
    if insert_after == -1:
        # Inserting at the start of the file
        insert_byte_pos = 0
        need_leading_newline = False
    elif insert_after >= len(iml_lines):
        # Inserting after the last line (when file ends with \n)
        insert_byte_pos = sum(len(line.encode('utf-8')) for line in iml_lines)
        need_leading_newline = False  # Last line already has \n
    else:
        lines_before = iml_lines[: insert_after + 1]
        insert_byte_pos = sum(
            len(line.encode('utf-8')) for line in lines_before
        )
        # Check if we need to add a leading newline
        # (the last line might not end with "\n", so which case we need to add
        # it)
        need_leading_newline = not iml_lines[insert_after].endswith('\n')

    # Prepare the text to insert (ensure lines end with newlines)
    insert_text = '\n'.join(lines)
    if ensure_trailing_newline:
        insert_text += '\n'
    if need_leading_newline:
        insert_text = '\n' + insert_text

    insert_bytes = insert_text.encode('utf-8')
    insert_length = len(insert_bytes)

    # Calculate tree edit points
    if insert_after >= len(iml_lines):
        # Inserting after all lines (file ends with \n)
        start_row = insert_after
        start_col = 0
    elif need_leading_newline:
        # Inserting at the end of line insert_after (no trailing newline)
        start_row = insert_after
        start_col = len(iml_lines[insert_after].encode('utf-8'))
    else:
        # Inserting at the start of the next line
        start_row = insert_after + 1
        start_col = 0

    start_point = (start_row, start_col)
    old_end_point = start_point  # For insertion, old_end = start

    # Calculate end point based on number of newlines inserted
    num_newlines = insert_text.count('\n')
    new_end_point = (start_row + num_newlines, 0)

    # Apply tree edit
    tree.edit(
        start_byte=insert_byte_pos,
        old_end_byte=insert_byte_pos,  # Insertion: old_end = start
        new_end_byte=insert_byte_pos + insert_length,
        start_point=start_point,
        old_end_point=old_end_point,
        new_end_point=new_end_point,
    )

    # Apply text insertion
    code_bytes = code.encode('utf-8')
    new_code_bytes = (
        code_bytes[:insert_byte_pos]
        + insert_bytes
        + code_bytes[insert_byte_pos:]
    )
    new_code = new_code_bytes.decode('utf-8')

    # Parse new tree
    parser = create_parser(ocaml=False)
    new_tree = parser.parse(new_code_bytes, old_tree=tree)

    return new_code, new_tree


# Pretty-printing
# ====================


def fmt_node(
    node: Node | Tree,
    *,
    anonymous_text: bool = True,
    leaf_text: bool = True,
    max_depth: int | None = None,
) -> str:
    """
    Pretty-print a tree-sitter node as an S-expression.

    Shows field names, and optionally literal text for anonymous and leaf nodes.

    Args:
        node: the node to print
        anonymous_text: show literal text for anonymous nodes (e.g. "let", ":")
        leaf_text: show literal text for named leaf nodes (e.g. value_name "int")
        max_depth: maximum depth to recurse into

    """
    lines: list[str] = []
    _fmt_node_impl(node, lines, 0, max_depth, anonymous_text, leaf_text)
    return '\n'.join(lines)


# Keep old names as aliases
fmt_node_with_leaf_text = fmt_node
fmt_node_with_field_name = fmt_node


def _fmt_node_impl(
    node: Node | Tree,
    lines: list[str],
    depth: int,
    max_depth: int | None,
    anonymous_text: bool,
    leaf_text: bool,
) -> None:
    if isinstance(node, Tree):
        node = node.root_node

    if max_depth is not None and depth > max_depth:
        return

    indent = '  ' * depth
    is_leaf = not node.children

    if not node.is_named:
        # Anonymous node — show literal text in quotes
        if anonymous_text:
            text = unwrap_bytes(node.text).decode('utf-8')
            lines.append(f'{indent}"{text}"')
        return

    if is_leaf:
        # Named leaf node
        if leaf_text and node.text:
            text = unwrap_bytes(node.text).decode('utf-8')
            lines.append(f'{indent}({node.type} "{text}")')
        else:
            lines.append(f'{indent}({node.type})')
        return

    # Named node with children
    lines.append(f'{indent}({node.type}')
    for i, child in enumerate(node.children):
        field_name = node.field_name_for_child(i)
        if field_name:
            # Collect child lines, then prefix the first one with field name
            child_lines: list[str] = []
            _fmt_node_impl(
                child,
                child_lines,
                depth + 1,
                max_depth,
                anonymous_text,
                leaf_text,
            )
            if child_lines:
                first = child_lines[0].lstrip()
                child_lines[0] = f'{"  " * (depth + 1)}{field_name}: {first}'
                lines.extend(child_lines)
        else:
            _fmt_node_impl(
                child,
                lines,
                depth + 1,
                max_depth,
                anonymous_text,
                leaf_text,
            )
    lines.append(f'{indent})')

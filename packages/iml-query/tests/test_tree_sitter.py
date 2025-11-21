# pyright: basic
from inline_snapshot import snapshot

from iml_query.tree_sitter_utils import (
    get_nesting_relationship,
    get_parser,
    insert_lines,
    mk_query,
    run_query,
    unwrap_bytes,
)


def test_get_nesting_relationship():
    """Test nesting relationship detection with complex nested structure."""
    iml = """\
let triple_nested (f : int list) (i : int) (n : int) : int list =
  let rec outer_helper curr_f curr_i =
    let rec inner_helper curr_f curr_i =
      if curr_i > n then
        curr_f
      else
        let rec deepest_helper x =
          if x = 0 then curr_f
          else deepest_helper (x - 1)
        [@@measure Ordinal.of_int x]
        in
        deepest_helper curr_i
    [@@measure Ordinal.of_int (n - curr_i)]
    in
    inner_helper curr_f curr_i
  [@@measure Ordinal.of_int (n - curr_i)]
  in
  outer_helper f i

let top_level_function x = x + 1
[@@measure Ordinal.of_int 1]
"""
    parser = get_parser()
    tree = parser.parse(bytes(iml, encoding='utf8'))

    # Find all value definitions
    value_def_query = mk_query(r"""
    (value_definition
        (let_binding
            pattern: (value_name) @func_name
        )
    ) @function
    """)

    matches = run_query(value_def_query, node=tree.root_node)

    # Build a map of function names to nodes
    functions = {}
    for _, capture in matches:
        func_name = unwrap_bytes(capture['func_name'][0].text).decode('utf-8')
        func_node = capture['function'][0]
        functions[func_name] = func_node

    # Test various nesting relationships
    triple_nested = functions['triple_nested']
    top_level = functions['top_level_function']

    # Find all nested functions
    nested_funcs = []
    for name, node in functions.items():
        if name not in ['triple_nested', 'top_level_function']:
            nested_funcs.append((name, node))

    # Test nesting levels
    relationships = {}
    for name, nested_node in nested_funcs:
        # Test relationship to triple_nested
        level_to_triple = get_nesting_relationship(nested_node, triple_nested)
        # Test relationship to top_level (should be -1, not nested)
        level_to_top = get_nesting_relationship(nested_node, top_level)
        # Test relationship to itself (should be 0)
        level_to_self = get_nesting_relationship(nested_node, nested_node)

        relationships[name] = {
            'to_triple_nested': level_to_triple,
            'to_top_level': level_to_top,
            'to_self': level_to_self,
        }

    assert relationships == snapshot(
        {
            'outer_helper': {
                'to_triple_nested': 1,
                'to_top_level': -1,
                'to_self': 0,
            },
            'inner_helper': {
                'to_triple_nested': 2,
                'to_top_level': -1,
                'to_self': 0,
            },
            'deepest_helper': {
                'to_triple_nested': 3,
                'to_top_level': -1,
                'to_self': 0,
            },
        }
    )


def test_insert_lines_without_trailing_newline():
    """Test insert_lines when the last line lacks a trailing newline."""
    iml = """\
let g (x : int) : int =
  if x > 22 then
    9
  else
    100 + x

let f (x : int) : int =
  if x > 99 then
    100
  else if 70 > x && x > 23 then
    89 + x
  else if x > 20 then
    g x + 20
  else if x > -2 then
    103
  else
    99"""  # Note: no trailing newline
    parser = get_parser()
    tree = parser.parse(bytes(iml, encoding='utf8'))

    # Insert after the last line
    file_end_row = tree.root_node.end_point[0]
    req_str = 'verify (fun x -> f x > 0)'
    new_iml, new_tree = insert_lines(
        code=iml, tree=tree, lines=[req_str], insert_after=file_end_row
    )

    # Should have a newline separating the last line and the inserted line
    assert new_iml == snapshot("""\
let g (x : int) : int =
  if x > 22 then
    9
  else
    100 + x

let f (x : int) : int =
  if x > 99 then
    100
  else if 70 > x && x > 23 then
    89 + x
  else if x > 20 then
    g x + 20
  else if x > -2 then
    103
  else
    99
verify (fun x -> f x > 0)
""")

    # Verify the tree is valid and parses correctly
    assert not new_tree.root_node.has_error


def test_insert_lines_multiple_consecutive():
    """Test multiple consecutive insert_lines to verify tree validity."""
    iml = """\
let x = 1
let y = 2"""  # No trailing newline
    parser = get_parser()
    tree = parser.parse(bytes(iml, encoding='utf8'))

    # First insertion after line 1
    iml2, tree2 = insert_lines(iml, tree, lines=['let z = 3'], insert_after=1)
    assert iml2 == snapshot("""\
let x = 1
let y = 2
let z = 3
""")
    assert not tree2.root_node.has_error

    # Second insertion after line 2 (using the new tree)
    iml3, tree3 = insert_lines(iml2, tree2, lines=['let w = 4'], insert_after=2)
    assert iml3 == snapshot("""\
let x = 1
let y = 2
let z = 3
let w = 4
""")
    assert not tree3.root_node.has_error

    # Third insertion in the middle (line 1)
    iml4, tree4 = insert_lines(iml3, tree3, lines=['let a = 0'], insert_after=1)
    assert iml4 == snapshot("""\
let x = 1
let y = 2
let a = 0
let z = 3
let w = 4
""")
    assert not tree4.root_node.has_error


def test_insert_lines_out_of_bounds():
    """Test insert_lines raises ValueError for out-of-bounds line numbers."""
    iml = """\
let x = 1
let y = 2
"""
    parser = get_parser()
    tree = parser.parse(bytes(iml, encoding='utf8'))

    # Should raise for insert_after > max_valid
    # File has 2 lines with trailing \n, so insert_after=2 is valid
    # but insert_after=3 should be out of bounds
    try:
        insert_lines(iml, tree, lines=['let z = 3'], insert_after=3)
        raise AssertionError('Expected ValueError')
    except ValueError as e:
        assert 'out of range' in str(e)

    # Also test with negative line number lower than -1
    try:
        insert_lines(iml, tree, lines=['let z = 3'], insert_after=-2)
        raise AssertionError('Expected ValueError')
    except ValueError as e:
        assert 'out of range' in str(e)

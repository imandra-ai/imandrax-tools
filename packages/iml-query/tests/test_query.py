from inline_snapshot import snapshot

from iml_query.queries import (
    DECOMP_QUERY_SRC,
    INSTANCE_QUERY_SRC,
    VERIFY_QUERY_SRC,
    InstanceCapture,
    VerifyCapture,
)
from iml_query.tree_sitter_utils import (
    get_parser,
    mk_query,
    run_query,
    unwrap_bytes,
)


def test_complex_decomp_with_composition():
    """Test complex decomp parsing with composition operators."""
    iml = """\
let base_function x =
  if x mod 3 = 0 then 0
  else if x mod 3 = 1 then 1
  else 2
[@@decomp top ()]

let dependent_function x =
  let base_result = base_function x in
  if base_result = 0 then x / 3
  else if base_result = 1 then x + 1
  else x - 1

let merged_decomposition = dependent_function
[@@decomp top ~basis:[[%id base_function]] () << top () [%id base_function]]

let compound_merged = dependent_function
[@@decomp top ~basis:[[%id base_function]] () <|< top () [%id base_function]]

let redundant_regions x =
  if x > 0 then 1
  else if x < -10 then 1
  else if x = 0 then 0
  else 1
[@@decomp ~| (top ())]
"""
    parser = get_parser()
    tree = parser.parse(bytes(iml, encoding='utf8'))

    # Find all decomp attributes
    matches = run_query(mk_query(DECOMP_QUERY_SRC), node=tree.root_node)

    # Count how many decomp attributes we found
    decomp_count = len(matches)
    assert decomp_count == snapshot(4)

    # Test that we can identify the function names
    func_names: list[str] = []
    for _, capture in matches:
        if 'decomposed_func_name' in capture:
            name = unwrap_bytes(capture['decomposed_func_name'][0].text).decode(
                'utf-8'
            )
            func_names.append(name)

    assert func_names == snapshot(
        [
            'base_function',
            'merged_decomposition',
            'compound_merged',
            'redundant_regions',
        ]
    )


def test_edge_cases_empty_content():
    """Test edge cases with minimal or empty content."""
    # Test with just comments
    iml_comments = """\
(* This is just a comment *)
(* Another comment *)
"""
    parser = get_parser()
    tree = parser.parse(bytes(iml_comments, encoding='utf8'))

    # Should find no verify statements
    matches = run_query(mk_query(VERIFY_QUERY_SRC), node=tree.root_node)
    assert len(matches) == 0

    # Test with just a simple expression
    iml_simple = 'let x = 42'
    tree_simple = parser.parse(bytes(iml_simple, encoding='utf8'))
    matches_simple = run_query(
        mk_query(VERIFY_QUERY_SRC), node=tree_simple.root_node
    )
    assert len(matches_simple) == 0


def test_statements_with_attributes():
    """Test that verify and instance queries exclude item attributes."""
    iml = """\
verify (fun x y -> x > 0) [@@by auto]
instance (fun x -> x > 0) [@@by auto]
"""
    parser = get_parser()
    tree = parser.parse(bytes(iml, encoding='utf8'))

    # Test VERIFY query
    verify_matches = run_query(mk_query(VERIFY_QUERY_SRC), node=tree.root_node)
    assert len(verify_matches) == 1
    verify_capture = VerifyCapture.from_ts_capture(verify_matches[0][1])

    # verify_statement should include the attribute
    assert (
        verify_capture.verify_statement.text
        == b'verify (fun x y -> x > 0) [@@by auto]'
    )
    # verify_expr should exclude the attribute
    assert verify_capture.verify_expr.text == b'(fun x y -> x > 0)'

    # Test INSTANCE query
    instance_matches = run_query(
        mk_query(INSTANCE_QUERY_SRC), node=tree.root_node
    )
    assert len(instance_matches) == 1
    instance_capture = InstanceCapture.from_ts_capture(instance_matches[0][1])

    # instance_statement should include the attribute
    assert (
        instance_capture.instance_statement.text
        == b'instance (fun x -> x > 0) [@@by auto]'
    )
    # instance_expr should exclude the attribute
    assert instance_capture.instance_expr.text == b'(fun x -> x > 0)'

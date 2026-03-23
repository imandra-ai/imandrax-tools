"""Test cases for extract_type_decl_names function."""

from inline_snapshot import snapshot

from iml_query.processing import extract_type_decl_names


def test_simple_type():
    """Test simple type without parameters."""
    iml = """\
type direction = North | South | East | West
"""
    types = extract_type_decl_names(iml)
    assert types == ['direction'], f"Expected ['direction'], got {types}"


def test_single_param_type():
    """Test type with single parameter."""
    iml = """\
type 'a option = None | Some of 'a
"""
    types = extract_type_decl_names(iml)
    assert types == ['option'], f"Expected ['option'], got {types}"
    print('✓ test_single_param_type passed')


def test_multi_param_type():
    """Test type with multiple parameters."""
    iml = """\
type ('a, 'b) container =
    | Empty
    | Single of 'a
    | Pair of 'a * 'b
    | Labeled of { key: 'a; value: 'b }
    | Multi of 'a list * 'b list
"""
    types = extract_type_decl_names(iml)
    assert types == ['container'], f"Expected ['container'], got {types}"


def test_mutually_recursive_types():
    """Test mutually recursive type definitions."""
    iml = """\
type tree = Leaf | Node of node_data
and node_data = { value: int; left: tree; right: tree }
"""
    types = extract_type_decl_names(iml)
    assert types == ['tree', 'node_data'], (
        f"Expected ['tree', 'node_data'], got {types}"
    )


def test_multiple_types():
    """Test multiple separate type definitions."""
    iml = """\
type color = Red | Green | Blue

type 'a list_wrapper = List of 'a list

type ('k, 'v) map = Empty | Node of 'k * 'v * ('k, 'v) map * ('k, 'v) map
"""
    types = extract_type_decl_names(iml)
    assert types == ['color', 'list_wrapper', 'map'], (
        f"Expected ['color', 'list_wrapper', 'map'], got {types}"
    )


def test_record_type():
    """Test record type definition."""
    iml = """\
type person = { name: string; age: int; email: string }
"""
    types = extract_type_decl_names(iml)
    assert types == ['person'], f"Expected ['person'], got {types}"


def test_type_alias():
    """Test type alias."""
    iml = """\
type int_pair = int * int
type string_list = string list
"""
    types = extract_type_decl_names(iml)
    assert types == ['int_pair', 'string_list'], (
        f"Expected ['int_pair', 'string_list'], got {types}"
    )


def test_complex_param_type():
    """Test type with complex parameters."""
    iml = """\
type ('a, 'b, 'c) triple = { first: 'a; second: 'b; third: 'c }
"""
    types = extract_type_decl_names(iml)
    assert types == ['triple'], f"Expected ['triple'], got {types}"


def test_gadt_simple():
    """Test simple GADT (Generalized Algebraic Data Type)."""
    iml = """\
type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Add : int expr * int expr -> int expr
  | Eq : 'a expr * 'a expr -> bool expr
"""
    types = extract_type_decl_names(iml)
    assert types == ['expr'], f"Expected ['expr'], got {types}"


def test_gadt_with_params():
    """Test GADT with type parameters."""
    iml = """\
type (_, _) vec =
  | Nil : ('a, 'z) vec
  | Cons : 'a * ('a, 'n) vec -> ('a, 'n succ) vec
"""
    types = extract_type_decl_names(iml)
    assert types == ['vec'], f"Expected ['vec'], got {types}"


def test_gadt_complex():
    """Test complex GADT with existential types."""
    iml = """\
type _ term =
  | Lit : int -> int term
  | Pair : 'a term * 'b term -> ('a * 'b) term
  | App : ('a -> 'b) term * 'a term -> 'b term
  | Lam : ('a -> 'b term) -> ('a -> 'b) term
"""
    types = extract_type_decl_names(iml)
    assert types == ['term'], f"Expected ['term'], got {types}"


def test_extract_type_decl_names():
    """Test extracting type declaration names from IML source."""
    iml = """\
type market =
  | ISM
  | HGS

type issuer = {
  issuer_id : string;
  name : string;
}

(* This comment mentions type and market but should not be extracted *)

let all_issuers (st : programme_state) : issuer list = []

type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree
"""
    names = extract_type_decl_names(iml)
    assert names == snapshot(['market', 'issuer', 'tree'])


def test_extract_type_decl_names_ignores_comments():
    """
    Test that type extraction ignores words in comments.

    Regression test: a regex-based approach would incorrectly match
    'and <word>' patterns inside comments.
    """
    iml = """\
type direction = North | South

(* Issuer and Market are linked together and system validates them *)
(* WHEN user clicks "x" and the new issuer confirms deletion *)

type colour = Red | Blue
"""
    names = extract_type_decl_names(iml)
    assert names == snapshot(['direction', 'colour'])

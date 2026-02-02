"""Test cases for extract_type_decl_names function."""

from imandrax_codegen.gen_tests import (
    _extract_type_decl_names,  # pyright: ignore[reportPrivateUsage]
)


def test_simple_type():
    """Test simple type without parameters."""
    iml = """\
type direction = North | South | East | West
"""
    types = _extract_type_decl_names(iml)
    assert types == ['direction'], f"Expected ['direction'], got {types}"


def test_single_param_type():
    """Test type with single parameter."""
    iml = """\
type 'a option = None | Some of 'a
"""
    types = _extract_type_decl_names(iml)
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
    types = _extract_type_decl_names(iml)
    assert types == ['container'], f"Expected ['container'], got {types}"


def test_mutually_recursive_types():
    """Test mutually recursive type definitions."""
    iml = """\
type tree = Leaf | Node of node_data
and node_data = { value: int; left: tree; right: tree }
"""
    types = _extract_type_decl_names(iml)
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
    types = _extract_type_decl_names(iml)
    assert types == ['color', 'list_wrapper', 'map'], (
        f"Expected ['color', 'list_wrapper', 'map'], got {types}"
    )


def test_record_type():
    """Test record type definition."""
    iml = """\
type person = { name: string; age: int; email: string }
"""
    types = _extract_type_decl_names(iml)
    assert types == ['person'], f"Expected ['person'], got {types}"


def test_type_alias():
    """Test type alias."""
    iml = """\
type int_pair = int * int
type string_list = string list
"""
    types = _extract_type_decl_names(iml)
    assert types == ['int_pair', 'string_list'], (
        f"Expected ['int_pair', 'string_list'], got {types}"
    )


def test_complex_param_type():
    """Test type with complex parameters."""
    iml = """\
type ('a, 'b, 'c) triple = { first: 'a; second: 'b; third: 'c }
"""
    types = _extract_type_decl_names(iml)
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
    types = _extract_type_decl_names(iml)
    assert types == ['expr'], f"Expected ['expr'], got {types}"


def test_gadt_with_params():
    """Test GADT with type parameters."""
    iml = """\
type (_, _) vec =
  | Nil : ('a, 'z) vec
  | Cons : 'a * ('a, 'n) vec -> ('a, 'n succ) vec
"""
    types = _extract_type_decl_names(iml)
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
    types = _extract_type_decl_names(iml)
    assert types == ['term'], f"Expected ['term'], got {types}"

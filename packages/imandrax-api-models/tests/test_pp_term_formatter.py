"""Term printer output is IML syntax ImandraX can parse back."""

from imandrax_api_models.pp.term_builders import (
    app,
    const_int,
    construct,
    tuple_,
    var,
)
from imandrax_api_models.pp.term_formatter import prettify


def pp(t) -> str:
    return prettify(t, width=200)


# Constructors
# ============


def test_constructor_no_args():
    assert pp(construct('S1', [])) == 'S1'


def test_constructor_one_arg():
    assert pp(construct('Some', [var('x')])) == 'Some x'


def test_constructor_one_arg_application_is_parenthesised():
    assert pp(construct('Some', [app('f', [var('x')])])) == 'Some (f x)'


def test_constructor_several_args_is_a_tuple():
    t = construct('Set', [construct('S1', []), var('a'), app('f', [var('b')])])
    assert pp(t) == 'Set (S1, a, f b)'


def test_constructor_tuple_arg_stays_one_element():
    t = construct('Pair', [tuple_([const_int(1), const_int(2)]), var('x')])
    assert pp(t) == 'Pair ((1, 2), x)'


def test_constructor_in_cons():
    t = construct('::', [construct('Set', [var('a'), var('b')]), construct('[]', [])])
    assert pp(t) == 'Set (a, b) :: []'

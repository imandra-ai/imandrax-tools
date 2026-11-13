"""Term printer output is IML syntax ImandraX can parse back."""

from imandrax_api_models.pp.term_builders import (
    app,
    const_int,
    construct,
    destruct,
    field,
    is_a,
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


# Is_a and Destruct
# =================
# Printed as ImandraX prints them in region strings.


def test_is_a():
    assert pp(is_a('Set', var('e'))) == 'Is_a(Set, e)'


def test_is_a_under_not_is_parenthesised():
    assert pp(app('not', [is_a('Add', var('e'))])) == 'not (Is_a(Add, e))'


def test_is_a_in_conjunction():
    t = app('&&', [is_a('Set', var('e')), app('not', [var('b')])])
    assert pp(t) == 'Is_a(Set, e) && not b'


def test_destruct():
    assert pp(destruct('Set', 1, var('e'))) == 'Destruct(Set, 1, e)'


def test_destruct_as_argument_is_parenthesised():
    t = app('mem_int', [field('gone', var('s')), destruct('Replace', 0, var('e'))])
    assert pp(t) == 'mem_int s.gone (Destruct(Replace, 0, e))'


def test_field_of_destruct_is_parenthesised():
    t = app(
        'mem_int', [field('gone', var('s')), field('src', destruct('Add', 0, var('e')))]
    )
    assert pp(t) == 'mem_int s.gone (Destruct(Add, 0, e)).src'


def test_destruct_in_comparison():
    t = app('=', [field('v3', field('now', var('s'))), destruct('Set', 1, var('e'))])
    assert pp(t) == 's.now.v3 = Destruct(Set, 1, e)'

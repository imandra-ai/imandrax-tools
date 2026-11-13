from imandrax_api_models.pp.term_builders import (
    app,
    const_int,
    const_q,
    construct,
    destruct,
    field,
    is_a,
    sequence,
    tuple_,
    tuple_field,
    var,
)
from imandrax_api_models.pp.term_formatter import prettify
from imandrax_api_models.pp.xtype import Term


def term_to_string(t: Term) -> str:
    return prettify(t, width=200)


# Constructors
# ============


def test_constructor_no_args():
    assert term_to_string(construct('S1', [])) == 'S1'


def test_constructor_one_arg():
    assert term_to_string(construct('Some', [var('x')])) == 'Some x'


def test_constructor_one_arg_application_is_parenthesised():
    assert term_to_string(construct('Some', [app('f', [var('x')])])) == 'Some (f x)'


def test_constructor_several_args_is_a_tuple():
    t = construct('Set', [construct('S1', []), var('a'), app('f', [var('b')])])
    assert term_to_string(t) == 'Set (S1, a, f b)'


def test_constructor_tuple_arg_stays_one_element():
    t = construct('Pair', [tuple_([const_int(1), const_int(2)]), var('x')])
    assert term_to_string(t) == 'Pair ((1, 2), x)'


def test_constructor_in_cons():
    t = construct('::', [construct('Set', [var('a'), var('b')]), construct('[]', [])])
    assert term_to_string(t) == 'Set (a, b) :: []'


# Is_a and Destruct
# =================
# Printed as ImandraX prints them in region strings.


def test_is_a():
    assert term_to_string(is_a('Set', var('e'))) == 'Is_a(Set, e)'


def test_is_a_under_not_is_parenthesised():
    assert term_to_string(app('not', [is_a('Add', var('e'))])) == 'not (Is_a(Add, e))'


def test_is_a_in_conjunction():
    t = app('&&', [is_a('Set', var('e')), app('not', [var('b')])])
    assert term_to_string(t) == 'Is_a(Set, e) && not b'


def test_destruct():
    assert term_to_string(destruct('Set', 1, var('e'))) == 'Destruct(Set, 1, e)'


def test_destruct_as_argument_is_parenthesised():
    t = app('mem_int', [field('gone', var('s')), destruct('Replace', 0, var('e'))])
    assert term_to_string(t) == 'mem_int s.gone (Destruct(Replace, 0, e))'


def test_field_of_destruct_is_parenthesised():
    t = app(
        'mem_int', [field('gone', var('s')), field('src', destruct('Add', 0, var('e')))]
    )
    assert term_to_string(t) == 'mem_int s.gone (Destruct(Add, 0, e)).src'


def test_destruct_in_comparison():
    t = app('=', [field('v3', field('now', var('s'))), destruct('Set', 1, var('e'))])
    assert term_to_string(t) == 's.now.v3 = Destruct(Set, 1, e)'


def test_constructor_negative_arg_is_parenthesised():
    assert term_to_string(construct('Some', [const_int(-1)])) == 'Some (-1)'
    assert term_to_string(construct('Some', [const_q(-3, 1)])) == 'Some (-3.0)'


def test_application_negative_arg_is_parenthesised():
    assert term_to_string(app('f', [const_int(-1), var('x')])) == 'f (-1) x'


def test_negative_operand_of_infix_is_not_parenthesised():
    assert term_to_string(app('+', [var('x'), const_int(-1)])) == 'x + -1'


# Field and tuple-element access
# ==============================


def test_field_of_application_is_parenthesised():
    t = field('x', app('hd', [var('l')]))
    assert term_to_string(t) == '(hd l).x'


def test_tuple_field_of_application_is_parenthesised():
    t = tuple_field(0, app('f', [var('x')]))
    assert term_to_string(t) == '(f x).0'


def test_nested_field_is_not_parenthesised():
    assert term_to_string(field('v3', field('now', var('s')))) == 's.now.v3'


# Sequences in tuples
# ===================


def test_sequence_in_tuple_element_is_parenthesised():
    seq = sequence([var('a')], var('b'))
    assert term_to_string(construct('Set', [seq, var('c')])) == 'Set ((a; b), c)'
    assert term_to_string(destruct('C', 0, seq)) == 'Destruct(C, 0, (a; b))'
    assert term_to_string(is_a('C', seq)) == 'Is_a(C, (a; b))'
    assert term_to_string(tuple_([seq, var('c')])) == '(a; b), c'


def test_nested_tuple_element_is_parenthesised():
    t = tuple_([tuple_([var('a'), var('b')]), var('c')])
    assert term_to_string(t) == '(a, b), c'

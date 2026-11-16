# ai-disclosure: ai-generated
from imandrax_api_models.pp.term_builders import (
    app,
    apply,
    case,
    const_int,
    const_q,
    construct,
    destruct,
    field,
    if_,
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


def test_field_access_does_not_break_at_the_dot():
    t = app(
        '=',
        [
            field(
                'order_price',
                app('hd', [app('tl', [field('sells', var('order_book_snapshot'))])]),
            ),
            var('ref_price'),
        ],
    )
    assert prettify(t, width=20) == (
        '(hd\n  (tl\n    order_book_snapshot.sells)).order_price\n=\nref_price'
    )


# Operator constructor names
# ==========================


def test_destruct_of_cons_parenthesises_the_constructor():
    assert term_to_string(destruct('::', 0, var('l'))) == 'Destruct((::), 0, l)'


def test_is_a_cons_parenthesises_the_constructor():
    assert term_to_string(is_a('::', var('l'))) == 'Is_a((::), l)'


# Operator symbols
# ================
# The infix layout and the printed text come from the same name.


def test_module_qualified_operator_is_infix():
    assert term_to_string(app('Int.<', [var('x'), var('y')])) == 'x < y'
    assert term_to_string(app('Float.+.', [var('x'), var('y')])) == 'x +. y'


def test_module_qualified_keyword_operator_is_infix():
    assert term_to_string(app('Int.mod', [var('x'), var('y')])) == 'x mod y'


def test_module_qualified_operator_operand_is_parenthesised():
    t = app('=', [var('b'), app('Int.<', [var('x'), var('y')])])
    assert term_to_string(t) == 'b = (x < y)'


def test_operator_alias_prints_the_operator():
    assert term_to_string(app('List.append', [var('x'), var('y')])) == 'x @ y'
    assert term_to_string(app('implies', [var('a'), var('b')])) == 'a ==> b'


def test_module_qualified_function_keeps_short_name():
    assert term_to_string(app('List.length', [var('x')])) == 'length x'


# Unary minus
# ===========


def test_unary_minus_prints_as_minus():
    t = app('~-', [app('f', [var('x')])])
    assert term_to_string(t) == '- f x'
    assert term_to_string(app('~-.', [var('r')])) == '-. r'


# If branches
# ===========


def test_sequence_in_if_branches_is_parenthesised():
    seq = sequence([var('a')], var('b'))
    assert term_to_string(if_(var('c'), seq, var('e'))) == 'if c then (a; b) else e'
    assert term_to_string(if_(var('c'), var('x'), seq)) == 'if c then x else (a; b)'


def test_else_if_chain_is_not_parenthesised():
    t = if_(var('c'), var('x'), if_(var('d'), var('y'), var('z')))
    assert term_to_string(t) == 'if c then x else if d then y else z'


# Case
# ====
# Printed as a pseudo-constructor, like `Destruct`: MIR's `Case` binds nothing
# in its patterns, so it has no `match` form.


def test_case():
    t = case(var('a'), [('None', var('b')), ('Some', destruct('Some', 0, var('a')))])
    assert term_to_string(t) == 'Case(a, [(None, b); (Some, Destruct(Some, 0, a))])'


def test_case_default_is_third_element():
    t = case(var('a'), [('None', var('b'))], var('d'))
    assert term_to_string(t) == 'Case(a, [(None, b)], d)'


def test_case_of_cons_parenthesises_the_constructor():
    t = case(var('l'), [('::', const_int(1)), ('[]', const_int(0))])
    assert term_to_string(t) == 'Case(l, [((::), 1); ([], 0)])'


def test_case_branch_sequence_is_parenthesised():
    t = case(var('a'), [('None', sequence([var('x')], var('y')))])
    assert term_to_string(t) == 'Case(a, [(None, (x; y))])'


def test_case_as_argument_is_parenthesised():
    t = app('f', [case(var('a'), [('None', var('b'))]), var('x')])
    assert term_to_string(t) == 'f (Case(a, [(None, b)])) x'


def test_case_breaks_one_branch_per_line():
    t = case(var('l'), [('::', app('f', [var('l')])), ('[]', const_int(0))])
    assert (
        prettify(t, width=20)
        == 'Case(\n  l,\n  [\n    ((::), f l);\n    ([], 0)\n  ]\n)'
    )


# Applied terms
# =============
# The function of an application, when not a symbol, is parenthesised if
# compound, like the base of a field access.


def test_applied_variable_is_not_parenthesised():
    assert term_to_string(apply(var('f'), [var('x')])) == 'f x'


def test_applied_variable_as_argument_is_parenthesised():
    t = app('g', [apply(var('f'), [var('x')])])
    assert term_to_string(t) == 'g (f x)'


def test_applied_destruct_is_parenthesised():
    t = apply(destruct('::', 0, var('ts')), [destruct('::', 0, var('goals'))])
    assert term_to_string(t) == '(Destruct((::), 0, ts)) (Destruct((::), 0, goals))'


def test_applied_application_is_parenthesised():
    t = apply(app('g', [var('y')]), [var('x')])
    assert term_to_string(t) == '(g y) x'


def test_applied_if_is_parenthesised():
    t = apply(if_(var('c'), var('f'), var('g')), [var('x')])
    assert term_to_string(t) == '(if c then f else g) x'


def test_applied_field_is_not_parenthesised():
    assert term_to_string(apply(field('f', var('r')), [var('x')])) == 'r.f x'

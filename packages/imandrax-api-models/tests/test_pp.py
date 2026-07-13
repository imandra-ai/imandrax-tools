# """Tests for the term-formatter port in `imandrax_api_models.pp`."""

# from __future__ import annotations

# from imandrax_api_models.pp import pretty as P
# from imandrax_api_models.pp.operators import (
#     Associativity,
#     Notation,
#     needs_parentheses,
#     operator_info,
#     operator_info_of_term,
#     short_id,
# )
# from imandrax_api_models.pp.term_builders import (
#     app,
#     apply,
#     case,
#     const_bool,
#     const_int,
#     const_q,
#     const_str,
#     construct,
#     destruct,
#     field,
#     if_,
#     is_a,
#     record,
#     sequence,
#     sym,
#     tuple_,
#     tuple_field,
#     uid,
#     var,
# )
# from imandrax_api_models.pp.term_formatter import prettify
# from inline_snapshot import snapshot

# # Pretty-printer combinators
# # ==========================


# class TestPretty:
#     def test_text_flat_fits(self):
#         d = P.group(P.hcat(P.text('hello'), P.line, P.text('world')))
#         assert P.pretty(80, d) == 'hello world'

#     def test_text_breaks_when_too_narrow(self):
#         d = P.group(P.hcat(P.text('hello'), P.line, P.text('world')))
#         assert P.pretty(5, d) == 'hello\nworld'

#     def test_nest_indents_after_break(self):
#         d = P.group(
#             P.hcat(
#                 P.text('a'),
#                 P.nest(2, P.hcat(P.line, P.text('b'))),
#             )
#         )
#         assert P.pretty(80, d) == 'a b'
#         assert P.pretty(1, d) == 'a\n  b'

#     def test_punctuate_empty(self):
#         assert P.pretty(80, P.punctuate(P.text(', '), [])) == ''

#     def test_tupled(self):
#         docs = [P.text('a'), P.text('b'), P.text('c')]
#         assert P.pretty(80, P.tupled(docs)) == '(a, b, c)'

#     def test_list_doc(self):
#         docs = [P.text('a'), P.text('b')]
#         assert P.pretty(80, P.list_doc(docs)) == '[a, b]'


# # Operator info
# # =============


# class TestOperatorInfo:
#     def test_plus_is_infix_left_13(self):
#         oi = operator_info('+')
#         assert oi.notation == Notation.INFIX
#         assert oi.associativity == Associativity.LEFT
#         assert oi.precedence == 13

#     def test_iff_special(self):
#         oi = operator_info('iff')
#         assert oi.name == '<==>'
#         assert oi.precedence == 8.1

#     def test_unary_minus_prefix_when_single_arg(self):
#         assert operator_info('-').notation == Notation.PREFIX
#         # binary minus when more than one arg present
#         assert operator_info('-', more_than_one_arg=True).notation == Notation.INFIX

#     def test_default_is_app(self):
#         oi = operator_info('foo')
#         assert oi.precedence == 17
#         assert oi.notation == Notation.PREFIX

#     def test_needs_parens_basic(self):
#         plus = operator_info('+')
#         times = operator_info('*')
#         # `a + b` inside `* `: parent (times, p=14) > child (plus, p=13) → parens
#         assert needs_parentheses(times, plus, is_left=True) is True
#         # `a * b` inside `+`: parent (plus, p=13) < child (times, p=14) → no parens
#         assert needs_parentheses(plus, times, is_left=True) is False


# # operator_info_of_term
# # =====================


# class TestOpInfoOfTerm:
#     def test_apply_infix(self):
#         t = app('+', [var('x'), var('y')])
#         oi = operator_info_of_term(t)
#         assert oi.name == '+'
#         assert oi.notation == Notation.INFIX

#     def test_const_q_proper_is_div(self):
#         oi = operator_info_of_term(const_q(3, 2))
#         assert oi.name == '/.'

#     def test_const_q_int_is_default(self):
#         oi = operator_info_of_term(const_q(3, 1))
#         assert oi.precedence == 17

#     def test_short_id_passthrough(self):
#         assert short_id(uid('hello')) == 'hello'


# # Term formatter — simple snapshots
# # =================================


# class TestTermFormatterSimple:
#     def test_const_int(self):
#         assert prettify(const_int(42), 80) == snapshot('42')

#     def test_const_bool(self):
#         assert prettify(const_bool(True), 80) == snapshot('true')
#         assert prettify(const_bool(False), 80) == snapshot('false')

#     def test_const_string(self):
#         assert prettify(const_str('hi'), 80) == snapshot('"hi"')

#     def test_const_q_proper(self):
#         assert prettify(const_q(3, 2), 80) == snapshot('3.0 /. 2.0')

#     def test_const_q_integer(self):
#         assert prettify(const_q(3, 1), 80) == snapshot('3.0')

#     def test_var(self):
#         assert prettify(var('x'), 80) == snapshot('x')

#     def test_sym_alone(self):
#         assert prettify(sym('foo'), 80) == snapshot('foo')

#     def test_apply_prefix(self):
#         assert prettify(app('foo', [var('x'), var('y')]), 80) == snapshot('foo x y')

#     def test_with_turnstile(self):
#         assert prettify(const_int(7), 80, with_turnstile=True) == snapshot('⊢ 7')


# # Term formatter — complex cases
# # ==============================


# class TestTermFormatterComplex:
#     """Larger, more representative terms exercised via inline snapshots."""

#     def test_arith_precedence(self):
#         # x + y * z  → no parens
#         e = app('+', [var('x'), app('*', [var('y'), var('z')])])
#         assert prettify(e, 80) == snapshot('x + y * z')

#         # (x + y) * z  → parens needed
#         e2 = app('*', [app('+', [var('x'), var('y')]), var('z')])
#         assert prettify(e2, 80) == snapshot('(x + y) * z')

#     def test_left_assoc_minus(self):
#         # `-` is left-assoc: `(a - b) - c` rendered without parens,
#         #                    `a - (b - c)` keeps parens.
#         e = app('-', [app('-', [var('a'), var('b')]), var('c')])
#         assert prettify(e, 80) == snapshot('a - b - c')

#         e2 = app('-', [var('a'), app('-', [var('b'), var('c')])])
#         assert prettify(e2, 80) == snapshot('a - (b - c)')

#     def test_right_assoc_implies(self):
#         # `==>` is right-assoc (precedence 8.3): `a ==> (b ==> c)` flattens,
#         # `(a ==> b) ==> c` keeps parens.
#         e = app('==>', [var('a'), app('==>', [var('b'), var('c')])])
#         assert prettify(e, 80) == snapshot('a ==> b ==> c')

#         e2 = app('==>', [app('==>', [var('a'), var('b')]), var('c')])
#         assert prettify(e2, 80) == snapshot('(a ==> b) ==> c')

#     def test_logical_mix(self):
#         # (a && b) || (c && d) — `&&` (9, R) binds tighter than `||` (8, R)
#         e = app(
#             '||',
#             [
#                 app('&&', [var('a'), var('b')]),
#                 app('&&', [var('c'), var('d')]),
#             ],
#         )
#         assert prettify(e, 80) == snapshot('a && b || c && d')

#     def test_q_inside_times(self):
#         # 3/2 * x — the rational is internally `/.` and must be parenthesised.
#         e = app('*', [const_q(3, 2), var('x')])
#         assert prettify(e, 80) == snapshot('3.0 /. 2.0 * x')

#     def test_if_fits_flat(self):
#         e = if_(var('c'), const_int(1), const_int(2))
#         assert prettify(e, 80) == snapshot('if c then 1 else 2')

#     def test_if_breaks(self):
#         e = if_(
#             app('=', [var('x'), const_int(0)]),
#             app('foo', [var('a'), var('b'), var('c')]),
#             app('bar', [var('p'), var('q'), var('r')]),
#         )
#         assert prettify(e, 20) == snapshot("""\
# if x = 0 then
#   foo a b c
# else
#   bar p q r\
# """)

#     def test_nested_if(self):
#         # if a then (if b then 1 else 2) else 3
#         inner = if_(var('b'), const_int(1), const_int(2))
#         e = if_(var('a'), inner, const_int(3))
#         assert prettify(e, 80) == snapshot('if a then if b then 1 else 2 else 3')

#     def test_tuple_flat(self):
#         e = tuple_([var('x'), var('y'), var('z')])
#         assert prettify(e, 80) == snapshot('x, y, z')

#     def test_tuple_inside_app(self):
#         # `f (x, y)` — `,` (p=7) is lower precedence than app (p=17), so parens.
#         e = app('f', [tuple_([var('x'), var('y')])])
#         assert prettify(e, 80) == snapshot('f (x, y)')

#     def test_construct_nullary(self):
#         assert prettify(construct('Nil', []), 80) == snapshot('Nil')

#     def test_construct_with_args(self):
#         e = construct('Cons', [var('x'), construct('Nil', [])])
#         assert prettify(e, 80) == snapshot('Cons x Nil')

#     def test_is_a(self):
#         e = is_a('Some', var('x'))
#         assert prettify(e, 80) == snapshot('x is-a Some')

#     def test_destruct(self):
#         e = destruct('Cons', 0, var('xs'))
#         assert prettify(e, 80) == snapshot('destruct[Cons|0] xs')

#     def test_field(self):
#         e = field('name', var('p'))
#         assert prettify(e, 80) == snapshot('p.name')

#     def test_tuple_field(self):
#         e = tuple_field(1, var('t'))
#         assert prettify(e, 80) == snapshot('t.1')

#     def test_record_flat(self):
#         e = record([('x', const_int(1)), ('y', const_int(2))])
#         assert prettify(e, 80) == snapshot('{ x: 1; y: 2 }')

#     def test_record_with_rest(self):
#         e = record([('x', const_int(1))], rest=var('base'))
#         assert prettify(e, 80) == snapshot('{ base with x: 1 }')

#     def test_record_broken_leading_separators(self):
#         e = record([('first_key', var('item')), ('second_key', var('snd_item'))])
#         assert prettify(e, 30) == snapshot("""\
# { first_key: item
# ; second_key: snd_item
# }\
# """)

#     def test_case(self):
#         e = case(
#             var('opt'),
#             [
#                 ('None', const_int(0)),
#                 ('Some', var('v')),
#             ],
#         )
#         assert prettify(e, 80) == snapshot('case opt of | None -> 0 | Some -> v')

#     def test_case_with_default(self):
#         e = case(
#             var('x'),
#             [('A', const_int(1))],
#             default=const_int(99),
#         )
#         assert prettify(e, 80) == snapshot('case x of | A -> 1 | _ -> 99')

#     def test_sequence(self):
#         e = sequence(
#             [app(':=', [var('x'), const_int(1)]), app(':=', [var('y'), const_int(2)])],
#             var('x'),
#         )
#         assert prettify(e, 80) == snapshot('x := 1; y := 2; x')

#     def test_higher_order_apply(self):
#         # `(f x) y` — function position is a non-symbol Apply.
#         inner = app('f', [var('x')])
#         e = apply(inner, [var('y')])
#         assert prettify(e, 80) == snapshot("""\
# (f x
#   y)\
# """)

#     def test_negation_unary(self):
#         # `-` as prefix with a single arg.
#         e = app('-', [var('x')])
#         assert prettify(e, 80) == snapshot('- x')

#     def test_deeply_nested_breaks(self):
#         # A long chain of `&&` should break onto its own lines at narrow width.
#         chain = var('a0')
#         for i in range(1, 6):
#             chain = app('&&', [chain, var(f'a{i}')])
#         assert prettify(chain, 80) == snapshot(
#             '((((a0 && a1) && a2) && a3) && a4) && a5'
#         )
#         assert prettify(chain, 15) == snapshot("""\
# ((((a0 && a1)
# &&
# a2)
# &&
# a3)
# &&
# a4)
# &&
# a5\
# """)

#     def test_iff_associativity(self):
#         # `<==>` is None-associative, so both sides need parens when nested.
#         e = app('<==>', [app('<==>', [var('a'), var('b')]), var('c')])
#         assert prettify(e, 80) == snapshot('a <==> b <==> c')

#     def test_mixed_realistic(self):
#         # `if x > 0 then f (x, x + 1) else g {n: 0}`
#         cond = app('>', [var('x'), const_int(0)])
#         then_b = app('f', [tuple_([var('x'), app('+', [var('x'), const_int(1)])])])
#         else_b = app('g', [record([('n', const_int(0))])])
#         e = if_(cond, then_b, else_b)
#         assert prettify(e, 80) == snapshot('if x > 0 then f (x, x + 1) else g { n: 0 }')

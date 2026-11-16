# pyright: basic
"""Pretty-printer for ImandraX MIR `Term`s."""

from __future__ import annotations

import imandrax_api.lib as xtype

from . import operators as IXO
from .operators import (
    Notation,
    OperatorInfo,
    const2str,
    has_multiple_children,
    needs_parentheses,
    operator_info,
    operator_info_of_term,
    short_id,
    sym_operator_info,
)
from .pretty import (
    Doc,
    flatten,
    group,
    hcat,
    join,
    line,
    linebreak,
    nest,
    nil,
    parens,
    pretty,
    python_obj,
    text,
)

# Note [MIR Term Formatter]
# ~~~~~~~~~~~~~~~~~~~~~~~~~
#
# (A) Goal
#
# The output should read as IML: it should parse, with a parse tree matching
# the MIR term.
# Type-checking is not a goal. Some MIR nodes have no IML source form and are printed
# in a form that parses but is not defined in IML.
# Names are printed without their module path (`List.length` as `length`), so they may
# not resolve.
#
# (B) How it works
#
# `term2doc` walks the term's view and builds a `Doc`. `group`
# lays a part out flat if it fits the width and breaks its `line`s otherwise.
#
# Parentheses come from precedence. `operators.operator_info_of_term` gives the
# precedence, associativity and notation of a term as printed, following
# OCaml's table. `recwp` parenthesises a child when `needs_parentheses` says
# the parent binds tighter. Some positions have their own rule:
#
#   - `_tuple_elem2doc`, a tuple element or constructor argument: parenthesise
#     anything binding no tighter than `,`, as in `Set ((a; b), c)`.
#   - `_atom2doc`, the base of `.f`, or an applied function that is not a
#     symbol: parenthesise anything compound, as in `(hd l).x` and
#     `(Destruct((::), 0, ts)) x`.
#   - `if` branches: parenthesise anything binding looser than `if`.
#     `if c then a; b else e` does not parse, and `if c then x else a; b`
#     parses as `(if c then x else a); b`.
#
# `operators.sym_operator_info` gives both the layout (infix or prefix) and the
# printed text of a symbol, from the same name: `Int.<` prints as `x < y`,
# `List.append` as `x @ y`, `~-` as `- x`.
#
# (W1) Cases that parses but does not type-check
#
# MIR reads and tests constructors directly, where IML binds fields in a
# `match` pattern. These nodes are printed as a capitalised name applied to a
# tuple, which parses as a constructor application:
#
#   - `Destruct(C, i, t)`: field `i` of `t`, built with constructor `C`.
#   - `Is_a(C, t)`: `t` is built with `C`.
#   - `Case(u, [(C1, t1); (C2, t2)], d)`: branch on the constructor of `u`; `d`
#     is the default branch, printed only when present. MIR's `Case` binds no
#     fields; branches read them with `Destruct`. A `match` would need each
#     constructor's arity for its pattern.
#
# `Destruct` and `Is_a` are spelled as in ImandraX's region strings; `Case`
# follows them. None of the three is defined in IML. Operator constructors are
# parenthesised in them: `Destruct((::), 0, l)`, `Case(l, [((::), 1); ([], 0)])`.
#
# (W2) Does not parse
#
#   - Tuple projections print as `t.0`, which is a syntax error. IML has no
#     projection syntax, and a `match` would need the tuple's size.
#   - Generated names, such as a lifted lambda `$lambda...` or `$opt_depth` in
#     `Tactic.repeat`, print as is. `$` cannot start an identifier. Printing
#     the lambda as `fun ...` would need the MIR DB.
#
# Both occur mostly in prelude internals (`fst`, `snd`, `Tactic`, `Decomp`).
#

# Small helpers
# =============


def _kw(w: str) -> Doc:
    return text(w)


def _indent(ds: list[Doc]) -> Doc:
    return nest(2, hcat(*ds))


def _g(ds: list[Doc]) -> Doc:
    return group(hcat(*ds))


def _par_if(cond: bool, ds: list[Doc]) -> Doc:
    return parens(hcat(*ds)) if cond else hcat(*ds)


def _applied_sym_id(s: xtype.Common_Applied_symbol_t_poly) -> xtype.Uid:
    return s.sym.id


def _applied_sym_short(s: xtype.Common_Applied_symbol_t_poly) -> str:
    """
    Short, display-friendly name for an applied symbol, with module path stripped (the dotted prefix, if any).
    """
    return sym_operator_info(short_id(_applied_sym_id(s)))[1]


# Constants
# ==========


def _const2doc(c: xtype.Const) -> Doc:
    return text(const2str(c))


# Subterm-selection (used inside `subterm_selection_select`)
# ==========================================================


def _get_string_const(t: xtype.Mir_Term) -> str | None:
    v = t.view
    if isinstance(v, xtype.Mir_Term_view_Const) and isinstance(
        v.arg, xtype.Const_Const_string
    ):
        return v.arg.arg
    return None


# Term
# ==========


# Symbols
# -------


def sym2doc(s: xtype.Common_Applied_symbol_t_poly) -> Doc:
    return text(_applied_sym_short(s))


def _ctor_name2doc(c: xtype.Common_Applied_symbol_t_poly) -> Doc:
    """
    A constructor's name where it stands alone, as in `Destruct(C, i, t)`.

    An operator constructor is parenthesised: `Destruct((::), 0, l)`.
    """
    name = _applied_sym_short(c)
    is_operator = not (name[:1].isalnum() or name[:1] in ('_', '[', '(', '`'))
    return text(f'({name})' if is_operator else name)


# subterm_selection_select(...) special-casing
# --------------------------------------------


def _subterm_selection2doc(ts: list[xtype.Mir_Term]) -> Doc:
    def t2d(x: xtype.Mir_Term) -> Doc:
        return term2doc(x)

    rs: list[Doc] = []
    for t in ts:
        v = t.view
        rendered: Doc | None = None
        if isinstance(v, xtype.Mir_Term_view_Apply) and isinstance(
            v.f.view, xtype.Mir_Term_view_Sym
        ):
            sid = short_id(v.f.view.arg.sym.id)
            if sid == 'subterm_selection_index':
                rendered = _g([_kw('index'), parens(_indent([linebreak, t2d(v.l[0])]))])
            elif sid == 'subterm_selection_nth':
                rendered = _g([_kw('nth'), parens(_indent([linebreak, t2d(v.l[0])]))])
            elif sid == 'subterm_selection_lhs':
                rendered = _kw('lhs')
            elif sid == 'subterm_selection_rhs':
                rendered = _kw('rhs')
        elif isinstance(v, xtype.Mir_Term_view_Tuple) and len(v.l) == 2:
            tag = _get_string_const(v.l[0])
            if tag == 'at':
                rendered = _g([_kw('at'), _indent([line, parens(t2d(v.l[1]))])])
            elif tag == 'in':
                rendered = _g([_kw('in'), _indent([line, parens(t2d(v.l[1]))])])

        rs.append(rendered if rendered is not None else t2d(t))

    return join(line, rs)


# Main recursion
# --------------


def _tuple_elem2doc(x: xtype.Mir_Term) -> Doc:
    """
    Print a term as one element of a tuple.

    A term binding no tighter than `,` (a tuple, `if`, `match`, ...) is
    parenthesised so it stays one element.
    """
    return _par_if(operator_info_of_term(x).precedence <= _COMMA_PREC, [term2doc(x)])


_COMMA_PREC = operator_info(',').precedence
_IF_PREC = operator_info('if').precedence


def _atom2doc(x: xtype.Mir_Term) -> Doc:
    """
    Print a term where only an atom fits.

    These are the base of a field or tuple-element access, and the function of
    an application. Both bind at least as tight as application, so any compound term is
    parenthesised: `(f x).g`, not `f x.g`; `(Destruct(C, i, t)) x`, not
    `Destruct(C, i, t) x`, which applies a constructor to two arguments.
    """
    return _par_if(has_multiple_children(x), [term2doc(x)])


def _tuple2doc(docs: list[Doc]) -> Doc:
    """Print already-printed elements in IML tuple syntax, `(a, b, c)`."""
    return _g(
        [
            text('('),
            _indent([linebreak, join(hcat(text(','), line), docs)]),
            linebreak,
            text(')'),
        ]
    )


def terms2doc(ts: list[xtype.Mir_Term]) -> Doc:
    """Show terms in one `Terms(t1, t2, ...)` block."""
    term_f = lambda t: flatten(term2doc(t))
    term_docs = [hcat(text("'"), term_f(t), text("'")) for t in ts]
    return python_obj('Terms', [(None, d) for d in term_docs])


def term2doc(t: xtype.Mir_Term) -> Doc:
    rec = term2doc

    def recwp(
        parent_oi: OperatorInfo,
        x: xtype.Mir_Term,
        is_left: bool | None = None,
    ) -> Doc:
        child_oi = operator_info_of_term(x)
        base_needs = needs_parentheses(
            parent_oi, child_oi, is_left, has_multiple_children(x)
        )
        # Special case for rationals: they are constants here, but really an
        # application of `/.`, so parenthesise when the parent binds tighter.
        rational_special = (
            isinstance(x.view, xtype.Mir_Term_view_Const)
            and isinstance(x.view.arg, xtype.Const_Const_q)
            and x.view.arg.arg[1] != 1
            and parent_oi.precedence > operator_info('/.').precedence
        )
        return _par_if(base_needs or rational_special, [rec(x)])

    v = t.view

    match v:
        case xtype.Mir_Term_view_Const(arg=c):
            return _const2doc(c)

        case xtype.Mir_Term_view_If(args=(c, th, el)):
            # A branch binding looser than `if` is parenthesised:
            # `if c then a; b else e` does not parse, and `if c then x else a; b`
            # parses as `(if c then x else a); b`.
            def branch(x: xtype.Mir_Term) -> Doc:
                looser = operator_info_of_term(x).precedence < _IF_PREC
                return _par_if(looser, [rec(x)])

            return _g(
                [
                    _g([_kw('if'), _indent([line, branch(c)]), line, _kw('then')]),
                    _indent([line, branch(th)]),
                    line,
                    _kw('else'),
                    _indent([line, branch(el)]),
                ]
            )

        case xtype.Mir_Term_view_Apply(f=f, l=args):
            if isinstance(f.view, xtype.Mir_Term_view_Sym):
                s = f.view.arg
                sid = short_id(s.sym.id)
                pi, name = sym_operator_info(sid, len(args) > 1)
                fn = text(name)

                if sid == 'subterm_selection_select':
                    return _subterm_selection2doc(args)
                if len(args) == 0:
                    return fn
                if pi.notation == Notation.INFIX:
                    if len(args) == 2:
                        lhs = recwp(pi, args[0], True)
                        rhs = recwp(pi, args[1], False)
                        return _g([lhs, line, fn, line, rhs])
                    hargs = _indent(
                        [
                            line,
                            join(line, [recwp(pi, x) for x in args]),
                        ]
                    )
                    return _g([text('( '), fn, text(' )'), hargs])
                # Prefix / default
                hargs = _indent(
                    [
                        line,
                        join(line, [recwp(pi, x) for x in args]),
                    ]
                )
                return _g([fn, hargs])
            # The function is a term, not a symbol.
            fn = _atom2doc(f)
            if len(args) == 0:
                return fn
            hargs = _indent(
                [
                    line,
                    join(line, [recwp(IXO.default_(), x) for x in args]),
                ]
            )
            return _g([fn, hargs])

        case xtype.Mir_Term_view_Var(arg=var):
            return text(var.id.name)

        case xtype.Mir_Term_view_Sym(arg=s):
            return sym2doc(s)

        case xtype.Mir_Term_view_Construct(c=c, args=args):
            if len(args) == 0:
                return sym2doc(c)
            sid = short_id(c.sym.id)
            pi, name = sym_operator_info(sid, len(args) > 1)
            op_doc = text(name)
            if pi.notation == Notation.INFIX:
                if len(args) == 2:
                    lhs = recwp(pi, args[0], True)
                    rhs = recwp(pi, args[1], False)
                    return _g([lhs, line, op_doc, line, rhs])
                hargs = _indent(
                    [
                        line,
                        join(line, [recwp(pi, x) for x in args]),
                    ]
                )
                return _g([text('( '), op_doc, text(' )'), hargs])
            # NOTE: this deviates from `term-formatter.ts`, which joins the
            # arguments with spaces and no parentheses, producing `Set S1 a b`
            # for `Set (S1, a, b)` and `Some f x` for `Some (f x)`, neither of
            # which is valid IML. We print one argument as an application argument
            # (parenthesised when needed), several as parenthesised.
            if len(args) == 1:
                return _g([op_doc, _indent([line, recwp(IXO.default_(), args[0])])])
            return _g(
                [op_doc, text(' '), _tuple2doc([_tuple_elem2doc(x) for x in args])]
            )

        # NOTE: `Destruct` and `Is_a` deviate from `term-formatter.ts`, which
        # prints them as `destruct[C|i] t` and `t is-a C`, display notation
        # that does not parse as IML.
        # We print the form ImandraX itself uses in its region strings,
        # `Destruct(C, i, t)` and `Is_a(C, t)`.
        # These parse, though neither is defined in IML, so they do not type-check
        # as source.
        case xtype.Mir_Term_view_Destruct(c=c, i=i, t=inner):
            return _g(
                [
                    _kw('Destruct'),
                    _tuple2doc(
                        [_ctor_name2doc(c), text(str(i)), _tuple_elem2doc(inner)]
                    ),
                ]
            )

        case xtype.Mir_Term_view_Is_a(c=c, t=inner):
            return _g(
                [_kw('Is_a'), _tuple2doc([_ctor_name2doc(c), _tuple_elem2doc(inner)])]
            )

        case xtype.Mir_Term_view_Tuple(l=ls):
            return _g(
                [
                    _indent(
                        [join(hcat(text(','), line), [_tuple_elem2doc(x) for x in ls])]
                    )
                ]
            )

        case xtype.Mir_Term_view_Field(f=f, t=inner):
            inner_doc = _atom2doc(inner)
            return hcat(inner_doc, text('.'), sym2doc(f))

        case xtype.Mir_Term_view_Tuple_field(i=i, t=inner):
            inner_doc = _atom2doc(inner)
            return hcat(inner_doc, text('.'), text(str(i)))

        case xtype.Mir_Term_view_Record(rows=rows, rest=rest):
            if False:
                # NOTE: this deviates from `term-formatter.ts`, which emits
                # `text(":")` with no following separator — producing `{x:1}`.
                # We add a `line` so the field/value pair is space-separated
                # (and breakable): `{x: 1}`.
                hrows = join(
                    hcat(text(';'), line),
                    [
                        _g([sym2doc(sym), text(':'), line, rec(term)])
                        for sym, term in rows
                    ],
                )
                hrest: Doc = (
                    _g([rec(rest), line, _kw('with'), line])
                    if rest is not None
                    else nil
                )
                return hcat(text('{'), hrest, hrows, text('}'))
            else:
                # leading separators style, with
                # `{`, `;` and `}` aligned when broken. Flat: `{ a: 1; b: 2 }`.
                #     { fst_key: fst_item
                #     ; snd_key: snd_item
                #     }
                if not rows:
                    return (
                        text('{}')
                        if rest is None
                        else _g([text('{ '), rec(rest), text(' }')])
                    )
                fields = [
                    group(_indent([sym2doc(sym), text(':'), line, rec(term)]))
                    for sym, term in rows
                ]
                head: list[Doc] = [text('{ ')]
                if rest is not None:
                    head += [rec(rest), text(' with ')]
                body: list[Doc] = [fields[0]]
                for fd in fields[1:]:
                    # `linebreak` is empty when flat, so the separator is `; ` flat
                    # and `\n; ` broken — putting the `;` at the record's base column.
                    body += [linebreak, text('; '), fd]
                return group(hcat(*head, *body, line, text('}')))

        # NOTE: this deviates from `term-formatter.ts`, which prints
        # `case u of | C -> t | ...`. That does not parse as IML, and a real
        # `match` would need a pattern for each constructor's fields, which MIR's
        # `Case` does not bind: branches read them with `Destruct`.
        # We print the MIR node as a pseudo-constructor, like `Destruct`:
        # `Case(u, [(C1, t1); (C2, t2)])`, with the default branch as a third
        # element when present, `Case(u, [(C1, t1)], d)`.
        case xtype.Mir_Term_view_Case(u=u, cases=cases, default=default):
            branches = [
                _tuple2doc([_ctor_name2doc(sym), _tuple_elem2doc(term)])
                for sym, term in cases
            ]
            hcases = _g(
                [
                    text('['),
                    _indent([linebreak, join(hcat(text(';'), line), branches)]),
                    linebreak,
                    text(']'),
                ]
            )
            elems = [_tuple_elem2doc(u), hcases]
            if default is not None:
                elems.append(_tuple_elem2doc(default))
            return _g([_kw('Case'), _tuple2doc(elems)])

        case xtype.Mir_Term_view_Sequence(args=(items, last)):
            # NOTE: this deviates from `term-formatter.ts`, which joins
            # `items` with `; ` and then concatenates the result term with
            # no separator — producing `…; 2x` for `x := 1; y := 2; x`.
            # We include `last` in the same `;`-separated layout.
            hseq = join(hcat(text(';'), line), [rec(x) for x in [*items, last]])
            return _g([hseq])

    raise ValueError(f'Unhandled term view: {type(v).__name__}')


def prettify(t: xtype.Mir_Term, width: int = 88, with_turnstile: bool = False) -> str:
    """Pretty-print term `t` with the given line width."""
    doc = term2doc(t)
    if with_turnstile:
        doc = _g([_kw('⊢'), _indent([line, doc])])
    return pretty(width, doc)

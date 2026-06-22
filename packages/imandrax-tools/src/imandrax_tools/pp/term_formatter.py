"""Pretty-printer for ImandraX MIR `Term`s."""

from __future__ import annotations

import imandrax_api.lib as xtype

from . import operators as IXO
from .operators import (
    Notation,
    OperatorInfo,
    has_multiple_children,
    needs_parentheses,
    operator_info,
    operator_info_of_term,
    short_id,
)
from .pretty import (
    Doc,
    brackets,
    group,
    hcat,
    join,
    line,
    linebreak,
    nest,
    nil,
    parens,
    pretty,
    text,
)

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
    sid = short_id(_applied_sym_id(s))
    op_info = operator_info(sid)
    op_name = sid if op_info.name == '' else op_info.name
    dot_inx = sid.rfind('.')
    if 0 < dot_inx < len(sid) - 1:
        op_name = sid[dot_inx + 1 :]
    return op_name


# Constants
# ==========


def _const2doc(c: xtype.Const) -> Doc:
    match c:
        case xtype.Const_Const_float(arg=v):
            return text(repr(v))
        case xtype.Const_Const_string(arg=v):
            return text(f'"{v}"')
        case xtype.Const_Const_z(arg=v):
            return text(str(v))
        case xtype.Const_Const_q(arg=(num, den)):
            if den != 1:
                return text(f'{num}.0 /. {den}.0')
            return text(f'{num}.0')
        case xtype.Const_Const_real_approx(arg=v):
            return text(v)
        case xtype.Const_Const_uid(arg=u):
            return text(u.name)
        case xtype.Const_Const_bool(arg=v):
            return text('true' if v else 'false')
    return text(repr(c))


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
            return _g(
                [
                    _g([_kw('if'), _indent([line, rec(c)]), line, _kw('then')]),
                    _indent([line, rec(th)]),
                    line,
                    _kw('else'),
                    _indent([line, rec(el)]),
                ]
            )

        case xtype.Mir_Term_view_Apply(f=f, l=args):
            fn = rec(f)
            if isinstance(f.view, xtype.Mir_Term_view_Sym):
                s = f.view.arg
                sid = short_id(s.sym.id)
                pi = operator_info(sid, len(args) > 1)

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
            # fn is a function term, not a symbol
            hargs = _indent(
                [
                    line,
                    join(line, [recwp(IXO.default_(), x) for x in args]),
                ]
            )
            return _par_if(len(args) > 0, [fn, hargs])

        case xtype.Mir_Term_view_Var(arg=var):
            return text(var.id.name)

        case xtype.Mir_Term_view_Sym(arg=s):
            return sym2doc(s)

        case xtype.Mir_Term_view_Construct(c=c, args=args):
            if len(args) == 0:
                return sym2doc(c)
            op_doc = sym2doc(c)
            sid = short_id(c.sym.id)
            pi = operator_info(sid, len(args) > 1)
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
            rargs = [rec(x) for x in args]
            return _g([op_doc, _indent([line, join(line, rargs)])])

        case xtype.Mir_Term_view_Destruct(c=c, i=i, t=inner):
            return _g(
                [
                    _kw('destruct'),
                    brackets(hcat(sym2doc(c), text('|'), text(str(i)))),
                    _indent([line, recwp(IXO.default_(), inner)]),
                ]
            )

        case xtype.Mir_Term_view_Is_a(c=c, t=inner):
            pi = operator_info_of_term(t)
            lhs = recwp(pi, inner, True)
            rhs = sym2doc(c)
            return _g([lhs, line, _kw(pi.name), line, rhs])

        case xtype.Mir_Term_view_Tuple(l=ls):
            return _g([_indent([join(hcat(text(','), line), [rec(x) for x in ls])])])

        case xtype.Mir_Term_view_Field(f=f, t=inner):
            return _g([rec(inner), text('.'), linebreak, sym2doc(f)])

        case xtype.Mir_Term_view_Tuple_field(i=i, t=inner):
            return _g([rec(inner), text('.'), linebreak, text(str(i))])

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

        case xtype.Mir_Term_view_Case(u=u, cases=cases, default=default):
            hcases = join(
                line,
                [
                    _g(
                        [
                            text('|'),
                            line,
                            sym2doc(sym),
                            line,
                            text('->'),
                            line,
                            rec(term),
                        ]
                    )
                    for sym, term in cases
                ],
            )
            hdefault: Doc = (
                _g(
                    [
                        line,
                        text('|'),
                        line,
                        text('_'),
                        line,
                        text('->'),
                        line,
                        rec(default),
                    ]
                )
                if default is not None
                else nil
            )
            # NOTE: this deviates from `term-formatter.ts`, which writes
            # `rec(v.u), kw("of")` with no separator — producing `optof`.
            # We add a `line` so the scrutinee and `of` are space-separated.
            return _g(
                [
                    _kw('case'),
                    line,
                    rec(u),
                    line,
                    _kw('of'),
                    line,
                    hcases,
                    hdefault,
                ]
            )

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

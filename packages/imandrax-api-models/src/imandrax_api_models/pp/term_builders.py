"""
Tiny constructors for synthesizing `Mir_Term` values.

The generated `Uid`s and `Mir_Type`s are intentionally placeholders -- the
pretty-printer only inspects `Uid.name` and term shape, never type structure.
"""

from __future__ import annotations

import imandrax_api.lib as xtype

# Identifiers and placeholder types
# =================================


def uid(name: str) -> xtype.Uid:
    return xtype.Uid(name=name, view=xtype.Uid_view_Persistent())  # type: ignore[arg-type]


def ty() -> xtype.Mir_Type:
    """Placeholder `Mir_Type`. The formatter doesn't read type internals."""
    return xtype.Mir_Type(view=xtype.Ty_view_view_Var(arg=uid('a')))  # type: ignore[arg-type]


# Term wrapping
# =============


def term(view) -> xtype.Mir_Term:
    return xtype.Mir_Term(view=view, ty=ty(), sub_anchor=None)


# Constants
# =========


def const_int(n: int) -> xtype.Mir_Term:
    return term(xtype.Mir_Term_view_Const(arg=xtype.Const_Const_z(arg=n)))


def const_bool(b: bool) -> xtype.Mir_Term:
    return term(xtype.Mir_Term_view_Const(arg=xtype.Const_Const_bool(arg=b)))


def const_str(s: str) -> xtype.Mir_Term:
    return term(xtype.Mir_Term_view_Const(arg=xtype.Const_Const_string(arg=s)))


def const_q(num: int, den: int) -> xtype.Mir_Term:
    return term(xtype.Mir_Term_view_Const(arg=xtype.Const_Const_q(arg=(num, den))))


def const_float(v: float) -> xtype.Mir_Term:
    return term(xtype.Mir_Term_view_Const(arg=xtype.Const_Const_float(arg=v)))


# Symbols, variables, application
# ===============================


def applied_sym(name: str) -> xtype.Common_Applied_symbol_t_poly:
    return xtype.Common_Applied_symbol_t_poly(
        sym=xtype.Common_Typed_symbol_t_poly(
            id=uid(name),
            ty=xtype.Common_Type_schema_t_poly(params=[], ty=ty()),
        ),
        args=[],
        ty=ty(),
    )


def sym(name: str) -> xtype.Mir_Term:
    return term(xtype.Mir_Term_view_Sym(arg=applied_sym(name)))


def var(name: str) -> xtype.Mir_Term:
    return term(
        xtype.Mir_Term_view_Var(arg=xtype.Common_Var_t_poly(id=uid(name), ty=ty()))
    )


def apply(f: xtype.Mir_Term, args: list[xtype.Mir_Term]) -> xtype.Mir_Term:
    return term(xtype.Mir_Term_view_Apply(f=f, l=args))


def app(name: str, args: list[xtype.Mir_Term]) -> xtype.Mir_Term:
    """Shorthand: apply symbol `name` to `args`."""
    return apply(sym(name), args)


# Control / data shapes
# =====================


def if_(c: xtype.Mir_Term, t: xtype.Mir_Term, e: xtype.Mir_Term) -> xtype.Mir_Term:
    return term(xtype.Mir_Term_view_If(args=(c, t, e)))


def tuple_(elems: list[xtype.Mir_Term]) -> xtype.Mir_Term:
    return term(xtype.Mir_Term_view_Tuple(l=elems))


def construct(name: str, args: list[xtype.Mir_Term]) -> xtype.Mir_Term:
    return term(
        xtype.Mir_Term_view_Construct(c=applied_sym(name), args=args, labels=None)
    )


def destruct(name: str, i: int, t: xtype.Mir_Term) -> xtype.Mir_Term:
    return term(xtype.Mir_Term_view_Destruct(c=applied_sym(name), i=i, t=t))


def is_a(name: str, t: xtype.Mir_Term) -> xtype.Mir_Term:
    return term(xtype.Mir_Term_view_Is_a(c=applied_sym(name), t=t))


def field(name: str, t: xtype.Mir_Term) -> xtype.Mir_Term:
    return term(xtype.Mir_Term_view_Field(f=applied_sym(name), t=t))


def tuple_field(i: int, t: xtype.Mir_Term) -> xtype.Mir_Term:
    return term(xtype.Mir_Term_view_Tuple_field(i=i, t=t))


def record(
    rows: list[tuple[str, xtype.Mir_Term]],
    rest: xtype.Mir_Term | None = None,
) -> xtype.Mir_Term:
    return term(
        xtype.Mir_Term_view_Record(
            rows=[(applied_sym(name), val) for name, val in rows],
            rest=rest,
        )
    )


def case(
    scrutinee: xtype.Mir_Term,
    cases: list[tuple[str, xtype.Mir_Term]],
    default: xtype.Mir_Term | None = None,
) -> xtype.Mir_Term:
    return term(
        xtype.Mir_Term_view_Case(
            u=scrutinee,
            cases=[(applied_sym(name), rhs) for name, rhs in cases],
            default=default,
        )
    )


def sequence(items: list[xtype.Mir_Term], last: xtype.Mir_Term) -> xtype.Mir_Term:
    return term(xtype.Mir_Term_view_Sequence(args=(items, last)))

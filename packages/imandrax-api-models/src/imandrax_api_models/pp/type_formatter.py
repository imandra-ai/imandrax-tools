"""Pretty-printer for ImandraX MIR `Type`s (IML/OCaml type syntax)."""

from __future__ import annotations

import imandrax_api.lib as xtype

from .operators import short_id
from .pretty import (
    Doc,
    group,
    hcat,
    join,
    line,
    parens,
    text,
)

# Precedence levels
# =================
# Lowest binds loosest. A child rendered with context `ctx` parenthesises
# itself when `ctx` exceeds its own precedence.
#
#   arrow  a -> b -> c   right-assoc, loosest
#   tuple  a * b         binds tighter than arrow
#   app    int list      constructor application, tightest

_ARROW = 0
_TUPLE = 1
_APP = 2


def _paren_if(cond: bool, d: Doc) -> Doc:
    return parens(d) if cond else d


def _tyvar_name(uid: xtype.Uid) -> str:
    name = short_id(uid)
    return name if name.startswith("'") else f"'{name}"


def type2doc(t: xtype.Mir_Type) -> Doc:
    return _go(t, _ARROW)


def _go(t: xtype.Mir_Type, ctx: int) -> Doc:
    v = t.view
    match v:
        case xtype.Ty_view_view_Var(arg=uid):
            return text(_tyvar_name(uid))

        case xtype.Ty_view_view_Constr(args=(uid, [])):
            return text(short_id(uid))

        case xtype.Ty_view_view_Constr(args=(uid, [single])):
            # postfix application: `int list`
            return group(hcat(_go(single, _APP), text(' '), text(short_id(uid))))

        case xtype.Ty_view_view_Constr(args=(uid, many)):
            # parenthesised arg list: `(a, b) map`
            args = join(hcat(text(','), line), [_go(a, _ARROW) for a in many])
            return hcat(parens(group(args)), text(' '), text(short_id(uid)))

        case xtype.Ty_view_view_Tuple(arg=elems):
            body = join(hcat(text(' *'), line), [_go(e, _TUPLE + 1) for e in elems])
            return _paren_if(ctx > _TUPLE, group(body))

        case xtype.Ty_view_view_Arrow(args=(_lbl, dom, cod)):
            body = hcat(_go(dom, _ARROW + 1), text(' ->'), line, _go(cod, _ARROW))
            return _paren_if(ctx > _ARROW, group(body))

    raise ValueError(f'Unhandled type view: {type(v).__name__}')

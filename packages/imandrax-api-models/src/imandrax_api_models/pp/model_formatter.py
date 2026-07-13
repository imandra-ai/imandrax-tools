# pyright: basic
# ai-disclosure: autonomous
"""
Pretty-printer for ImandraX models (counter-models / instances).

A model is a set of bindings: constant values, function interpretations, and
type-domain interpretations. We render it as a flat brace block, one binding
per line, followed by the `representable`/`completed` flags.
"""

from __future__ import annotations

import re
from typing import Any

import imandrax_api.lib as xtype

from .pretty import (
    Doc,
    group,
    hardline,
    hcat,
    join,
    line,
    nest,
    text,
)
from .term_formatter import _applied_sym_short, term2doc
from .type_formatter import type2doc

_IDENT = re.compile(r'^[A-Za-z_][A-Za-z0-9_]*$')


def _sym2doc(s: Any) -> Doc:
    """Symbol name, parenthesised when it is an operator (OCaml convention)."""
    name = _applied_sym_short(s)
    return text(name if _IDENT.match(name) else f'({name})')


def _var2doc(v: Any) -> Doc:
    return hcat(text('('), text(v.id.name), text(' : '), type2doc(v.ty), text(')'))


# Bindings
# ========


def _const2doc(sym: Any, term: Any) -> Doc:
    return hcat(_sym2doc(sym), text(' = '), term2doc(term))


def _fun2doc(sym: Any, fi: Any) -> Doc:
    head: list[Doc] = [_sym2doc(sym)]
    for a in fi.fi_args:
        head += [text(' '), _var2doc(a)]
    head += [text(' : '), type2doc(fi.fi_ty_ret)]
    header = hcat(*head)
    if not fi.fi_cases:
        return hcat(header, text(' = '), term2doc(fi.fi_else))
    # piecewise interpretation
    cases: list[Doc] = []
    for guards, rhs in fi.fi_cases:
        g = join(text(' && '), [term2doc(x) for x in guards]) if guards else text('_')
        cases.append(hcat(text('| '), g, text(' -> '), term2doc(rhs)))
    cases.append(hcat(text('| _ -> '), term2doc(fi.fi_else)))
    body = nest(2, hcat(*[hcat(hardline, c) for c in cases]))
    return hcat(header, text(' ='), body)


def _ty_interp2doc(ty: Any, tydef: Any) -> Doc:
    match tydef:
        case xtype.Common_Model_ty_def_Ty_finite(arg=terms):
            dom = join(text(', '), [term2doc(t) for t in terms])
            return hcat(type2doc(ty), text(' = {'), dom, text('}'))
        case xtype.Common_Model_ty_def_Ty_alias_unit():
            return hcat(type2doc(ty), text(' = unit'))
    return hcat(type2doc(ty), text(' = ?'))


def _ty_subst2doc(uid: Any, ty: Any) -> Doc:
    return hcat(text(f"'{uid.name} := "), type2doc(ty))


# Block layout
# ============


def model2doc(m: xtype.Common_Model_t_poly) -> Doc:
    """Render a model as a flat block of bindings with its metadata flags."""
    bindings: list[Doc] = []
    bindings += [_const2doc(sym, t) for sym, t in m.consts]
    bindings += [_fun2doc(sym, fi) for sym, fi in m.funs]
    bindings += [_ty_interp2doc(ty, d) for ty, d in m.tys]
    bindings += [_ty_subst2doc(uid, ty) for uid, ty in m.ty_subst]

    flags = text(f' (representable={m.representable}, completed={m.completed})')

    if not bindings:
        return hcat(text('Model {}'), flags)
    if len(bindings) == 1:
        block = group(
            hcat(text('Model {'), nest(2, hcat(line, bindings[0])), line, text('}'))
        )
    else:
        inner = nest(2, hcat(*[hcat(hardline, b) for b in bindings]))
        block = hcat(text('Model {'), inner, hardline, text('}'))
    return hcat(block, flags)

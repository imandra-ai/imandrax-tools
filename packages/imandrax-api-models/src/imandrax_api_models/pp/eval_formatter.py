# pyright: basic
# ai-disclosure: ai-generated
"""
Pretty-printer for ImandraX evaluation values (`Eval.Value`).

Values are rendered in IML syntax (`Some 3`, `[1; 2]`, `{x = 1; y = 2}`, `"abc"`)
rather than as their wire shape.
"""

from __future__ import annotations

import re
from typing import Any, assert_never

import imandrax_api.lib as xtype

from .pretty import Doc, group, hcat, join, line, nest, python_obj, python_quote, text
from .term_formatter import term2doc as term2doc_
from .type_formatter import type2doc


def term2doc(t: xtype.Mir_Term) -> Doc:
    return python_obj('Term', [(None, python_quote(term2doc_(t)))])


# a (possibly module-qualified) OCaml identifier, e.g. `Blue`, `Ordinal.Int`
_IDENT = re.compile(r"^[A-Za-z_][A-Za-z0-9_']*(\.[A-Za-z_][A-Za-z0-9_']*)*$")

_STR_ESCAPES = {
    '\\': '\\\\',
    '"': '\\"',
    '\n': '\\n',
    '\t': '\\t',
    '\r': '\\r',
}


def _str_lit(s: str) -> str:
    """An OCaml string literal."""
    body = ''.join(_STR_ESCAPES.get(c, c) for c in s)
    return f'"{body}"'


def _cstor_name(name: str) -> str:
    return name if _IDENT.match(name) else f'({name})'


def top_fun2doc(v: tuple[list[Any], xtype.Mir_Term]) -> Doc:
    """`Mir.Top_fun` / eval task term: `fun (x : ty) -> body`, or just `body`."""
    vars, body = v
    if not vars:
        return term2doc(body)
    var_docs = [
        hcat(text('('), text(var.id.name), text(' : '), type2doc(var.ty), text(')'))
        for var in vars
    ]
    head = hcat(text('fun '), join(text(' '), var_docs), text(' ->'))
    return group(hcat(head, nest(2, hcat(line, term2doc(body)))))


def ordinal2doc(o: xtype.Eval_Ordinal, *, atomic: bool = False) -> Doc:
    """`Ordinal.Int 3` / `Ordinal.Cons (a, 2, b)`."""
    match o:
        case xtype.Eval_Ordinal_Int(arg=n):
            doc = hcat(text('Ordinal.Int '), text(str(n)))
        case xtype.Eval_Ordinal_Cons(args=(a, n, b)):
            inner = join(
                hcat(text(','), line),
                [ordinal2doc(a), text(str(n)), ordinal2doc(b)],
            )
            doc = group(hcat(text('Ordinal.Cons ('), nest(2, inner), text(')')))
        case _:
            assert_never(o)
    return hcat(text('('), doc, text(')')) if atomic else doc


CONS = '::'
NIL = '[]'


def _as_list(view: Any) -> list[xtype.Eval_Value] | None:
    """Collapse a `::`/`[]` cstor chain into its elements, else `None`."""
    items: list[xtype.Eval_Value] = []
    while True:
        match view:
            case xtype.Eval_Value_view_V_cstor(args=(cd, args)):
                name = cd.cd_name.name
                if name == NIL and not args:
                    return items
                if name == CONS and len(args) == 2:
                    items.append(args[0])
                    view = args[1].v
                    continue
                return None
            case _:
                return None


def value2doc(v: xtype.Eval_Value, *, atomic: bool = False) -> Doc:
    """
    Render an eval value in IML syntax.

    Args:
        atomic: parenthesize the result unless it is already atomic, so that it
            can be used as the argument of a constructor application.

    """
    return _view2doc(v.v, atomic=atomic)


def _view2doc(view: Any, *, atomic: bool) -> Doc:
    def parens_if_needed(doc: Doc) -> Doc:
        return hcat(text('('), doc, text(')')) if atomic else doc

    match view:
        case xtype.Eval_Value_view_V_true():
            return text('true')
        case xtype.Eval_Value_view_V_false():
            return text('false')
        case xtype.Eval_Value_view_V_int(arg=n):
            # negative literals need parens in argument position
            return parens_if_needed(text(str(n))) if n < 0 else text(str(n))
        case xtype.Eval_Value_view_V_real(arg=(num, den)):
            if den == 1:
                return parens_if_needed(text(f'{num}.')) if num < 0 else text(f'{num}.')
            return parens_if_needed(text(f'{num}/{den}'))
        case xtype.Eval_Value_view_V_string(arg=s):
            return text(_str_lit(s))
        case xtype.Eval_Value_view_V_uid(arg=uid):
            return text(uid.name)
        case xtype.Eval_Value_view_V_tuple(arg=vs):
            inner = join(hcat(text(','), line), [value2doc(x) for x in vs])
            return group(hcat(text('('), nest(2, inner), text(')')))
        case xtype.Eval_Value_view_V_cstor() if (items := _as_list(view)) is not None:
            inner = join(hcat(text(';'), line), [value2doc(x) for x in items])
            return group(hcat(text('['), nest(2, inner), text(']')))
        case xtype.Eval_Value_view_V_cstor(args=(cd, args)):
            name = text(_cstor_name(cd.cd_name.name))
            if not args:
                return name
            if len(args) == 1:
                doc = group(
                    hcat(name, nest(2, hcat(line, value2doc(args[0], atomic=True))))
                )
            else:
                inner = join(hcat(text(','), line), [value2doc(x) for x in args])
                doc = group(hcat(name, text(' ('), nest(2, inner), text(')')))
            return parens_if_needed(doc)
        case xtype.Eval_Value_view_V_record(args=(rd, vs)):
            rows = [
                hcat(text(f.name), text(' = '), value2doc(x))
                for f, x in zip(rd.rd_fields, vs, strict=False)
            ]
            inner = join(hcat(text(';'), line), rows)
            return group(hcat(text('{'), nest(2, inner), text('}')))
        case xtype.Eval_Value_view_V_closure(arg=c):
            missing = getattr(c, 'missing', None)
            if missing:
                return text(f'<fun; missing {missing} arg(s)>')
            return text('<fun>')
        case xtype.Eval_Value_view_V_quoted_term(arg=tf):
            return hcat(text('<<'), top_fun2doc(tf), text('>>'))
        case xtype.Eval_Value_view_V_ordinal(arg=o):
            return ordinal2doc(o, atomic=atomic)
        case xtype.Eval_Value_view_V_custom(arg=c):
            return text(f'<custom: {c!r}>')
        case _:
            return text(repr(view))

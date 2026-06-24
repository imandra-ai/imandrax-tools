"""
PO printers.

`value2doc` is a generic structural renderer over the decoded dataclass tree:

  * `Mir_Term`                      -> `term2doc`   (IML syntax)
  * `Common_Applied_symbol_t_poly`  -> `sym2doc`    (short symbol name)
  * `Uid`                           -> name (+ a compact tag for non-persistent
                                       views, so the discriminator is not lost)
  * dataclasses                     -> `ClassName` header + one line per field
  * lists / tuples / sets           -> inline `[a, b]` when every element is
                                       scalar, else a bullet list
  * scalars                         -> rendered directly
"""

from __future__ import annotations

from dataclasses import fields, is_dataclass
from typing import Any, assert_never

import imandrax_api.lib as xtype

from . import pretty as Pp
from .pretty import Doc
from .term_formatter import sym2doc, term2doc

type PO_task = xtype.Tasks_PO_task_Mir
type PO_res = xtype.Tasks_PO_res_Shallow


# Layout helpers
# ==============
#
# A "record" is a header line followed by indented field lines. These are
# deliberately *not* wrapped in groups: the top-level render mode is BREAK, so
# every `Pp.line` stays a real newline. `nest` is additive through the tree, so
# nested records indent correctly regardless of horizontal position. (term2doc
# still groups its own subtrees, which flatten locally as usual.)


# def _record(header: Doc, fields_: list[Doc]) -> Doc:
#     if not fields_:
#         return header
#     return Pp.hcat(header, Pp.nest(2, Pp.hcat(Pp.line, Pp.vsep(fields_))))


# def _inline(label: str, value: Doc) -> Doc:
#     """`label: value` -- the value continues on the same line."""
#     return Pp.hcat(Pp.text(f'{label}: '), value)


# def _block(label: str, body: Doc) -> Doc:
#     """`label:` header with `body` indented underneath."""
#     return _record(Pp.text(f'{label}:'), [body])


# def _bullets(docs: list[Doc]) -> Doc:
#     """One `- item` per line; continuation lines of each item indent under it."""
#     return Pp.vsep([Pp.hcat(Pp.text('- '), Pp.nest(2, d)) for d in docs])


# Generic structural renderer
# ===========================


def _is_scalar(v: Any) -> bool:
    return v is None or isinstance(v, (bool, int, float, str, bytes))


# def _is_inline(v: Any) -> bool:
#     """Whether `v` renders on a single (logical) line, so it can follow `label: `."""
#     if isinstance(v, (xtype.Mir_Term, xtype.Common_Applied_symbol_t_poly, xtype.Uid)):
#         return True
#     if _is_scalar(v):
#         return True
#     if isinstance(v, xtype.Ca_store_Ca_ptr_Raw):
#         return True
#     if isinstance(v, (list, tuple, set, frozenset)):
#         return all(_is_inline(x) for x in v)
#     return False


def _bytes2doc(b: bytes, limit: int = 64) -> Doc:
    if len(b) <= limit:
        return Pp.text(repr(b))
    head = b[:limit]
    return Pp.text(f'<{len(b)} bytes: {repr(head)}...>')


def _scalar2doc(v: None | bool | int | float | str | bytes) -> Doc:
    match v:
        case None:
            return Pp.text('None')
        case bool():
            return Pp.text('true' if v else 'false')
        case int() | float():
            return Pp.text(str(v))
        case str():
            return Pp.text(v)
        case bytes():
            return _bytes2doc(v)
        case _:
            assert_never(v)


# def _seq2doc(xs: list[Any] | tuple[Any, ...], opener: str, closer: str) -> Doc:
#     items = list(xs)
#     if not items:
#         return Pp.text(f'{opener}{closer}')
#     if all(_is_inline(x) for x in items):
#         return Pp.python_enclose(
#             Pp.text(opener), Pp.text(closer), [value2doc(x) for x in items]
#         )
#     return _bullets([value2doc(x) for x in items])


# def _record2doc(v: Any) -> Doc:
#     name = type(v).__name__
#     flds = fields(v)
#     if not flds:
#         return Pp.text(name)
#     rows = [_field2doc(f.name, getattr(v, f.name)) for f in flds]
#     return _record(Pp.text(name), rows)


# def _field2doc(name: str, v: Any) -> Doc:
#     return _inline(name, value2doc(v)) if _is_inline(v) else _block(name, value2doc(v))


def value2doc(v: Any) -> Doc:
    # Semantic dispatch for the types with dedicated pretty-printers
    if isinstance(v, xtype.Mir_Term):
        return term2doc(v)
    if isinstance(v, xtype.Common_Applied_symbol_t_poly):
        return sym2doc(v)
    if isinstance(v, xtype.Uid):
        return Pp.text(v.name)
    # Content-addressed pointer: just a key string
    if isinstance(v, xtype.Ca_store_Ca_ptr_Raw):
        return Pp.text(f'<Ca_store.Ca_ptr.Raw.key {v.key!r}>')
    # Scalars
    if _is_scalar(v):
        return _scalar2doc(v)
    # Collections
    if isinstance(v, list):
        return Pp.list_doc(v)
    if isinstance(v, tuple):
        return Pp.tupled(v)
    if isinstance(v, (set, frozenset)):
        # return _seq2doc(sorted(v, key=repr), '{', '}')
        return Pp.python_enclose(
            Pp.text('{'), Pp.text('}'), sorted((value2doc(i) for i in v), key=repr)
        )
    if isinstance(v, dict):
        # rows = [_field2doc(str(k), val) for k, val in v.items()]
        # return _record(Pp.text('{}'), rows) if rows else Pp.text('{}')
        return Pp.python_dict([(value2doc(k), value2doc(v)) for k, v in v.items()])
    # Dataclasses (the bulk of the tree)
    if is_dataclass(v) and not isinstance(v, type):
        return Pp.python_obj(
            v.__class__.__name__,
            [(f.name, value2doc(getattr(v, f.name))) for f in fields(v)],
        )
    return Pp.text(repr(v))

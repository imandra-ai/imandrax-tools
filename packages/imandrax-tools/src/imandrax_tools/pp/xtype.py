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


def _is_scalar(v: Any) -> bool:
    return v is None or isinstance(v, (bool, int, float, str, bytes))


def _bytes2doc(b: bytes, limit: int | None = None) -> Doc:
    if limit is None or limit == 0:
        return Pp.text(f'<{len(b)} bytes>')
    elif limit < 0 or len(b) <= limit:
        return Pp.text(repr(b))
    else:
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


def value2doc(v: Any) -> Doc:
    # Semantic dispatch for the types with dedicated pretty-printers
    match v:
        case xtype.Mir_Term():
            return term2doc(v)
        case xtype.Common_Applied_symbol_t_poly():
            return sym2doc(v)
        case xtype.Uid():
            return Pp.text(v.name)
        case xtype.Ca_store_Ca_ptr_Raw():
            return Pp.text(f'<Ca_store.Ca_ptr.Raw.key {v.key!r}>')
        case _ if _is_scalar(v):
            return _scalar2doc(v)
        # Collections
        case list():
            return Pp.list_doc([value2doc(i) for i in v])
        case tuple():
            return Pp.tupled([value2doc(i) for i in v])
        case set() | frozenset():
            return Pp.python_enclose(
                Pp.text('{'), Pp.text('}'), sorted((value2doc(i) for i in v), key=repr)
            )
        case dict():
            return Pp.python_dict([(value2doc(k), value2doc(v)) for k, v in v.items()])
        case _ if is_dataclass(v) and not isinstance(v, type):
            return Pp.python_obj(
                v.__class__.__name__,
                [(f.name, value2doc(getattr(v, f.name))) for f in fields(v)],
            )
        case _:
            raise NotImplementedError
            # return Pp.text(repr(v))


def show_value(v: Any) -> str:
    return Pp.pretty(88, value2doc(v))

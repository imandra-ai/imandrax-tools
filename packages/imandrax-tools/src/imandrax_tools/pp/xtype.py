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

from dataclasses import dataclass, fields, is_dataclass
from typing import Any

import imandrax_api.lib as xtype

from . import pretty as Pp
from .pretty import Doc
from .term_formatter import sym2doc, term2doc

type PO_task = xtype.Tasks_PO_task_Mir
type PO_res = xtype.Tasks_PO_res_Shallow


@dataclass
class PrinterConfig:
    bytes_limit: int | None = None


def _bytes2doc(b: bytes, limit: int | None = None) -> Doc:
    if limit is None or limit == 0:
        return Pp.text(f'<{len(b)} bytes>')
    elif limit < 0 or len(b) <= limit:
        return Pp.text(repr(b))
    else:
        head = b[:limit]
        return Pp.text(f'<{len(b)} bytes: {repr(head)}...>')


# def _is_scalar(v: Any) -> bool:
#     return v is None or isinstance(v, (bool, int, float, str, bytes))


# def _scalar2doc(v: None | bool | int | float | str | bytes) -> Doc:
#     match v:
#         case None:
#             return Pp.text('None')
#         case bool():
#             return Pp.text('True' if v else 'False')
#         case int() | float():
#             return Pp.text(str(v))
#         case str():
#             return Pp.text(v)
#         case bytes():
#             return _bytes2doc(v)
#         case _:
#             assert_never(v)


class Printer:
    """
    General printer for imandrax-api deserialized values.

    - MIR terms are printed with a dedicated printer
    - Values of some types are summarized
    - Configurable via `PrinterConfig`
    """

    config: PrinterConfig

    def __init__(self, config: PrinterConfig = PrinterConfig()):
        self.config = config

    def bytes2doc(self, v: bytes) -> Doc:
        return _bytes2doc(v, self.config.bytes_limit)

    def value2doc(self, v: Any) -> Doc:
        # Semantic dispatch for the types with dedicated pretty-printers
        match v:
            case bytes():
                return self.bytes2doc(v)
            case xtype.Mir_Term():
                return term2doc(v)
            case xtype.Common_Applied_symbol_t_poly():
                return sym2doc(v)
            case xtype.Uid():
                return Pp.text(v.name)
            case xtype.Ca_store_Ca_ptr_Raw():
                return Pp.text(f'<Ca_store.Ca_ptr.Raw.key {v.key!r}>')
            # Collections
            case list():
                return Pp.list_doc([self.value2doc(i) for i in v])
            case tuple():
                return Pp.tupled([self.value2doc(i) for i in v])
            case set() | frozenset():
                return Pp.python_enclose(
                    Pp.text('{'),
                    Pp.text('}'),
                    sorted((self.value2doc(i) for i in v), key=repr),
                )
            case dict():
                return Pp.python_dict(
                    [(self.value2doc(k), self.value2doc(v)) for k, v in v.items()]
                )
            case _ if is_dataclass(v) and not isinstance(v, type):
                return Pp.python_obj(
                    v.__class__.__name__,
                    [(f.name, self.value2doc(getattr(v, f.name))) for f in fields(v)],
                )
            case _:
                return Pp.text(repr(v))


def show_value(v: Any) -> str:
    printer = Printer(PrinterConfig())
    return Pp.pretty(88, printer.value2doc(v))

# ruff: noqa: F403, F405, D417
# pyright: reportUnusedExpression=false
# ruff: noqa: RUF100, F401
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
from functools import partial
from typing import Any

import imandrax_api.lib as xtype

from . import pretty as Pp
from ._common import *
from .goal_state import doc_of_sequent as Sequent2doc_raw
from .pretty import Doc, hcat, nil, python_obj, text
from .term_formatter import sym2doc, term2doc


@dataclass
class PrinterConfig:
    bytes_limit: int | None = None
    show_ca_store_key: bool = False
    show_po_task_db: bool = False
    unwrap_single_arg_dataclass: bool = True


def _bytes2doc(b: bytes, limit: int | None = None) -> Doc:
    if limit is None or limit == 0:
        return Pp.text(f'<{len(b)} bytes>')
    elif limit < 0 or len(b) <= limit:
        return Pp.text(repr(b))
    else:
        head = b[:limit]
        return Pp.text(f'<{len(b)} bytes: {repr(head)}...>')


def Sequent2doc(v: Sequent) -> Doc:
    inner = Sequent2doc_raw(v)
    inner = hcat(text("'"), inner, text("'"))
    return python_obj('Sequent', [(None, inner)])


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

    def dataclass_row_docs(
        self,
        v: Any,
        filter_none_values: bool = False,
    ) -> AssocList[Doc]:
        rows: AssocList[Doc] = []
        for fld in fields(v):
            val = getattr(v, fld.name)
            if filter_none_values and val is None:
                continue
            rows.append((fld.name, self.value2doc(val)))
        return rows

    def dataclass2doc(
        self,
        v: Any,
        unwrap_single_arg: bool,
        with_name: str | None = None,
        filter_none_values: bool = False,
    ) -> Doc:
        """
        General purpose pretty-printer for dataclasses.

        Field values with `nil` Doc are omitted from the output. So to remove
        a field from the output, set its printer to output `nil` in `value2doc`.

        Args:
            with_name: overrides the name of the dataclass
            unwrap_single_arg: if True, unwrap single-argument dataclasses

        """
        rows = self.dataclass_row_docs(v, filter_none_values=filter_none_values)
        if unwrap_single_arg and len(rows) == 1 and rows[0][0] == 'arg':
            rows = [(None, rows[0][1])]

        non_nil_rows = filter(lambda r: r[1] is not nil, rows)
        return Pp.python_obj(
            with_name or v.__class__.__name__,
            non_nil_rows,
        )

    def bytes2doc(self, v: bytes) -> Doc:
        return _bytes2doc(v, self.config.bytes_limit)

    # Custom xtype printers
    # --------------------

    def Tasks_PO_res_success_Proof2doc(
        self, v: xtype.Tasks_PO_res_success_Proof
    ) -> Doc:
        proof_found = v.arg
        return self.value2doc(proof_found)

    # def Tasks_PO_res_proof_found2doc(self, v: xtype.Tasks_PO_res_proof_found) -> Doc:
    #     return self.dataclass2doc(v, with_name='ProofFound')

    # def Proof_Proof_term2doc(
    #     self, v: xtype.Proof_Proof_term_t_poly[xtype.Mir_Term_term, xtype.Mir_Type]
    # ) -> Doc:
    #     return self.dataclass2doc(v, with_name='ProofTerm')
    # name = 'ProofTerm'
    # id = self.value2doc(v.id)
    # concl: Doc = Sequent2doc(v.concl)
    # view = self.value2doc(v.view)
    # return Pp.python_obj(
    #     name,
    #     [
    #         ('id', id),
    #         ('concl', concl),
    #         ('view', view),
    #     ],
    # )

    # def Anchor2doc(self, v: xtype.Anchor) -> Doc:
    #     match v:
    #         case Anchor_Named

    # def PO_res2doc(self, v: PO_res) -> Doc:

    # --------------------

    def value2doc(self, v: Any) -> Doc:
        dataclass2doc = partial(
            self.dataclass2doc,
            unwrap_single_arg=self.config.unwrap_single_arg_dataclass,
        )
        match v:
            case bytes():
                return self.bytes2doc(v)
            # ImandraX API types
            case xtype.Mir_Term():
                return term2doc(v)
            case xtype.Common_Applied_symbol_t_poly():
                return sym2doc(v)
            case xtype.Uid():
                return Pp.text(v.name)
            case xtype.Ca_store_Ca_ptr_Raw():
                if self.config.show_ca_store_key:
                    return Pp.text(f'<Ca_store.Ca_ptr.Raw.key {v.key!r}>')
                else:
                    return nil
            case xtype.Common_Db_ser_t_poly() if not self.config.show_po_task_db:
                return nil
            case xtype.Common_Sequent_t_poly():
                return Sequent2doc(v)
            case xtype.Tasks_PO_res_success_Proof():
                return self.Tasks_PO_res_success_Proof2doc(v)
            case xtype.Tasks_PO_res_proof_found():
                return dataclass2doc(v, with_name='ProofFound')
            case xtype.Proof_Proof_term_t_poly():
                return dataclass2doc(v, with_name='ProofTerm')
            case xtype.Tasks_PO_res_shallow_poly():
                return dataclass2doc(v, with_name='PORes')
            case (
                xtype.Proof_Arg_A_term()
                | xtype.Proof_Arg_A_ty()
                | xtype.Proof_Arg_A_int()
                | xtype.Proof_Arg_A_string()
                | xtype.Proof_Arg_A_list()
                | xtype.Proof_Arg_A_dict()
                | xtype.Proof_Arg_A_seq()
            ):
                # Strip the tag name in proof arg ADT
                return dataclass2doc(v, with_name='ProofArg')
            # PO task
            case xtype.Common_Proof_obligation_t_poly():
                return dataclass2doc(
                    v, with_name='ProofObligation', filter_none_values=True
                )
            # Collections
            case list():
                docs = [self.value2doc(i) for i in v]
                docs = filter(lambda d: d is not nil, docs)
                return Pp.list_doc(docs)
            case tuple():
                docs = [self.value2doc(i) for i in v]
                docs = filter(lambda d: d is not nil, docs)
                return Pp.tupled(docs)
            case set() | frozenset():
                docs = [self.value2doc(i) for i in v]
                docs = filter(lambda d: d is not nil, docs)
                docs = sorted(docs, key=repr)
                return Pp.python_enclose(
                    Pp.text('{'),
                    Pp.text('}'),
                    docs,
                )
            case dict():
                return Pp.python_dict(
                    [(self.value2doc(k), self.value2doc(v)) for k, v in v.items()]
                )
            case _ if is_dataclass(v) and not isinstance(v, type):
                rows = self.dataclass_row_docs(v)
                non_nil_rows = filter(lambda r: r[1] is not nil, rows)
                return Pp.python_obj(
                    v.__class__.__name__,
                    non_nil_rows,
                )
            case _:
                return Pp.text(repr(v))

    def __call__(self, v: Any) -> Doc:
        return self.value2doc(v)


def show_value(v: Any) -> str:
    printer = Printer(PrinterConfig())
    return Pp.pretty(88, printer.value2doc(v))

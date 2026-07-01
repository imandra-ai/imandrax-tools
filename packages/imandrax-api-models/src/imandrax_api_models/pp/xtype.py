# ruff: noqa: F403, F405, D417
# pyright: reportUnusedExpression=false
# ruff: noqa: RUF100, F401
"""
ImandraX API type printer.

- All outputs aim to be valid Python repr
- For DSL-syntax, they are wrapped in `python_quote`
"""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass, fields, is_dataclass
from functools import partial
from typing import Any, Literal, assert_never, cast

import imandrax_api.lib as xtype

from . import pretty as Pp
from ._common import *
from ._common import fmt_duration
from .decomp import (
    drop_meta_paths,
    region2doc,
    region_meta2doc,
    region_meta_assoc2doc,
)
from .goal_state import doc_of_sequent as Sequent2doc_raw
from .model_formatter import model2doc
from .pretty import (
    Doc,
    enclose_sep,
    flatten,
    group,
    hcat,
    line,
    nil,
    punctuate,
    python_dict,
    python_obj,
    python_quote,
    text,
    vsep,
)
from .proof_formatter import proof2doc
from .report_formatter import report2doc
from .term_formatter import sym2doc, term2doc as term2doc_, terms2doc
from .type_formatter import type2doc as type2doc_


def term2doc(t: Term) -> Doc:
    return python_obj('Term', [(None, python_quote(term2doc_(t)))])


def type2doc(t: Type) -> Doc:
    return python_obj('Type', [(None, python_quote(type2doc_(t)))])


def snake_to_camel(s: str) -> str:
    return ''.join(w.capitalize() for w in s.split('_'))


@dataclass
class PrinterConfig:
    bytes_limit: int | None = None
    show_ca_store_key: bool = False
    show_po_task_db: bool = False
    show_po_res_report: bool = (
        False  # Note: report can be found in a dedicated artifact
    )
    show_decomp_task_db: bool = False
    show_decomp_res_report: bool = False
    show_anchor_hash: bool = False
    """append a short chash to anchor/cname names"""
    summarize_po_task: bool = False
    """Show PO task as a summarized string. Useful in success case"""
    hide_po_res_success_cases: bool = False
    """Replace body of success cases with `...`"""
    report_expand_payloads: bool = False
    """render full models/SMT proofs in reports"""
    concise_region_repr: bool = True
    unwrap_single_arg_dataclass: bool = True
    ascii_only: bool = False


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


type Goal = tuple[list[xtype.Common_Var_t_poly[xtype.Mir_Type]], xtype.Mir_Term]
"""fun vars -> body"""


def Goal2doc(v: Goal, ty2doc: Callable[[xtype.Mir_Type], Doc]) -> Doc:
    vars: list[tuple[xtype.Uid, xtype.Mir_Type]] = [(var.id, var.ty) for var in v[0]]
    var_docs = [
        hcat(text('('), text(var_id.name), text(' : '), ty2doc(var_ty), text(')'))
        for var_id, var_ty in vars
    ]
    body = v[1]
    goal_doc = vsep(
        [
            text('fun'),
            *var_docs,
            text('->'),
            term2doc(body),
        ]
    )
    # return hcat(text('<'), text('Goal'), text(' '), goal_doc, text('>'))
    return python_obj('Goal', [(None, python_quote(goal_doc, single_quote=False))])


class Printer:
    """
    General printer for imandrax-api deserialized values.

    - MIR terms are printed with a dedicated printer
    - Values of some types are summarized
    - Configurable via `PrinterConfig`
    """

    config: PrinterConfig
    debug: bool

    def __init__(self, config: PrinterConfig = PrinterConfig(), debug: bool = False):
        self.config = config
        self.debug = debug

    def dataclass_row_docs(
        self,
        v: Any,
        filter_none_values: bool = False,
        filter_p: Callable[[str, Any], bool] | None = None,
    ) -> AssocList[Doc]:
        rows: AssocList[Doc] = []
        for fld in fields(v):
            val = getattr(v, fld.name)
            if filter_none_values and val is None:
                continue
            if filter_p is not None and not filter_p(fld.name, val):
                continue
            rows.append((fld.name, self.value2doc(val)))
        return rows

    def dataclass2doc(
        self,
        v: Any,
        unwrap_single_arg: bool,
        with_name: str | None = None,
        ignore_fields: list[str] | None = None,
        filter_none_values: bool = False,
        filter_p: Callable[[str, Any], bool] | None = None,
    ) -> Doc:
        """
        General purpose pretty-printer for dataclasses.

        Field values with `nil` Doc are omitted from the output. So to remove
        a field from the output, set its printer to output `nil` in `value2doc`.

        Args:
            with_name: overrides the name of the dataclass
            unwrap_single_arg: if True, unwrap single-argument dataclasses
            filter_p: a function that filters fields by name and value

        """
        ignore_fields = ignore_fields or []
        rows = self.dataclass_row_docs(
            v,
            filter_none_values=filter_none_values,
            filter_p=filter_p,
        )
        rows = [r for r in rows if r[0] not in ignore_fields]
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

    def Cname2doc(self, c: xtype.Cname_t_) -> Doc:
        """A content-addressed name: `name`, or `name/<short hash>` when configured."""
        if self.config.show_anchor_hash and c.chash:
            return Pp.text(f'{c.name}/{c.chash[:3].hex()}')
        return Pp.text(c.name)

    def Anchor2doc(self, a: xtype.Anchor) -> Doc:
        """
        Anchor as a content-addressed name qualified by transformation modifiers.

        Modifiers are slash-prefixed, outermost first, e.g.
        `proof_check/_verify_11_8`, `decomp/eval#3`.
        """
        match a:
            case xtype.Anchor_Named(arg=c):
                return self.Cname2doc(c)
            case xtype.Anchor_Eval(arg=n):
                return Pp.text(f'eval#{n}')
            case xtype.Anchor_Proof_check(arg=inner):
                return hcat(text('proof_check/'), self.Anchor2doc(inner))
            case xtype.Anchor_Decomp(arg=inner):
                return hcat(text('decomp/'), self.Anchor2doc(inner))
            case _:
                assert_never(a)

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
            # ====================
            # General
            # --------------------
            case xtype.Mir_Term():
                return term2doc(v)
            case xtype.Mir_Type():
                return type2doc(v)
            case xtype.Common_Applied_symbol_t_poly():
                return sym2doc(v)
            case xtype.Uid():
                return Pp.text(v.name)
            case xtype.Ca_store_Ca_ptr_Raw():
                if self.config.show_ca_store_key:
                    return Pp.text(f'<Ca_store.Ca_ptr.Raw.key {v.key!r}>')
                else:
                    return nil
            case xtype.Error_Error_core():
                return dataclass2doc(
                    v,
                    with_name='Error',
                    filter_p=lambda k, v: False if k == 'stack' and v == [] else True,
                )
            case xtype.Error_Kind(name):
                return python_quote(text(name))
            case xtype.Error_Error_core_message():
                rows = self.dataclass_row_docs(v, filter_none_values=True)
                rows_ = [(python_quote(text(k)), v) for k, v in rows]
                return python_dict(rows_)
            case xtype.Common_Model_t_poly():
                return python_obj(
                    'Model',
                    [(None, python_quote(model2doc(v), single_quote=False))],
                )
            case xtype.Statistics():
                if v.time_s < 0 and v.tactic is None:
                    # Ignore invalid / empty stats
                    return nil
                else:
                    rows = [
                        ('time_s', python_quote(text(fmt_duration(v.time_s)))),
                        ('tactic', self.value2doc(v.tactic)),
                    ]
                    return python_obj('TacticExecStats', rows)
            case xtype.Report_Report():
                return report2doc(
                    v,
                    value2doc=self.value2doc,
                    expand_payloads=self.config.report_expand_payloads,
                    ascii_only=self.config.ascii_only,
                )
            case (
                xtype.Common_Verify_kind_K_verify()
                | xtype.Common_Verify_kind_K_instance()
                | xtype.Common_Verify_kind_K_test()
            ):
                name = (
                    type(v).__name__.removeprefix('Common_Verify_kind_K_').capitalize()
                )
                return text(f'VerifyKind.{name}')
            # PO res
            case xtype.Tasks_PO_res_shallow_poly():
                ignore_fields = (
                    ['report'] if not self.config.show_po_res_report else None
                )
                return dataclass2doc(
                    v,
                    with_name='PORes',
                    ignore_fields=ignore_fields,
                    filter_p=lambda _, v: not (isinstance(v, list) and len(v) == 0),
                )
            case (
                xtype.Tasks_PO_res_success_Proof()
                | xtype.Tasks_PO_res_success_Instance()
                | xtype.Tasks_PO_res_success_Verified_upto()
                | xtype.Tasks_PO_res_success_Test_ok()
            ):
                with_name = type(v).__name__.removeprefix('Tasks_PO_res_success_')
                with_name = snake_to_camel(with_name)
                with_name = 'POSuccess' + with_name
                if isinstance(v, xtype.Tasks_PO_res_success_Test_ok):
                    # Test_ok has no .arg
                    return dataclass2doc(v, with_name=with_name)
                else:
                    if self.config.hide_po_res_success_cases:
                        inner_doc = text('...')
                        return python_obj(with_name, [(None, inner_doc)])
                    else:
                        return dataclass2doc(v.arg, with_name=with_name)
            case xtype.Tasks_PO_res_error_No_proof():
                return dataclass2doc(
                    v.arg, with_name='POErrorNoProof', filter_none_values=True
                )
            case xtype.Tasks_PO_res_error_Error():
                return dataclass2doc(v.arg, with_name='POErrorError')
            case xtype.Tasks_PO_res_error_Invalid_model():
                rows = [
                    ('error', self.value2doc(v.args[0])),
                    ('model', self.value2doc(v.args[1])),
                ]
                return python_obj(
                    'POErrorInvalidModel',
                    rows,
                )
            case xtype.Tasks_PO_res_error_Unsat():
                return dataclass2doc(v.arg, with_name='POErrorUnsat')
            case xtype.Common_Sequent_t_poly():
                return Sequent2doc(v)
            case xtype.Proof_Proof_term_t_poly():
                return python_obj(
                    'ProofTerm',
                    [
                        (
                            None,
                            python_quote(
                                proof2doc(v, ascii_only=self.config.ascii_only),
                                single_quote=False,
                            ),
                        )
                    ],
                )
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
            case (
                xtype.Anchor_Named()
                | xtype.Anchor_Eval()
                | xtype.Anchor_Proof_check()
                | xtype.Anchor_Decomp()
            ):
                inner = hcat(text("'"), self.Anchor2doc(v), text("'"))
                return python_obj('Anchor', [(None, inner)])
            case xtype.Sub_anchor():
                inner = text(f"'{v.fname}#{v.anchor}'")
                return python_obj('SubAnchor', [(None, inner)])
            case xtype.Cname_t_():
                return self.Cname2doc(v)
            # PO task
            case xtype.Tasks_PO_task_t_poly():
                if not self.config.summarize_po_task:
                    return dataclass2doc(
                        v,
                        with_name='POTask',
                        ignore_fields=['db']
                        if not self.config.show_po_task_db
                        else None,
                    )
                else:
                    sym_info = f'{v.from_sym}#{v.count}'
                    po_descr = v.po.descr
                    return python_obj(
                        'POTask',
                        [(None, python_quote(text(f'{sym_info}: {po_descr}')))],
                    )
            case xtype.Common_Proof_obligation_t_poly():
                rows: AssocList[Doc] = []
                for fld in fields(v):
                    key = fld.name
                    val = getattr(v, key)
                    if val is None:
                        continue
                    if key == 'goal':
                        val_doc = Goal2doc(val, ty2doc=type2doc)
                    elif key == 'is_instance' and not val:
                        continue
                    elif key == 'named_hypotheses':
                        if not val:
                            continue
                        val = cast(list[tuple[str, xtype.Mir_Term]], val)
                        val_doc = python_dict(
                            [(text(name), term2doc(term)) for name, term in val]
                        )
                    else:
                        val_doc = self.value2doc(val)
                    rows.append((key, val_doc))
                return python_obj('ProofObligation', rows)
            case xtype.Common_Var_t_poly():
                return dataclass2doc(v, with_name='Var')
            case xtype.Common_Tactic_t_poly_Default_termination():
                return dataclass2doc(v, with_name='TacticDefaultTermination')
            case xtype.Common_Tactic_t_poly_Default_thm():
                return dataclass2doc(v, with_name='TacticDefaultTheorem')
            case xtype.Common_Tactic_t_poly_Default_test():
                return dataclass2doc(v, with_name='TacticDefaultTest')
            case xtype.Common_Tactic_t_poly_Term():
                return dataclass2doc(v, with_name='Tactic')
            # Decomp
            case xtype.Tasks_Decomp_task_t_poly():
                ignore_fields = ['db'] if not self.config.show_decomp_task_db else None
                return dataclass2doc(
                    v,
                    with_name='DecompTask',
                    ignore_fields=ignore_fields,
                )
            case (
                xtype.Tasks_Decomp_task_decomp_poly_Decomp()
                | xtype.Tasks_Decomp_task_decomp_poly_Term()
                | xtype.Tasks_Decomp_task_decomp_poly_Return()
                | xtype.Tasks_Decomp_task_decomp_poly_Prune()
                | xtype.Tasks_Decomp_task_decomp_poly_Merge()
                | xtype.Tasks_Decomp_task_decomp_poly_Compound_merge()
                | xtype.Tasks_Decomp_task_decomp_poly_Combine()
                | xtype.Tasks_Decomp_task_decomp_poly_Get()
                | xtype.Tasks_Decomp_task_decomp_poly_Let()
            ):
                name = type(v).__name__.removeprefix('Tasks_Decomp_task_decomp_poly_')
                return self.dataclass2doc(v, with_name=name, unwrap_single_arg=False)
            case xtype.Tasks_Decomp_res_shallow_poly():
                ignore_fields = None if self.config.show_decomp_task_db else ['report']
                return dataclass2doc(
                    v, with_name='DecompRes', ignore_fields=ignore_fields
                )
            case xtype.Tasks_Decomp_res_success():
                return dataclass2doc(v, with_name='DecompResSuccess')
            case xtype.Tasks_Decomp_res_error_Error():
                return dataclass2doc(v, with_name='DecompResError')
            case xtype.Common_Fun_decomp_t_poly():
                return dataclass2doc(v, with_name='FunDecomp')
            # TODO: make following two different modes provided by decomp.py
            case xtype.Common_Region_t_poly() if not self.config.concise_region_repr:
                rows: AssocList[Doc] = []
                for fld in fields(v):
                    key = fld.name
                    val = getattr(v, key)
                    if key == 'meta':
                        val = cast(AssocList[xtype.Common_Region_meta], val)
                        meta_doc = region_meta_assoc2doc(val)
                        rows.append(('meta', meta_doc))
                    else:
                        rows.append((key, self.value2doc(val)))
                return python_obj('Region', rows)
            case xtype.Common_Region_t_poly() if self.config.concise_region_repr:
                return region2doc(v, self.value2doc)
            case (
                xtype.Common_Region_status_Unknown()
                | xtype.Common_Region_status_Feasible()
                | xtype.Common_Region_status_Feasibility_check_failed()
            ):
                kind = type(v).__name__.removeprefix('Common_')
                kind = snake_to_camel(kind)
                if (
                    isinstance(v, xtype.Common_Region_status_Feasible)
                    and self.config.concise_region_repr
                ):
                    # Ignore model since it's already in meta
                    return python_obj(kind, [(None, text('...'))])
                else:
                    return dataclass2doc(v, with_name=kind)

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
                return dataclass2doc(v)
            case _:
                return Pp.text(repr(v))

    def __call__(self, v: Any) -> Doc:
        return self.value2doc(v)


def to_string(v: Any, **kwargs: Any) -> str:
    printer = Printer(PrinterConfig(**kwargs))
    return Pp.pretty(88, printer.value2doc(v))


def show_value(v: Any, **kwargs: Any) -> None:
    return print(to_string(v, **kwargs))

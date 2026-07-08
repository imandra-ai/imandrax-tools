"""
Pretty-printer for ImandraX reports, as a box-drawing event tree.

A `Report` is a linear event trace whose `EL_enter_span`/`EL_exit_span`
markers are balanced. We reconstruct the implied tree of spans (with
`EL_atomic` events and nested spans as children) and render it with the shared
`pretty.tree` layout. Each node's label is its atomic event, rendered
compactly; spans additionally show their duration (exit ts - enter ts).
"""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass, field
from functools import partial
from typing import Any

import imandrax_api.lib as xtype

from . import pretty as Pp
from ._common import fmt_duration
from .goal_state import doc_of_sequent
from .pretty import Doc, hcat, join, line, nil, text, tree as tree_
from .term_formatter import term2doc

# TODO: find all non-ascii characters


@dataclass
class _Ctx:
    value2doc: Callable[[Any], Doc]
    expand_payloads: bool


@dataclass
class _Node:
    header: Doc
    children: list[_Node] = field(default_factory=list)
    enter: float | None = None  # enter timestamp, for span duration
    duration: float | None = None


def _leaf(d: Doc) -> _Node:
    return _Node(d)


def _kv(label: str, d: Doc) -> _Node:
    return _Node(hcat(text(f'{label}: '), d))


# Rich text
# =========


def _rtext2doc(items: list, ctx: _Ctx) -> Doc:
    parts: list[Doc] = []
    for it in items:
        match it:
            case (
                xtype.Report_Rtext_item_S(arg=s)
                | xtype.Report_Rtext_item_B(arg=s)
                | xtype.Report_Rtext_item_I(arg=s)
            ):
                parts.append(text(s))
            case xtype.Report_Rtext_item_Newline():
                parts.append(line)
            case xtype.Report_Rtext_item_Sub(arg=sub):
                parts.append(_rtext2doc(sub, ctx))
            case xtype.Report_Rtext_item_L(arg=lst):
                parts.append(join(text(', '), [_rtext2doc(x, ctx) for x in lst]))
            case xtype.Report_Rtext_item_Uid(arg=u):
                parts.append(text(u.name))
            case xtype.Report_Rtext_item_Term(arg=t):
                parts.append(term2doc(t))
            case xtype.Report_Rtext_item_Sequent(arg=sq):
                parts.append(doc_of_sequent(sq))
            case xtype.Report_Rtext_item_Subst(arg=pairs):
                parts.append(_subst2doc(pairs))
    return hcat(*parts) if parts else nil


def _subst2doc(pairs: list[tuple[Any, Any]]) -> Doc:
    binds = [hcat(term2doc(a), text(' ↦ '), term2doc(b)) for a, b in pairs]
    return hcat(text('['), join(text(', '), binds), text(']'))


# Model
# =====


def _model_node(model: Any, ctx: _Ctx) -> _Node:
    if ctx.expand_payloads:
        return _kv('model', ctx.value2doc(model))
    nc, nf = len(model.consts), len(model.funs)
    cs = '' if nc == 1 else 's'
    fs = '' if nf == 1 else 's'
    return _leaf(text(f'model ({nc} const{cs}, {nf} fun{fs})'))


# Atomic events
# =============


def _atom_node(ev: Any, ctx: _Ctx) -> _Node:
    match ev:
        case xtype.Report_Atomic_event_poly_E_enter_tactic(arg=s):
            return _leaf(text(f'tactic {s!r}'))
        case xtype.Report_Atomic_event_poly_E_title(arg=s):
            return _leaf(hcat(text('# '), text(s)))
        case xtype.Report_Atomic_event_poly_E_message(arg=rt):
            return _leaf(_rtext2doc(rt, ctx))
        case xtype.Report_Atomic_event_poly_E_enter_waterfall(goal=g):
            return _leaf(hcat(text('waterfall ⊢ '), term2doc(g)))
        case xtype.Report_Atomic_event_poly_E_attack_subgoal(name=nm, goal=g, depth=d):
            return _Node(
                text(f'subgoal {nm!r} (depth {d})'), [_kv('goal', doc_of_sequent(g))]
            )
        case xtype.Report_Atomic_event_poly_E_induction_scheme(arg=t):
            return _leaf(hcat(text('induction: '), term2doc(t)))
        case xtype.Report_Atomic_event_poly_E_rw_success(args=(rule, t, t2)):
            return _leaf(
                hcat(
                    text(f'rw✓ {rule.rw_name}: '),
                    term2doc(t),
                    text(' ↦ '),
                    term2doc(t2),
                )
            )
        case xtype.Report_Atomic_event_poly_E_rw_fail(args=(rule, t, msg)):
            return _leaf(
                hcat(text(f'rw✗ {rule.rw_name}: '), term2doc(t), text(f' — {msg}'))
            )
        case xtype.Report_Atomic_event_poly_E_inst_success(args=(_rule, t)):
            return _leaf(hcat(text('inst: '), term2doc(t)))
        case xtype.Report_Atomic_event_poly_E_simplify_t(args=(t, t2)):
            return _leaf(hcat(text('simp: '), term2doc(t), text(' ↦ '), term2doc(t2)))
        case xtype.Report_Atomic_event_poly_E_simplify_clause(args=(t, ts)):
            return _Node(
                hcat(text('simp clause: '), term2doc(t)),
                [_kv('to', Pp.list_doc([term2doc(x) for x in ts]))],
            )
        case xtype.Report_Atomic_event_poly_E_fun_expansion(args=(t, t2)):
            return _leaf(hcat(text('expand: '), term2doc(t), text(' ↦ '), term2doc(t2)))
        case xtype.Report_Atomic_event_poly_E_proved_by_smt(args=(t, proof)):
            children = (
                [_kv('smt_proof', ctx.value2doc(proof))] if ctx.expand_payloads else []
            )
            return _Node(hcat(text('proved by SMT: '), term2doc(t)), children)
        case xtype.Report_Atomic_event_poly_E_refuted_by_smt(args=(t, model)):
            children = [_model_node(model, ctx)] if model is not None else []
            return _Node(hcat(text('refuted by SMT: '), term2doc(t)), children)
        case xtype.Report_Atomic_event_poly_E_waterfall_checkpoint(arg=seqs):
            n = len(seqs)
            hdr = text(f'checkpoint: {n} sequent{"" if n == 1 else "s"}')
            kids = (
                [_leaf(doc_of_sequent(s)) for s in seqs] if ctx.expand_payloads else []
            )
            return _Node(hdr, kids)
    return _leaf(text(f'<{type(ev).__name__}>'))


# Tree reconstruction and layout
# ==============================


def _build_forest(events: list, ctx: _Ctx) -> list[_Node]:
    root: list[_Node] = []
    children_stack: list[list[_Node]] = [root]
    span_stack: list[_Node] = []
    for e in events:
        match e:
            case xtype.Report_Event_t_linear_EL_atomic(ev=ev):
                children_stack[-1].append(_atom_node(ev, ctx))
            case xtype.Report_Event_t_linear_EL_enter_span(ts=ts, ev=ev):
                node = _atom_node(ev, ctx)
                node.enter = ts
                children_stack[-1].append(node)
                span_stack.append(node)
                children_stack.append(node.children)
            case xtype.Report_Event_t_linear_EL_exit_span(ts=ts):
                if span_stack:
                    node = span_stack.pop()
                    children_stack.pop()
                    if node.enter is not None:
                        node.duration = ts - node.enter
    return root


def _node2doc(n: _Node, tree: Callable[[Doc, list[Doc]], Doc]) -> Doc:
    header = n.header
    if n.duration is not None:
        header = hcat(header, text(f'  ({fmt_duration(n.duration)})'))
    if n.children:
        return tree(header, [_node2doc(c, tree) for c in n.children])
    return header


def report2doc(
    report: xtype.Report_Report,
    *,
    value2doc: Callable[[Any], Doc],
    expand_payloads: bool = False,
    ascii_only: bool = False,
) -> Doc:
    """
    Render a report as an event tree.

    Args:
        report: the decoded report (a linear event trace).
        value2doc: the host printer, used to render heavy payloads (models,
            SMT proofs) when `expand_payloads` is set.
        expand_payloads: if True, render embedded models/SMT proofs in full;
            otherwise summarize them to a one-liner.
        ascii_only: if True, use ASCII characters only for tree indentation.

    """
    tree = partial(tree_, ascii_only=ascii_only)

    ctx = _Ctx(value2doc=value2doc, expand_payloads=expand_payloads)
    forest = _build_forest(list(report.events), ctx)
    return tree(text('Report'), [_node2doc(n, tree) for n in forest])

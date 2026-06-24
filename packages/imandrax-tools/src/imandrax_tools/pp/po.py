"""
Pretty-printers for proof-obligation tasks and results.

`po_task2doc` / `po_res2doc` render the structured `Tasks_PO_task_Mir` and
`Tasks_PO_res_Shallow` objects into compact, readable `Doc`s, eliding the
noisy fields that dominate the default Python `repr` (the serialized `db`,
the `report` archive, the `from_` pointer and the tactic tree).

`goal_state.py` covers the goal-state view of the error case; this module is
the whole-object view. The two are intentionally independent.
"""

from __future__ import annotations

import imandrax_api.lib as xtype

from . import pretty as Pp
from .pretty import Doc
from .term_formatter import sym2doc, term2doc

type PO_task = xtype.Tasks_PO_task_Mir
type PO_res = xtype.Tasks_PO_res_Shallow
type Sequent = xtype.Common_Sequent_t_poly[xtype.Mir_Term]
type Model = xtype.Common_Model_t_poly[xtype.Mir_Term, xtype.Mir_Type]
type ModelFi = xtype.Common_Model_fi[xtype.Mir_Term, xtype.Mir_Type]
type AppliedSym = xtype.Common_Applied_symbol_t_poly[xtype.Mir_Type]


# Layout helpers
# ==============
#
# A "record" is a header line followed by indented field lines, e.g.
#
#     PO res
#       res: Instance
#       time: 0.04s
#
# These are deliberately *not* wrapped in groups: the top-level render mode is
# BREAK, so every `Pp.line` stays a real newline (term2doc still groups its own
# subtrees, which flatten locally as usual).


def _record(header: Doc, fields: list[Doc]) -> Doc:
    """`header` followed by `fields`, each on its own line, indented by 2."""
    if not fields:
        return header
    return Pp.hcat(header, Pp.nest(2, Pp.hcat(Pp.line, Pp.vsep(fields))))


def _inline(label: str, value: Doc) -> Doc:
    """`label: value` on one (logical) line."""
    return Pp.hcat(Pp.text(f'{label}: '), value)


def _block(label: str, body: Doc) -> Doc:
    """`label:` header with `body` indented underneath."""
    return _record(Pp.text(f'{label}:'), [body])


# Sequents
# ========


def _labeled_term2doc(labeled: tuple[None | str, xtype.Mir_Term]) -> Doc:
    label, term = labeled
    if label is not None:
        return Pp.hcat(Pp.text(f'{label}: '), term2doc(term))
    return term2doc(term)


def _sequent2doc(sg: Sequent) -> Doc:
    hyps = Pp.punctuate(Pp.text(', '), [_labeled_term2doc(h) for h in sg.hyps])
    concls = Pp.punctuate(Pp.text(', '), [_labeled_term2doc(c) for c in sg.concls])
    return Pp.group(Pp.hcat(hyps, Pp.text(' ⊢ '), concls))


def _subgoals2doc(subgoals: list[Sequent]) -> Doc:
    if not subgoals:
        return Pp.text('∎')
    return Pp.vsep([Pp.hcat(Pp.text('- '), _sequent2doc(sg)) for sg in subgoals])


# Models
# ======


def _fi2doc(sym: AppliedSym, fi: ModelFi) -> Doc:
    cases: list[Doc] = []
    for args, result in fi.fi_cases:
        args_doc = (
            Pp.punctuate(Pp.text(', '), [term2doc(a) for a in args])
            if args
            else Pp.text('_')
        )
        cases.append(
            Pp.hcat(Pp.text('| '), args_doc, Pp.text(' -> '), term2doc(result))
        )
    cases.append(Pp.hcat(Pp.text('| _ -> '), term2doc(fi.fi_else)))
    return _record(Pp.hcat(sym2doc(sym), Pp.text(':')), [Pp.vsep(cases)])


def model2doc(model: Model) -> Doc:
    """Render a model's constants (`name = value`) and functions (case tables)."""
    rows: list[Doc] = []
    for sym, term in model.consts:
        rows.append(Pp.hcat(sym2doc(sym), Pp.text(' = '), term2doc(term)))
    for sym, fi in model.funs:
        rows.append(_fi2doc(sym, fi))
    if not rows:
        return Pp.text('(empty model)')
    return Pp.vsep(rows)


# Errors
# ======


def _error2doc(err: xtype.Error_Error_core) -> Doc:
    return Pp.text(f'[{err.kind.name}] {err.msg.msg}')


# Result
# ======


def _res2doc(
    res: xtype.Tasks_PO_res_success[xtype.Mir_Term, xtype.Mir_Type]
    | xtype.Tasks_PO_res_error[xtype.Mir_Term, xtype.Mir_Type],
) -> Doc:
    match res:
        # Success cases
        case xtype.Tasks_PO_res_success_Proof():
            return Pp.text('Proof')
        case xtype.Tasks_PO_res_success_Instance(arg=inst):
            return _record(
                Pp.text('Instance'), [_block('model', model2doc(inst.model))]
            )
        case xtype.Tasks_PO_res_success_Verified_upto(arg=vu):
            steps = vu.upto.arg if isinstance(vu.upto, xtype.Upto_N_steps) else '?'
            return Pp.text(f'Verified up to {steps} steps')
        case xtype.Tasks_PO_res_success_Test_ok():
            return Pp.text('Test ok')
        # Error cases
        case xtype.Tasks_PO_res_error_No_proof(arg=np):
            if np.counter_model is not None:
                return _record(
                    Pp.text('Counter-model'),
                    [_block('model', model2doc(np.counter_model))],
                )
            return _record(
                Pp.text('No proof'),
                [_block('subgoals', _subgoals2doc(list(np.subgoals)))],
            )
        case xtype.Tasks_PO_res_error_Unsat(arg=unsat):
            return _record(Pp.text('Unsat'), [_inline('err', _error2doc(unsat.err))])
        case xtype.Tasks_PO_res_error_Invalid_model(args=(err, model)):
            return _record(
                Pp.text('Invalid model'),
                [_inline('err', _error2doc(err)), _block('model', model2doc(model))],
            )
        case xtype.Tasks_PO_res_error_Error(arg=err):
            return _record(Pp.text('Error'), [_inline('err', _error2doc(err))])
    return Pp.text(repr(res))


# Sub-results
# ===========


def _sub_res2doc(sub: xtype.Tasks_PO_res_sub_res[xtype.Mir_Term]) -> Doc:
    fields: list[Doc] = [_inline('goal', _sequent2doc(sub.goal))]
    if sub.sub_goals:
        k = len(sub.sub_goals)
        fields.append(Pp.text(f'sub_goals: {k} subgoal{"s" if k > 1 else ""}'))
    head = Pp.text(f'- res: {sub.res}' if sub.res is not None else '-')
    return _record(head, fields)


# Top-level
# =========


def _verify_kind_name(vk: xtype.Common_Verify_kind | None) -> str | None:
    match vk:
        case xtype.Common_Verify_kind_K_verify():
            return 'verify'
        case xtype.Common_Verify_kind_K_instance():
            return 'instance'
        case xtype.Common_Verify_kind_K_test():
            return 'test'
        case None:
            return None


def po_task2doc(task: PO_task) -> Doc:
    """Render a `Tasks_PO_task_Mir`, eliding the serialized db and tactic tree."""
    po = task.po
    fields: list[Doc] = []
    if po.descr:
        fields.append(_inline('descr', Pp.text(po.descr)))
    kind = _verify_kind_name(po.verify_kind)
    if kind is not None:
        fields.append(_inline('kind', Pp.text(kind)))

    binders, goal_term = po.goal
    if binders:
        names = ', '.join(v.id.name for v in binders)
        fields.append(_inline('vars', Pp.text(names)))
    fields.append(_block('goal', Pp.group(Pp.hcat(Pp.text('⊢ '), term2doc(goal_term)))))

    if po.named_hypotheses:
        hyps = Pp.vsep(
            [
                Pp.hcat(Pp.text(f'{name}: '), term2doc(t))
                for name, t in po.named_hypotheses
            ]
        )
        fields.append(_block('hypotheses', hyps))

    if po.timeout is not None:
        fields.append(_inline('timeout', Pp.text(str(po.timeout))))
    if isinstance(po.upto, xtype.Upto_N_steps):
        fields.append(_inline('upto', Pp.text(f'{po.upto.arg} steps')))

    return _record(Pp.text(f'PO task: {task.from_sym}'), fields)


def po_res2doc(po_res: PO_res) -> Doc:
    """Render a `Tasks_PO_res_Shallow`, eliding the from_ pointer and report archive."""
    fields: list[Doc] = [_inline('res', _res2doc(po_res.res))]
    # NOTE: `stats.time_s` currently decodes to garbage from the shallow po_res
    # artifact (e.g. 2.01e+208) -- an upstream `Statistics_of_twine` bug. Disabled
    # until that is fixed; re-enable once the decoded value is trustworthy.
    if False:
        fields.append(_inline('time', Pp.text(f'{po_res.stats.time_s:.4g}s')))

    flat_sub = [s for group in po_res.sub_res for s in group]
    if flat_sub:
        fields.append(_block('sub_res', Pp.vsep([_sub_res2doc(s) for s in flat_sub])))

    return _record(Pp.text('PO res'), fields)


# String entry points
# ====================


def prettify_po_task(task: PO_task, width: int = 88) -> str:
    return Pp.pretty(width, po_task2doc(task))


def prettify_po_res(po_res: PO_res, width: int = 88) -> str:
    return Pp.pretty(width, po_res2doc(po_res))

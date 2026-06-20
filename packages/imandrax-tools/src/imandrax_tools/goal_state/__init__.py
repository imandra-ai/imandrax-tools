"""
Format the open proof goal state from a ``po_res`` artifact.
"""

from __future__ import annotations

import zipfile

import imandrax_api.lib as xtype

from imandrax_tools.pp.term_formatter import prettify as _pp_prettify

__all__ = [
    'GoalStateProved',
    'GoalStateCounterModel',
    'format_goal_state_from_bytes',
    'format_goal_state_from_zip',
]

type Sequent = xtype.Common_Sequent_t_poly[xtype.Mir_Term]
type PO_res = xtype.Tasks_PO_res_Shallow


class GoalStateProved(Exception):
    """Raised when the proof is complete (no open subgoals)."""


class GoalStateCounterModel(Exception):
    """Raised when a counter-model was found instead of open subgoals."""


# Formatting
# ==========


def _format_term(labeled: tuple[None | str, xtype.Mir_Term]) -> str:
    label, term = labeled
    pretty = _pp_prettify(term)
    return f'{label}: {pretty}' if label is not None else pretty


def _format_sequent(sg: Sequent) -> str:
    hyps = ', '.join(_format_term(h) for h in sg.hyps)
    concls = ', '.join(_format_term(c) for c in sg.concls)
    return f'⊢ {concls}' if not hyps else f'{hyps} ⊢ {concls}'


def _format_subgoals(ok: bool, subgoals: list[Sequent]) -> str:
    if not subgoals:
        return '∎\n' if ok else 'No subgoals reported after error.\n'
    k = len(subgoals)
    buf = f'{k} subgoal{"s" if k > 1 else ""} '
    for sg in subgoals:
        buf += f'-\n {_format_sequent(sg)}\n'
    return buf + '\n'


def format_goal_state(po_res: PO_res) -> str:
    """
    Format the open goal state from an already-decoded ``po_res``.

    Returns the formatted subgoal string when there are open goals. Raises
    :class:`GoalStateProved` if the proof is complete, :class:`GoalStateCounterModel`
    if a counter-model was found, and :class:`RuntimeError` for other errors.
    """
    res = po_res.res
    if isinstance(res, xtype.Tasks_PO_res_error_No_proof):
        no_proof = res.arg
        if no_proof.counter_model is not None:
            raise GoalStateCounterModel('counter-model found')
        return _format_subgoals(True, list(no_proof.subgoals))
    if isinstance(
        res,
        xtype.Tasks_PO_res_error_Error
        | xtype.Tasks_PO_res_error_Invalid_model
        | xtype.Tasks_PO_res_error_Unsat,
    ):
        raise RuntimeError('error in proof object')
    raise GoalStateProved('proof is complete')


# Deserialization helpers
# =======================


def _decode_po_res(twine_data: bytes) -> PO_res:
    decoder = xtype.twine.Decoder(twine_data)
    return xtype.Tasks_PO_res_Shallow_of_twine(d=decoder, off=decoder.entrypoint())


def format_goal_state_from_bytes(data: bytes) -> str:
    """Format goal state from raw ``data.twine`` bytes. See :func:`format_goal_state`."""
    return format_goal_state(_decode_po_res(data))


def format_goal_state_from_zip(path: str) -> str:
    """Format goal state from a ``po_res_art.zip`` path. See :func:`format_goal_state`."""
    with zipfile.ZipFile(path) as zf:
        data = zf.read('data.twine')
    return format_goal_state_from_bytes(data)

# ruff: noqa: F403, F405
from typing import Literal

import imandrax_api.lib as xtype

from . import pretty as Pp
from ._common import *
from .pretty import Doc
from .term_formatter import term2doc


def doc_of_labeled_term(labeled: tuple[None | str, xtype.Mir_Term]) -> Doc:
    label, term = labeled
    if label is not None:
        return Pp.text(f'{label}: {term2doc(term)}')
    else:
        return term2doc(term)


def doc_of_sequent(sg: Sequent) -> Doc:
    hyps = Pp.punctuate(Pp.text(','), [doc_of_labeled_term(h) for h in sg.hyps])
    concls = Pp.punctuate(Pp.text(','), [doc_of_labeled_term(c) for c in sg.concls])
    return Pp.hcat(hyps, Pp.text('⊢'), concls)


def doc_of_subgoals(ok: bool, subgoals: list[Sequent]) -> Doc:
    if not subgoals:
        return Pp.text('∎') if ok else Pp.text('No subgoals reported after error.')
    k = len(subgoals)
    title = Pp.text(f'{k} subgoal{"s" if k > 1 else ""}\n')
    subgoal_docs: list[Doc] = [
        Pp.concat(Pp.text('- '), doc_of_sequent(sg)) for sg in subgoals
    ]
    subgoals_doc = Pp.group(Pp.vsep(subgoal_docs))
    return Pp.hcat(title, subgoals_doc)


def subgoals_of_po_res(po_res: PO_res) -> list[Sequent] | None:
    res = po_res.res
    match res:
        case xtype.Tasks_PO_res_error_No_proof():
            no_proof = res.arg
            if no_proof.counter_model is None:
                return no_proof.subgoals
            else:
                return None
        case _:
            return None


type Either[A, B] = tuple[Literal['left'], A] | tuple[Literal['right'], B]


def left[A, B](x: A) -> Either[A, B]:
    return ('left', x)


def right[A, B](x: B) -> Either[A, B]:
    return ('right', x)


def goal_state_doc_of_po_res(
    po_res: PO_res,
) -> Either[Doc, str]:
    res = po_res.res
    match res:
        case xtype.Tasks_PO_res_error_No_proof():
            no_proof = res.arg
            if no_proof.counter_model is not None:
                return right('Counter-model found')
            else:
                return left(doc_of_subgoals(True, list(no_proof.subgoals)))
        # TODO: implement doc_of_error and properly print the error
        case xtype.Tasks_PO_res_error_Error():
            return right('Error in proof object')
        case xtype.Tasks_PO_res_error_Invalid_model():
            return right('Invalid model')
        case xtype.Tasks_PO_res_error_Unsat():
            return right('Unsat')
        # TODO: properly print success cases
        case xtype.Tasks_PO_res_success_Instance():
            return right('Instance found')
        case xtype.Tasks_PO_res_success_Verified_upto():
            return right('Verified up to')
        case xtype.Tasks_PO_res_success_Proof():
            return right('Proof found')
        case xtype.Tasks_PO_res_success_Test_ok():
            return right('Test ok')


# Serde helper
# ====================


def po_res_of_art_zip(art_zip: bytes) -> xtype.Tasks_PO_res_Shallow:
    from tempfile import NamedTemporaryFile

    with NamedTemporaryFile() as f:
        f.write(art_zip)
        f.seek(0)
        art: xtype.Artifact = xtype.read_artifact_zip(f.name)
    assert isinstance(art, xtype.Tasks_PO_res_shallow_poly), (
        'ZIP artifact is not a PO res shallow'
    )
    return art


def show_goal_state_zip(art_zip: bytes) -> Either[str, str]:
    match goal_state_doc_of_po_res(po_res_of_art_zip(art_zip)):
        case ('left', doc):
            return left(Pp.pretty(88, doc))
        case r:
            return r

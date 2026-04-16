from inline_snapshot import snapshot

from minirec.common import DecompAsmSigMismatchDiag, InfixOpMissingParenDiag, Loc, NoLoc
from minirec.db import (
    EVAL_RES_DECOMP_ASSUMING_SIG_MISMATCH,
    EVAL_RES_INFIX_OP_MISSING_PAREN,
    IML_INFIX_OP_MISSING_PAREN,
)
from minirec.err_rec import check_decomp_asm_mismatch, check_infix_op_missing_paren


def test_infix_op_missing_paren():
    diag_opt = check_infix_op_missing_paren(
        IML_INFIX_OP_MISSING_PAREN, EVAL_RES_INFIX_OP_MISSING_PAREN
    )
    assert diag_opt is not None
    assert diag_opt == snapshot(
        InfixOpMissingParenDiag(
            loc=Loc(start_byte=4, end_byte=10, start_point=(0, 4), end_point=(0, 10)),
            op='land :',
        )
    )


def test_decomp_asm_mismatch():
    diag_opt = check_decomp_asm_mismatch(EVAL_RES_DECOMP_ASSUMING_SIG_MISMATCH)
    assert diag_opt is not None
    assert diag_opt == snapshot(DecompAsmSigMismatchDiag(loc=NoLoc(reason='WIP')))

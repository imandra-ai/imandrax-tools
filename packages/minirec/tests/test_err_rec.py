from inline_snapshot import snapshot

from minirec.common import InfixOpMissingParenDiag, Loc
from minirec.db import EVAL_RES_INFIX_OP_MISSING_PAREN, IML_INFIX_OP_MISSING_PAREN
from minirec.err_rec import check_infix_op_missing_paren


def test():
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

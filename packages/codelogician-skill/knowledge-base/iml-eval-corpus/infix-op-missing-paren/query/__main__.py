# %%

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import ClassVar, Self

from imandrax_api_models import ErrorKind, EvalRes
from iml_query.queries import BaseCapture
from iml_query.tree_sitter_utils import run_query, unwrap_bytes
from minirec.common import BaseDiag, BaseRule, Severity
from minirec.cst import range_to_loc
from pydantic import computed_field
from tree_sitter import Node

CURR_DIR = Path(__file__).parent

# %%
# Query
# ====================


@dataclass(slots=True, frozen=True)
class InfixOpMissingParenCapture(BaseCapture):
    QUERY: ClassVar[str] = """\
(value_definition
    "let"
    (ERROR
    ["land" "lor" "lxor" "lsl" "lsr" "asr" "mod"]) @op
    (let_binding))"""

    op: Node

    @classmethod
    def from_code(cls, code: str) -> Self | None:
        captures = run_query(query=cls.QUERY, code=code)
        if not captures:
            return None
        return cls.from_ts_capture(captures[0][1])

    def get_op(self) -> str:
        return unwrap_bytes(self.op.text).decode("utf8")


# %%
# Diagnostic
# ====================


class InfixOpMissingParenRule(BaseRule):
    id: ClassVar[str] = "infix-op-missing-paren"
    description: ClassVar[str] = "Infix operator missing parenthesis"
    severity: ClassVar[Severity] = Severity.Error


INFIX_OP_MISSING_PAREN_RULE = InfixOpMissingParenRule()


class InfixOpMissingParenDiag(BaseDiag):
    rule: ClassVar[InfixOpMissingParenRule] = INFIX_OP_MISSING_PAREN_RULE  # pyright: ignore[reportIncompatibleVariableOverride]
    error_kind: ClassVar[ErrorKind] = ErrorKind.SYNTAX_ERR

    op: str

    @computed_field
    @property
    def message(self) -> str:
        return (
            f"`{self.op}` is an infix operator. When used in let-binding, it "
            f"needs to be enclosed in parentheses.\n"
            "E.g. `let ( land ) = <new-definition>`"
        )


# %%
# Check
# ====================


def check_infix_op_missing_paren(
    iml: str, eval_res: EvalRes
) -> InfixOpMissingParenDiag | None:
    """Match the `infix-op-missing-paren` pattern.

    Conditions:
    - eval_res reports a syntax error
    - the tree-sitter parse exposes an ERROR node wrapping an infix-op keyword
      (`land`/`lor`/`lxor`/`lsl`/`lsr`/`asr`/`mod`) inside a `let` binding.
    """
    first_err = eval_res.errors[0] if eval_res.errors else None
    if first_err is None:
        return None
    if ErrorKind.from_proto_kind(first_err.kind) != ErrorKind.SYNTAX_ERR:
        return None

    capture = InfixOpMissingParenCapture.from_code(iml)
    if capture is None:
        return None

    return InfixOpMissingParenDiag(
        op=capture.get_op(),
        loc=range_to_loc(capture.op.range),
    )


# %%
if __name__ == "__main__":
    from iml_eval_corpus.cli import main_from_check

    main_from_check(check_infix_op_missing_paren)

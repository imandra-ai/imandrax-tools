# %%

from __future__ import annotations

from pathlib import Path
from typing import ClassVar

from imandrax_api_models import ErrorKind, EvalRes, TaskKind
from iml_eval_corpus.common import BaseDiag, BaseRule, NoLoc, Severity
from pydantic import computed_field

CURR_DIR = Path(__file__).parent

# %%
# Diagnostic
# ====================


class DecompAsmSigMismatchRule(BaseRule):
    id: ClassVar[str] = "decomp-asm-sig-mismatch"
    description: ClassVar[str] = "Region decomp assuming signature mismatch"
    severity: ClassVar[Severity] = Severity.Error


DECOMP_ASM_SIG_MISMATCH_RULE = DecompAsmSigMismatchRule()


class DecompAsmSigMismatchDiag(BaseDiag):
    rule: ClassVar[DecompAsmSigMismatchRule] = DECOMP_ASM_SIG_MISMATCH_RULE  # pyright: ignore[reportIncompatibleVariableOverride]
    error_kind: ClassVar[ErrorKind] = ErrorKind.TACTIC_EVAL_ERR

    # TODO: current error messages do not expose which decomp / ~assuming
    #   pair caused the mismatch, so we omit function_name / assuming fields.

    @computed_field
    @property
    def message(self) -> str:
        return (
            "Invalid decomp arguments: `~assuming` has inconsistent type "
            "signature with the function being decomposed."
        )


# %%
# Check
# ====================


def check_decomp_asm_mismatch(
    iml: str, eval_res: EvalRes
) -> DecompAsmSigMismatchDiag | None:
    """Match the `decomp-asm-sig-mismatch` pattern.

    Conditions:
    - eval succeeded with no top-level errors
    - a TacticEvalErr message mentioning `Inject_asm` is present
    - the evaluated source had a Decomp task
    - no proof-obligation results were produced
    """
    del iml

    if eval_res.has_errors or not eval_res.success:
        return None

    has_tactic_eval_err_with_inject_asm = False
    for msg in eval_res.messages:
        if (
            ErrorKind.from_proto_kind(msg) == ErrorKind.TACTIC_EVAL_ERR
            and "Inject_asm" in msg
        ):
            has_tactic_eval_err_with_inject_asm = True
            break
    if not has_tactic_eval_err_with_inject_asm:
        return None

    has_decomp_task = any(t.kind == TaskKind.TASK_DECOMP for t in eval_res.tasks)
    no_po_result = len(eval_res.po_results) == 0
    if not (has_decomp_task and no_po_result):
        return None

    return DecompAsmSigMismatchDiag(loc=NoLoc(reason="WIP"))


# %%
# Query-module Protocol aliases (see iml_eval_corpus.query_protocol)
# ====================

RULE = DECOMP_ASM_SIG_MISMATCH_RULE
Diag = DecompAsmSigMismatchDiag
check = check_decomp_asm_mismatch


# %%
if __name__ == "__main__":
    from iml_eval_corpus.cli import main_from_check

    main_from_check(check_decomp_asm_mismatch)

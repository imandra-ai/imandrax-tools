# %%

from dataclasses import dataclass
from pathlib import Path
from typing import ClassVar, Self

from imandrax_api_models import ErrorKind, EvalRes
from imandrax_tools.iml_eval_corpus.common import BaseDiag, BaseRule, Severity
from imandrax_tools.iml_eval_corpus.cst import range_to_loc
from iml_query.queries import MEASURE_QUERY_SRC, BaseCapture, MeasureCapture
from iml_query.tree_sitter_utils import run_query, unwrap_bytes
from pydantic import computed_field
from tree_sitter import Node

CURR_DIR = Path(__file__).parent

# %%
# Query
# ====================

TS_Q = (CURR_DIR / "ts.scm").read_text()


@dataclass(slots=True, frozen=True)
class RecFunCapture(BaseCapture):
    QUERY: ClassVar[str] = TS_Q

    function_definition: Node
    function_name: Node

    @classmethod
    def from_code(cls, code: str) -> list[Self]:
        captures = run_query(query=cls.QUERY, code=code)
        return [cls.from_ts_capture(c) for _, c in captures]


# %%
# Diagnostic
# ====================


class GeneralTerminationProofErrorRule(BaseRule):
    id: ClassVar[str] = "general-termination-proof-error"
    description: ClassVar[str] = (
        "Recursive function's default termination measure is counter-satisfiable"
    )
    severity: ClassVar[Severity] = Severity.Error


GENERAL_TERMINATION_PROOF_ERROR_RULE = GeneralTerminationProofErrorRule()


class GeneralTerminationProofErrorDiag(BaseDiag):
    rule: ClassVar[GeneralTerminationProofErrorRule] = (  # pyright: ignore[reportIncompatibleVariableOverride]
        GENERAL_TERMINATION_PROOF_ERROR_RULE
    )
    error_kind: ClassVar[ErrorKind] = ErrorKind.TACTIC_EVAL_ERR

    function_name: str

    @computed_field
    @property
    def message(self) -> str:
        return (
            f"The default termination measure for recursive function "
            f"`{self.function_name}` is counter-satisfiable. "
            "Supply a custom `[@@measure ...]` returning `Ordinal.t` that "
            "strictly decreases on every recursive call."
        )


# %%
# Check
# ====================


# Signatures in PO messages that indicate a *termination* goal (vs. other POs).
_TERMINATION_SIGS = ("Ordinal.is_valid", "Ordinal.(<<)", "Ordinal.Int")


def _po_has_termination_counter_sat(eval_res: EvalRes) -> bool:
    tactic_evals = [
        e
        for e in eval_res.po_errors
        if ErrorKind.from_proto_kind(e.kind) == ErrorKind.TACTIC_EVAL_ERR
        and e.msg is not None
        and "counter-satisfiable" in e.msg.msg.lower()
    ]
    if not tactic_evals:
        return False
    # Make sure this is a termination PO, not some other counter-satisfiable goal.
    combined = "\n".join(eval_res.messages)
    return any(sig in combined for sig in _TERMINATION_SIGS)


def check_general_termination_proof_error(
    iml: str, eval_res: EvalRes
) -> GeneralTerminationProofErrorDiag | None:
    """Match the `general-termination-proof-error` pattern.

    Conditions:
    - a PO error of kind TacticEvalErr saying the goal is counter-satisfiable
    - the PO goal is a termination goal (mentions `Ordinal.is_valid`/`Ordinal.(<<)`)
    - the source contains a top-level `let rec` without a `[@@measure ...]` attribute
    """
    if not _po_has_termination_counter_sat(eval_res):
        return None

    rec_caps = RecFunCapture.from_code(iml)
    if not rec_caps:
        return None

    measure_caps = [
        MeasureCapture.from_ts_capture(c)
        for _, c in run_query(query=MEASURE_QUERY_SRC, code=iml)
    ]
    measured_names: set[str] = {
        unwrap_bytes(m.function_name.text).decode("utf8") for m in measure_caps
    }

    for cap in rec_caps:
        name = unwrap_bytes(cap.function_name.text).decode("utf8")
        if name in measured_names:
            continue
        return GeneralTerminationProofErrorDiag(
            loc=range_to_loc(cap.function_definition.range),
            function_name=name,
        )
    return None


# %%
# Query-module Protocol aliases (see iml_eval_corpus.query_protocol)
# ====================

RULE = GENERAL_TERMINATION_PROOF_ERROR_RULE
Diag = GeneralTerminationProofErrorDiag
check = check_general_termination_proof_error

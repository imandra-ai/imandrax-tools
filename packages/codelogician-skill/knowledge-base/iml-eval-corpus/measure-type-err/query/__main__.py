# %%

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

from iml_eval_corpus.diag_utils import loc_within_range

CURR_DIR = Path(__file__).parent

# %%
# Query
# ====================

TS_Q = (CURR_DIR / "ts.scm").read_text()


@dataclass(slots=True, frozen=True)
class MeasureAttrCapture(BaseCapture):
    QUERY: ClassVar[str] = TS_Q

    function_definition: Node
    function_name: Node
    measure_attr: Node
    measure_payload: Node

    @classmethod
    def from_code(cls, code: str) -> list[Self]:
        captures = run_query(query=cls.QUERY, code=code)
        return [cls.from_ts_capture(c) for _, c in captures]


# %%
# Diagnostic
# ====================


class MeasureTypeErrRule(BaseRule):
    id: ClassVar[str] = "measure-type-err"
    description: ClassVar[str] = "Measure function does not return `Ordinal.t`"
    severity: ClassVar[Severity] = Severity.Error


MEASURE_TYPE_ERR_RULE = MeasureTypeErrRule()


class MeasureTypeErrDiag(BaseDiag):
    rule: ClassVar[MeasureTypeErrRule] = MEASURE_TYPE_ERR_RULE  # pyright: ignore[reportIncompatibleVariableOverride]
    error_kind: ClassVar[ErrorKind] = ErrorKind.TYPE_ERR

    function_name: str
    measure: str  # the measure expression, e.g. `m n`

    @computed_field
    @property
    def message(self) -> str:
        return (
            f"The measure `{self.measure}` attached to `{self.function_name}` "
            "must return `Ordinal.t`. "
            "Wrap the measure with `Ordinal.of_int` (or similar) to lift it."
        )


# %%
# Check
# ====================


def check_measure_type_err(iml: str, eval_res: EvalRes) -> MeasureTypeErrDiag | None:
    """Match the `measure-type-err` pattern.

    Conditions:
    - at least one error with kind TypeErr
    - a `[@@measure ...]` attribute exists in the source
    - the type error's locations lie within the measure attribute's range
    """
    type_errs = [
        e
        for e in eval_res.errors
        if ErrorKind.from_proto_kind(e.kind) == ErrorKind.TYPE_ERR
    ]
    if not type_errs:
        return None

    captures = MeasureAttrCapture.from_code(iml)
    if not captures:
        return None

    for cap in captures:
        for err in type_errs:
            if err.msg is None or not err.msg.locs:
                continue
            if all(loc_within_range(loc, cap.measure_attr) for loc in err.msg.locs):
                return MeasureTypeErrDiag(
                    loc=range_to_loc(cap.measure_attr.range),
                    function_name=unwrap_bytes(cap.function_name.text).decode("utf8"),
                    measure=unwrap_bytes(cap.measure_payload.text).decode("utf8"),
                )
    return None


# %%
if __name__ == "__main__":
    from iml_eval_corpus.cli import main_from_check

    main_from_check(check_measure_type_err)

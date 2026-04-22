# %%

import re
from dataclasses import dataclass
from pathlib import Path
from typing import ClassVar, Self

from imandrax_api_models import ErrorKind, EvalRes
from iml_query.queries import BaseCapture
from iml_query.tree_sitter_utils import run_query, unwrap_bytes
from imandrax_tools.iml_eval_corpus.common import BaseDiag, BaseRule, Severity
from imandrax_tools.iml_eval_corpus.cst import range_to_loc
from pydantic import computed_field
from tree_sitter import Node

from imandrax_tools.iml_eval_corpus.diag_utils import loc_within_range

CURR_DIR = Path(__file__).parent

# %%
# Query
# ====================

TS_Q = (CURR_DIR / "ts.scm").read_text()


@dataclass(slots=True, frozen=True)
class ValueDefCapture(BaseCapture):
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


class FunctionInCompositeTypeRule(BaseRule):
    id: ClassVar[str] = "function-in-composite-type"
    description: ClassVar[str] = (
        "Function value stored inside a composite type (list/tuple/record/variant)"
    )
    severity: ClassVar[Severity] = Severity.Error


FUNCTION_IN_COMPOSITE_TYPE_RULE = FunctionInCompositeTypeRule()

# Message pattern emitted by ImandraX:
#   "Functions must return a first-order type,\nbut `<name>` returns `<ty>`"
_MSG_RE = re.compile(
    r"Functions must return a first-order type,\s*but `(?P<name>[^`]+)` returns `(?P<ty>[^`]+)`"
)


class FunctionInCompositeTypeDiag(BaseDiag):
    rule: ClassVar[FunctionInCompositeTypeRule] = FUNCTION_IN_COMPOSITE_TYPE_RULE  # pyright: ignore[reportIncompatibleVariableOverride]
    error_kind: ClassVar[ErrorKind] = ErrorKind.VALIDATION_ERROR

    function_name: str
    returned_type: str

    @computed_field
    @property
    def message(self) -> str:
        return (
            f"Function `{self.function_name}` returns `{self.returned_type}`, "
            "which stores a function value inside a composite type. "
            "IML requires functions to be first-order: a function value cannot "
            "be placed inside a list, tuple, record, or variant."
        )


# %%
# Check
# ====================


def check_function_in_composite_type(
    iml: str, eval_res: EvalRes
) -> FunctionInCompositeTypeDiag | None:
    """Match the `function-in-composite-type` pattern.

    Conditions:
    - at least one ValidationError whose message matches the
      "Functions must return a first-order type" pattern
    - a value_definition in the source whose range contains the error's locs
    """
    val_errs = [
        e
        for e in eval_res.errors
        if ErrorKind.from_proto_kind(e.kind) == ErrorKind.VALIDATION_ERROR
    ]
    if not val_errs:
        return None

    captures = ValueDefCapture.from_code(iml)
    if not captures:
        return None

    for err in val_errs:
        if err.msg is None:
            continue
        m = _MSG_RE.search(err.msg.msg)
        if m is None:
            continue
        name = m.group("name")
        ty = m.group("ty")
        for cap in captures:
            cap_name = unwrap_bytes(cap.function_name.text).decode("utf8")
            if cap_name != name:
                continue
            if err.msg.locs and not all(
                loc_within_range(loc, cap.function_definition) for loc in err.msg.locs
            ):
                continue
            return FunctionInCompositeTypeDiag(
                loc=range_to_loc(cap.function_definition.range),
                function_name=name,
                returned_type=ty,
            )
    return None


# %%
# Query-module Protocol aliases (see iml_eval_corpus.query_protocol)
# ====================

RULE = FUNCTION_IN_COMPOSITE_TYPE_RULE
Diag = FunctionInCompositeTypeDiag
check = check_function_in_composite_type


# %%
if __name__ == "__main__":
    from imandrax_tools.iml_eval_corpus.cli import main_from_check

    main_from_check(check_function_in_composite_type)

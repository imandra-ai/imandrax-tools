# %%

from __future__ import annotations

from pathlib import Path
from typing import ClassVar

from imandrax_api_models import EvalRes
from iml_query.queries import (
    MEASURE_QUERY_SRC,
    VALUE_DEFINITION_QUERY_SRC,
    MeasureCapture,
    ValueDefCapture,
)
from iml_query.tree_sitter_utils import (
    get_nesting_relationship,
    get_parser,
    run_queries,
    unwrap_bytes,
)
from imandrax_tools.iml_eval_corpus.common import BaseDiag, BaseRule, Severity
from imandrax_tools.iml_eval_corpus.cst import range_to_loc
from pydantic import computed_field

CURR_DIR = Path(__file__).parent

# %%
# Diagnostic
# ====================


class NestedMeasureRule(BaseRule):
    id: ClassVar[str] = "nested-measure-attribute"
    description: ClassVar[str] = "Nested measure attribute"
    severity: ClassVar[Severity] = Severity.Warning


NESTED_MEASURE_RULE = NestedMeasureRule()


class NestedMeasureDiag(BaseDiag):
    rule: ClassVar[NestedMeasureRule] = NESTED_MEASURE_RULE  # pyright: ignore[reportIncompatibleVariableOverride]

    function_name: str
    measure: str
    top_function_name: str
    nesting_level: int

    @computed_field
    @property
    def message(self) -> str:
        return (
            f"Measure attribute `{self.measure}` should be attached to a "
            f"top-level function instead of nested function `{self.function_name}`."
        )


# %%
# Check
# ====================


def check_nested_measure(iml: str, eval_res: EvalRes) -> NestedMeasureDiag | None:
    """Match the `nested-measure-attribute` pattern.

    Conditions:
    - a `[@@measure ...]` attribute is attached to a function whose definition
      is lexically nested inside another top-level function.
    """
    del eval_res  # rule-based; eval output not consulted

    tree = get_parser().parse(bytes(iml, "utf-8"))
    captures_map = run_queries(
        {
            "value_def": VALUE_DEFINITION_QUERY_SRC,
            "measure": MEASURE_QUERY_SRC,
        },
        node=tree.root_node,
    )
    value_def_captures = [
        ValueDefCapture.from_ts_capture(c) for c in captures_map.get("value_def", [])
    ]
    measure_captures = [
        MeasureCapture.from_ts_capture(c) for c in captures_map.get("measure", [])
    ]

    top_captures = [c for c in value_def_captures if c.is_top_level]
    top_level_names = {
        unwrap_bytes(c.function_name.text).decode("utf-8") for c in top_captures
    }

    for cap in measure_captures:
        func_name = unwrap_bytes(cap.function_name.text).decode("utf-8")
        if func_name in top_level_names:
            continue

        measure_def_node = cap.function_definition
        for top in top_captures:
            nesting_level = get_nesting_relationship(
                measure_def_node, top.function_definition
            )
            if nesting_level > 0:
                return NestedMeasureDiag(
                    loc=range_to_loc(measure_def_node.range),
                    function_name=func_name,
                    measure=unwrap_bytes(cap.measure_attr.text).decode("utf-8"),
                    top_function_name=unwrap_bytes(top.function_name.text).decode(
                        "utf-8"
                    ),
                    nesting_level=nesting_level,
                )
    return None


# %%
# Query-module Protocol aliases (see iml_eval_corpus.query_protocol)
# ====================

RULE = NESTED_MEASURE_RULE
Diag = NestedMeasureDiag
check = check_nested_measure


# %%
if __name__ == "__main__":
    from imandrax_tools.iml_eval_corpus.cli import main_from_check

    main_from_check(check_nested_measure)

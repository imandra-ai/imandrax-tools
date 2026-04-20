# %%

from __future__ import annotations

from pathlib import Path
from typing import ClassVar

from imandrax_api_models import EvalRes
from iml_query.processing.base import resolve_nesting_definitions
from iml_query.queries import (
    VALUE_DEFINITION_QUERY_SRC,
    ValueDefCapture,
)
from iml_query.tree_sitter_utils import (
    get_parser,
    run_queries,
    unwrap_bytes,
)
from minirec.common import BaseDiag, BaseRule, Severity
from minirec.cst import range_to_loc
from pydantic import computed_field

CURR_DIR = Path(__file__).parent

# %%
# Diagnostic
# ====================


class NestedRecursiveFunctionRule(BaseRule):
    id: ClassVar[str] = "nested-recursive-function"
    description: ClassVar[str] = "Nested recursive function"
    severity: ClassVar[Severity] = Severity.Warning


NESTED_RECURSIVE_FUNCTION_RULE = NestedRecursiveFunctionRule()


class NestedRecursiveFunctionDiag(BaseDiag):
    rule: ClassVar[NestedRecursiveFunctionRule] = NESTED_RECURSIVE_FUNCTION_RULE  # pyright: ignore[reportIncompatibleVariableOverride]

    function_name: str
    top_function_name: str
    nesting_level: int

    @computed_field
    @property
    def message(self) -> str:
        return (
            f"Recursive function `{self.function_name}` nested in "
            f"`{self.top_function_name}` might cause proof-obligation difficulty."
        )


# %%
# Check
# ====================


def check_nested_rec(
    iml: str, eval_res: EvalRes
) -> NestedRecursiveFunctionDiag | None:
    """Match the `nested-recursive-function` pattern.

    Conditions:
    - a `let rec` binding is lexically nested inside another top-level
      value definition.
    """
    del eval_res  # rule-based; eval output not consulted

    tree = get_parser().parse(bytes(iml, "utf-8"))
    captures_map = run_queries(
        {"value_def": VALUE_DEFINITION_QUERY_SRC},
        node=tree.root_node,
    )
    value_def_captures = [
        ValueDefCapture.from_ts_capture(c) for c in captures_map.get("value_def", [])
    ]

    for nesting in resolve_nesting_definitions(value_def_captures):
        child = nesting["child"]
        if not child.is_rec:
            continue
        return NestedRecursiveFunctionDiag(
            loc=range_to_loc(child.function_definition.range),
            function_name=unwrap_bytes(child.function_name.text).decode("utf-8"),
            top_function_name=unwrap_bytes(
                nesting["parent"].function_name.text
            ).decode("utf-8"),
            nesting_level=nesting["nesting_level"],
        )
    return None


# %%
if __name__ == "__main__":
    from iml_eval_corpus.cli import main_from_check

    main_from_check(check_nested_rec)

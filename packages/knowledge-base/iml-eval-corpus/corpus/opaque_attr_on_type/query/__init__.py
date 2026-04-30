# %%

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import ClassVar, Self

from imandrax_api_models import ErrorKind, EvalRes
from imandrax_tools.iml_eval_corpus.common import BaseDiag, BaseRule, Severity
from imandrax_tools.iml_eval_corpus.cst import range_to_loc
from iml_query.queries import BaseCapture
from iml_query.tree_sitter_utils import run_query, unwrap_bytes
from pydantic import computed_field
from tree_sitter import Node

CURR_DIR = Path(__file__).parent

# %%
# Query
# ====================


@dataclass(slots=True, frozen=True)
class OpaqueAttrOnTypeCapture(BaseCapture):
    QUERY: ClassVar[str] = """\
(type_definition
    (type_binding
        (type_constructor) @type_name
        (item_attribute
            (attribute_id) @_attr_id
            (#eq? @_attr_id "opaque")) @opaque_attr)) @type_definition"""

    type_definition: Node
    type_name: Node
    opaque_attr: Node

    @classmethod
    def from_code(cls, code: str) -> Self | None:
        captures = run_query(query=cls.QUERY, code=code)
        if not captures:
            return None
        return cls.from_ts_capture(captures[0][1])

    def get_type_name(self) -> str:
        return unwrap_bytes(self.type_name.text).decode("utf8")


# %%
# Diagnostic
# ====================


class OpaqueAttrOnTypeRule(BaseRule):
    id: ClassVar[str] = "opaque-attr-on-type"
    description: ClassVar[str] = "`[@@opaque]` attribute attached to a type declaration"
    severity: ClassVar[Severity] = Severity.Warning


OPAQUE_ATTR_ON_TYPE_RULE = OpaqueAttrOnTypeRule()


class OpaqueAttrOnTypeDiag(BaseDiag):
    rule: ClassVar[OpaqueAttrOnTypeRule] = OPAQUE_ATTR_ON_TYPE_RULE  # pyright: ignore[reportIncompatibleVariableOverride]
    error_kind: ClassVar[ErrorKind] = ErrorKind.GENERIC_INTERNAL_ERROR

    type_name: str

    @computed_field
    @property
    def message(self) -> str:
        return (
            f"`[@@opaque]` cannot be attached to type `{self.type_name}`. "
            "The attribute is only valid on value bindings (e.g. "
            "`let f : t -> t = () [@@opaque]`)."
        )


# %%
# Check
# ====================


def check_opaque_attr_on_type(
    iml: str, eval_res: EvalRes
) -> OpaqueAttrOnTypeDiag | None:
    """Match the `opaque-attr-on-type` pattern.

    Conditions:
    - the source contains a `type` declaration with an `[@@opaque]` item
      attribute attached to its binding.

    eval output is not consulted.
    """
    del eval_res

    capture = OpaqueAttrOnTypeCapture.from_code(iml)
    if capture is None:
        return None

    return OpaqueAttrOnTypeDiag(
        type_name=capture.get_type_name(),
        loc=range_to_loc(capture.type_definition.range),
    )


# %%
# Query-module Protocol aliases (see iml_eval_corpus.query_protocol)
# ====================

RULE = OPAQUE_ATTR_ON_TYPE_RULE
Diag = OpaqueAttrOnTypeDiag
check = check_opaque_attr_on_type

# pyright: reportIncompatibleVariableOverride=false
from __future__ import annotations

from abc import abstractmethod
from enum import Enum
from typing import ClassVar, Protocol

from imandrax_api_models import ErrorKind, EvalRes
from pydantic import BaseModel, computed_field


class Loc(BaseModel):
    start_byte: int
    end_byte: int
    start_point: tuple[int, int]
    end_point: tuple[int, int]


class Severity(Enum):
    # in the order of decreasing severity:
    Error = 1  # must fix, blocks compilation
    Warning = 2  # likely wrong, but not always
    Info = 3  # informational, no action required
    Hint = 4  # stylistic suggestion


# Rule
# ====================


class BaseRule(BaseModel):
    id: ClassVar[str]
    description: ClassVar[str]
    severity: ClassVar[Severity]


class NestedMeasureRule(BaseRule):
    id: ClassVar[str] = 'nested-measure-attribute'
    description: ClassVar[str] = 'Nested measure attribute'
    severity: ClassVar[Severity] = Severity.Warning


class NestedRecursiveFunctionRule(BaseRule):
    id: ClassVar[str] = 'nested-recursive-function'
    description: ClassVar[str] = 'Nested recursive function'
    severity: ClassVar[Severity] = Severity.Warning


class InfixOpMissingParen(BaseRule):
    id: ClassVar[str] = 'infix-op-missing-paren'
    description: ClassVar[str] = 'Infix operator missing parenthesis'
    severity: ClassVar[Severity] = Severity.Error


Rule = NestedMeasureRule | NestedRecursiveFunctionRule | InfixOpMissingParen

NESTED_MEASURE_RULE = NestedMeasureRule()
NESTED_RECURSIVE_FUNCTION_RULE = NestedRecursiveFunctionRule()
INFIX_OP_MISSING_PAREN_RULE = InfixOpMissingParen()


# Diagnostics
# ====================


class BaseDiag(BaseModel):
    rule: Rule
    loc: Loc

    @computed_field
    @property
    @abstractmethod
    def message(self) -> str:
        pass

    def format_error_message(self) -> str:
        s = ''
        s += f'{self.rule.id}: {self.message}\n'
        s += f'location: {self.loc.start_point[0]}:{self.loc.start_point[1]}\n'
        return s


class NestedMeasureDiag(BaseDiag):
    rule: ClassVar[Rule] = NESTED_MEASURE_RULE

    function_name: str
    measure: str
    top_function_name: str
    nesting_level: int

    @computed_field
    @property
    def message(self) -> str:
        return (
            f'Measure attribute `{self.measure}` should be attached to a '
            f'top-level function instead of nested function `{self.function_name}`.'
        )


class NestedRecursiveFunctionDiag(BaseDiag):
    rule: ClassVar[Rule] = NESTED_RECURSIVE_FUNCTION_RULE

    function_name: str
    top_function_name: str
    nesting_level: int

    @computed_field
    @property
    def message(self) -> str:
        return (
            f'Recursive function `{self.function_name}` nested in '
            f'`{self.top_function_name}` might cause proof-obligation difficulty.'
        )


class InfixOpMissingParenDiag(BaseDiag):
    rule: ClassVar[Rule] = INFIX_OP_MISSING_PAREN_RULE
    error_kind: ClassVar[ErrorKind] = ErrorKind.SYNTAX_ERR

    op: str  # The infix operator that is missing parenthesis

    @computed_field
    @property
    def message(self) -> str:
        return (
            f'`{self.op}` is an infix operator. When used in let-binding, it '
            f'needs to be enclosed in parentheses.\n'
            'E.g. `let ( land ) = <new-definition>`'
        )


class RuleCheck(Protocol):
    rule: Rule

    def __call__(self, iml: str | None, eval_res: EvalRes | None) -> list[BaseDiag]: ...

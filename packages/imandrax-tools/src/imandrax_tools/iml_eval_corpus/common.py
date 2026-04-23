# pyright: reportIncompatibleVariableOverride=false
from __future__ import annotations

from abc import abstractmethod
from dataclasses import dataclass
from enum import Enum
from typing import ClassVar, Protocol

from imandrax_api_models import EvalRes
from pydantic import BaseModel, computed_field


class Loc(BaseModel):
    start_byte: int
    end_byte: int
    start_point: tuple[
        int, int
    ]  # (line, col), 1-indexed — matches imandrax-api's `Position` convention.
    # Note tree-sitter `Range.start_point`/`end_point` are 0-indexed; convert when
    # constructing a `Loc` from tree-sitter.
    end_point: tuple[int, int]


class Severity(Enum):
    # in the order of decreasing severity:
    Error = 1  # must fix, blocks compilation
    Warning = 2  # likely wrong, but not always
    Info = 3  # informational, no action required
    Hint = 4  # stylistic suggestion


class BaseRule(BaseModel):
    id: ClassVar[str]
    description: ClassVar[str]
    severity: ClassVar[Severity]


# Diagnostics
# ====================


@dataclass(slots=True, frozen=True)
class NoLoc:
    reason: str


class BaseDiag(BaseModel):
    rule: BaseRule
    loc: Loc | NoLoc

    @computed_field
    @property
    @abstractmethod
    def message(self) -> str:
        pass

    def format_error_message(self) -> str:
        s = ''
        s += f'{self.rule.id}: {self.message}\n'
        if isinstance(self.loc, Loc):
            s += f'location: {self.loc.start_point[0]}:{self.loc.start_point[1]}\n'
        return s


class RuleCheck(Protocol):
    rule: BaseRule

    def __call__(self, iml: str | None, eval_res: EvalRes | None) -> list[BaseDiag]: ...

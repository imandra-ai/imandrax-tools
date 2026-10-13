# pyright: reportIncompatibleVariableOverride=false
from __future__ import annotations

from abc import abstractmethod
from collections.abc import Mapping
from dataclasses import dataclass
from enum import Enum
from typing import ClassVar, Protocol

from imandrax_api_models import EvalRes
from imandrax_api_models.context_utils import format_code_snippet_with_loc
from pydantic import BaseModel, Field, computed_field


class Loc(BaseModel):
    start_byte: int
    end_byte: int
    start_point: tuple[int, int]
    """1-indexed (line, col) ImandraX position. """
    end_point: tuple[int, int]
    file: str | None = Field(default=None, exclude_if=lambda v: v is None)
    """File the points are in; `None` for the code the diagnostic was checked on."""

    # Note: tree-sitter `Range.start_point`/`end_point` are 0-indexed

    @classmethod
    def make(
        cls,
        src: str,
        start_point: tuple[int, int],
        end_point: tuple[int, int],
        file: str | None = None,
    ) -> Loc:
        """Make. Infers start and end bytes"""
        lines = src.encode('utf-8').split(b'\n')

        def byte(line: int, col: int) -> int:
            return sum(len(ln) + 1 for ln in lines[: line - 1]) + col - 1

        return cls(
            start_byte=byte(*start_point),
            end_byte=byte(*end_point),
            start_point=start_point,
            end_point=end_point,
            file=file,
        )


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
    rule: ClassVar[BaseRule]
    loc: Loc | NoLoc

    @computed_field
    @property
    @abstractmethod
    def message(self) -> str:
        pass

    def format(self, code: str | Mapping[str, str] | None = None) -> str:
        """`code` is the checked code, or the source of each `Loc.file` for excerpts."""
        s = ''
        s += f'{self.rule.id}({self.rule.severity.name.lower()}): {self.rule.description}\n'
        if isinstance(self.loc, Loc):
            loc = self.loc
            file = '' if loc.file is None else f'{loc.file}:'
            s += (
                f'->loc:{file}{loc.start_point[0]}:{loc.start_point[1]}'
                f'-{loc.end_point[0]}:{loc.end_point[1]}\n'
            )
            src: str | None
            match code:
                case None | str():
                    src = code
                case Mapping():
                    src = None if loc.file is None else code.get(loc.file)
            if src is not None:
                s += format_code_snippet_with_loc(src, loc.start_point, loc.end_point)
                s += '\n'
                s += f'help: {self.message}'
        return s


class RuleCheck(Protocol):
    rule: BaseRule

    def __call__(self, iml: str | None, eval_res: EvalRes | None) -> list[BaseDiag]: ...

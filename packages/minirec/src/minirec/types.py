from __future__ import annotations

from abc import abstractmethod
from typing import Annotated, Literal

from pydantic import BaseModel, Field, computed_field


class TopLevelDefinition(BaseModel):
    name: str

    start_byte: int
    end_byte: int
    start_point: tuple[int, int]
    end_point: tuple[int, int]

    measure: str | None
    opaque: bool


# Lintings
# ====================


class BaseLintingError(BaseModel):
    name: str
    start_byte: int
    end_byte: int
    start_point: tuple[int, int]
    end_point: tuple[int, int]

    @computed_field
    @property
    @abstractmethod
    def message(self) -> str:
        pass

    def format_error_message(self) -> str:
        s = ''
        s += f'{self.name}: {self.message}\n'
        s += f'location: {self.start_point[0]}:{self.start_point[1]}\n'
        return s


class NestedMeasureError(BaseLintingError):
    name: Literal['nested-measure-attribute'] = Field(
        default='nested-measure-attribute'
    )

    function_name: str
    measure: str = Field(description='Measure attribute text')
    top_function_name: str = Field(
        description='Name of the top-level function containing the error'
    )
    nesting_level: int

    @computed_field
    @property
    def message(self) -> str:
        s = ''
        s += f'Measure attribute `{self.measure}` should be attached to a '
        f'top-level function instead of a nested function `{self.function_name}`.\n'
        return s


class NestedRecursiveFunctionError(BaseLintingError):
    name: Literal['nested-recursive-function'] = Field(
        default='nested-recursive-function'
    )

    function_name: str
    top_function_name: str = Field(
        description='Name of the top-level function containing the error'
    )
    nesting_level: int

    @computed_field
    @property
    def message(self) -> str:
        s = ''
        s += f'Recursive function `{self.function_name}` nested in '
        f'{self.top_function_name} might cause proof-obligation difficulty and is'
        'generally discouraged.\n'
        return s


LintingError = Annotated[
    (NestedMeasureError | NestedRecursiveFunctionError), Field(discriminator='name')
]

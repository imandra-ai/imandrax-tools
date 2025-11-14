from __future__ import annotations

from pydantic import Field

from ..proto_utils import BaseModel


class Position(BaseModel):
    line: int
    col: int


class Location(BaseModel):
    file: str | None = Field(default=None)
    start: Position | None = Field(default=None)
    stop: Position | None = Field(default=None)

from pydantic import BaseModel, Field


class Position(BaseModel):
    line: int
    col: int


class Location(BaseModel):
    file: str | None = Field(default=None)
    start: Position | None = Field(default=None)
    stop: Position | None = Field(default=None)

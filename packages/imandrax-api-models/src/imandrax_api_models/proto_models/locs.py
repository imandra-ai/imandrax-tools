from __future__ import annotations

from collections.abc import Callable
from typing import Any, cast

from pydantic import Field

from ..proto_utils import BaseModel


class Position(BaseModel):
    line: int
    col: int


class Location(BaseModel):
    file: str | None = Field(default=None)
    start: Position | None = Field(default=None)
    stop: Position | None = Field(default=None)


def map_locations[T](obj: T, f: Callable[[Location], Location]) -> T:
    """
    Rebuild `obj` with `f` applied to every `Location` in it.

    Walks pydantic models, lists and tuples, wherever the `Location`s are
    (e.g. `EvalRes.errors[i].msg.locs`, `EvalRes.po_results[i].errors[j].stack[k].locs`).

    Parts with no changed `Location` are shared with `obj`, not copied.
    """
    # ai-disclosure: ai-generated

    def go(v: Any) -> Any:
        if isinstance(v, Location):
            return f(v)
        if isinstance(v, BaseModel):
            updates: dict[str, Any] = {}
            for name in type(v).model_fields:
                x = getattr(v, name)
                y = go(x)
                if y is not x:
                    updates[name] = y
            return v.model_copy(update=updates) if updates else v
        if isinstance(v, list | tuple):
            xs = cast('list[Any] | tuple[Any, ...]', v)
            ys = [go(x) for x in xs]
            if all(y is x for x, y in zip(xs, ys, strict=True)):
                return xs
            return tuple(ys) if isinstance(xs, tuple) else ys
        return v

    return go(obj)

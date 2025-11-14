from __future__ import annotations

from ..proto_utils import BaseModel


class Empty(BaseModel):
    """Void type, used for messages without arguments or return value."""

    pass


class StringMsg(BaseModel):
    msg: str

from pydantic import BaseModel


class Empty(BaseModel):
    """Void type, used for messages without arguments or return value."""

    pass


class StringMsg(BaseModel):
    msg: str

from __future__ import annotations

from pydantic import Field

from ..proto_utils import BaseModel


class Session(BaseModel):
    """A session identifier."""

    id: str = Field(description="The session's unique ID (e.g a uuid)")


class SessionCreate(BaseModel):
    """Create a new session."""

    po_check: bool = Field(default=True, description='Do we check Proof Obligations?')
    api_version: str = Field(description='the API types version (mandatory)')


class SessionOpen(BaseModel):
    """Reconnect to the given session."""

    id: Session | None = Field(
        default=None, description="The session's unique ID (e.g a uuid)"
    )
    api_version: str = Field(description='the API types version (mandatory)')

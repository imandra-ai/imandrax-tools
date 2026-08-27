from __future__ import annotations

from pydantic import Field

from ..proto_utils import BaseModel


class Gc_stats(BaseModel):
    """GC statistics."""

    heap_size_B: int
    major_collections: int
    minor_collections: int


class VersionResponse(BaseModel):
    version: str
    git_version: str | None = Field(default=None)

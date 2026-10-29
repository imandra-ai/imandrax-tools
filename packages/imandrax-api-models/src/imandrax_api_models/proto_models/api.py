"""Pydantic models for imandrax_api.bindings.api_pb2 types."""

from __future__ import annotations

import base64
import json
from enum import Enum
from io import BytesIO
from typing import Self
from zipfile import ZipFile

from imandrax_api.lib import Artifact as XtypesArtifact, read_artifact_data
from pydantic import Field, field_validator, model_validator

from ..proto_utils import BaseModel
from .artmsg import Artifact
from .error import Error
from .task import Task


class CodeSnippet(BaseModel):
    code: str
    task_filter: list[str] = Field(
        description=(
            'Glob patterns (`*`, `?`, `[...]`) for selecting tasks to be '
            'started during evaluation. The default is to start '
            'all tasks, but e.g. task_filter="*xyz*" would start '
            "only tasks pertaining to top-level definitions with 'xyz'"
            ' in their name. Tasks without names are matched under '
            '`anonymous`.'
        ),
    )


class EvalResult(Enum):
    EVAL_OK = 'EVAL_OK'
    EVAL_ERRORS = 'EVAL_ERRORS'


class CodeSnippetEvalResult(BaseModel):
    res: EvalResult
    duration_s: float
    tasks: list[Task] = Field(description='Tasks produced in the evaluation.')
    errors: list[Error] = Field(description='Errors occurring during evaluation.')


class ArtifactList(BaseModel):
    """The kinds of artifacts available for a task."""

    kinds: list[str] = Field(
        default_factory=list, description='Available artifact kinds'
    )


class ArtifactZip(BaseModel):
    """A zipped artifact containing twine-encoded data."""

    art_zip: bytes = Field(description='The raw zip file bytes')

    @field_validator('art_zip', mode='before')
    @classmethod
    def decode_base64(cls, v: str | bytes) -> bytes:
        # proto_to_dict() will encode the bytes as base64 string
        # when Pydantic sees a string assigned to a bytes field, it
        # just does `.encode('utf-8')` to convert it to bytes.
        if isinstance(v, str):
            return base64.b64decode(v)
        return v

    def to_artifact(self) -> XtypesArtifact:
        """
        Decode the zip bytes into an imandrax_api.lib.Artifact object.

        Returns:
            The decoded Artifact object.

        """
        with ZipFile(BytesIO(self.art_zip)) as f:
            manifest = json.loads(f.read('manifest.json'))
            kind = str(manifest['kind'])
            twine_data = f.read('data.twine')
        return read_artifact_data(data=twine_data, kind=kind)


class ArtifactResultError(Exception):
    """Error hold by an artifact result"""

    def __init__(self, error: Error) -> None:
        self.error = error
        msg = error.msg.msg if error.msg is not None else error.kind
        super().__init__(msg)


class _OkOrError[T](BaseModel):
    # oneof result
    ok: T | None = Field(default=None)
    error: Error | None = Field(default=None)
    # /oneof result

    @model_validator(mode='after')
    def one_of_result(self) -> Self:
        if (self.ok is None) == (self.error is None):
            raise ValueError('One of ok or error must be set')
        return self

    def unwrap(self) -> T:
        """The `ok` value, or raise `ArtifactResultError` on `error`."""
        if self.ok is None:
            assert self.error is not None
            raise ArtifactResultError(self.error)
        return self.ok


class ArtifactResult(_OkOrError[Artifact]):
    """Result of `get_artifact`, `parse_term` and `parse_type`."""


class ArtifactListResult(_OkOrError[ArtifactList]):
    """Result of listing artifacts for a task."""


class ArtifactZipResult(_OkOrError[ArtifactZip]):
    """Result of `get_artifact_zip`."""

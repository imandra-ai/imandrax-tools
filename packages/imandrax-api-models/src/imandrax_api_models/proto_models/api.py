"""Pydantic models for imandrax_api.bindings.api_pb2 types."""

from __future__ import annotations

import base64
import json
from enum import Enum
from io import BytesIO
from zipfile import ZipFile

from imandrax_api.lib import Artifact as XtypesArtifact, read_artifact_data
from pydantic import Field, field_validator

from ..proto_utils import BaseModel
from .artmsg import Art
from .error import Error
from .task import Task


class CodeSnippet(BaseModel):
    code: str
    task_filter: list[str] = Field(
        description=(
            'Regular expression for verification tasks to be '
            'started during evaluation. The default is to start '
            'all tasks, but e.g. task_filter="*xyz*" would start '
            "only tasks pertaining to top-level definitions with 'xyz'"
            ' in their name.'
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


class ArtifactListResult(BaseModel):
    """Result of listing artifacts for a task."""

    kinds: list[str] = Field(
        default_factory=list, description='Available artifact kinds'
    )


class Artifact(BaseModel):
    art: Art = Field(description='Requested artifact')


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

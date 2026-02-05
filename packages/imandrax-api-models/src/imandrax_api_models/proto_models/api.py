"""Pydantic models for imandrax_api.bindings.api_pb2 types."""

from __future__ import annotations

import json
from io import BytesIO
from zipfile import ZipFile

from imandrax_api.lib import Artifact, read_artifact_data
from pydantic import Field

from ..proto_utils import BaseModel


class ArtifactListResult(BaseModel):
    """Result of listing artifacts for a task."""

    kinds: list[str] = Field(
        default_factory=list, description='Available artifact kinds'
    )


class ArtifactZip(BaseModel):
    """A zipped artifact containing twine-encoded data."""

    art_zip: bytes = Field(description='The raw zip file bytes')

    def to_artifact(self) -> Artifact:
        """
        Decode the zip bytes into an Artifact.

        Returns:
            The decoded Artifact object.

        """
        with ZipFile(BytesIO(self.art_zip)) as f:
            manifest = json.loads(f.read('manifest.json'))
            kind = str(manifest['kind'])
            twine_data = f.read('data.twine')
        return read_artifact_data(data=twine_data, kind=kind)

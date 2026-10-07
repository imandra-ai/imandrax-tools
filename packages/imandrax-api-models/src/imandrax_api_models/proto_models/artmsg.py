# pyright: reportUnknownMemberType=false, reportUnknownVariableType=false
from __future__ import annotations

import imandrax_api.bindings.artmsg_pb2 as xartmsg_pb2
from pydantic import ConfigDict, Field

from ..proto_utils import BaseModel


class StorageEntry(BaseModel):
    model_config = ConfigDict(ser_json_bytes='base64', val_json_bytes='base64')

    key: str = Field(description='the CA store key')
    value: bytes = Field(description='the stored value')

    def to_proto(self) -> xartmsg_pb2.StorageEntry:
        return xartmsg_pb2.StorageEntry(key=self.key, value=self.value)


# We tend to generate this using `google.protobuf.json_format.MessageToDict`
# This converts protobuf bytes to base64 strings
class Art(BaseModel):
    model_config = ConfigDict(ser_json_bytes='base64', val_json_bytes='base64')

    kind: str = Field(description='The kind of artifact')
    data: bytes = Field(description='Serialized data, in twine')
    api_version: str = Field(
        description=(
            'Version of the API. This is mandatory and must match with the imandrax-api'
            ' library version.'
        )
    )
    storage: list[StorageEntry] = Field(
        default_factory=list, description='Additional definitions on the side'
    )

    def to_proto(self) -> xartmsg_pb2.Art:
        return xartmsg_pb2.Art(
            kind=self.kind,
            data=self.data,
            api_version=self.api_version,
            storage=[s.to_proto() for s in self.storage],
        )

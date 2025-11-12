from pydantic import BaseModel, ConfigDict, Field


class StorageEntry(BaseModel):
    model_config = ConfigDict(ser_json_bytes='base64', val_json_bytes='base64')

    key: str = Field(description='the CA store key')
    value: bytes = Field(description='the stored value')


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

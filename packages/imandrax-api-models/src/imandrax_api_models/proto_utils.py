from typing import Any

from google.protobuf.json_format import MessageToDict
from google.protobuf.message import Message
from pydantic import BaseModel as PydanticBaseModel, model_validator


def proto_to_dict(proto_obj: Message) -> dict[Any, Any]:
    return MessageToDict(
        proto_obj,
        preserving_proto_field_name=True,
        always_print_fields_with_no_presence=True,
    )


class BaseModel(PydanticBaseModel):
    @model_validator(mode='before')
    def validate_proto(cls, v: Any) -> dict[Any, Any]:
        if isinstance(v, Message):
            return proto_to_dict(v)
        return v

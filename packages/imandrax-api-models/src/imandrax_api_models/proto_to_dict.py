from typing import Any

from google.protobuf.json_format import MessageToDict
from google.protobuf.message import Message


def proto_to_dict(proto_obj: Message) -> dict[Any, Any]:
    return MessageToDict(
        proto_obj,
        preserving_proto_field_name=True,
        always_print_fields_with_no_presence=True,
    )

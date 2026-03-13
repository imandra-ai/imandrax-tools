from datetime import UTC, datetime
from enum import Enum

from imandrax_api_models import ErrorKind, ErrorMessage
from pydantic import BaseModel, ConfigDict, Field
from uuid_utils import uuid7


class ErrorTag(Enum):
    OCAML_CONFUSION = "OCAML_CONFUSION"
    TERMINATION_PROOFS = "TERMINATION_PROOFS"
    HIGHER_ORDER_RETURN_TYPES = "HIGHER_ORDER_RETURN_TYPES"
    OPAQUE = "OPAQUE"


class Item(BaseModel):
    # Meta data
    name: str | None = Field(default=None)
    id: str = Field(description="uuidv7", default_factory=lambda _: str(uuid7()))
    created_at: str = Field(
        description="created at",
        default_factory=lambda _: datetime.now(tz=UTC).isoformat(),
    )
    # Error
    kind: ErrorKind = Field(description="error kind")
    msg_str: str = Field(description="error message header from ImandraX API")
    tags: list[ErrorTag] = Field(description="error tags")
    # Context
    repro_iml: str = Field(
        description="minimum code to reproduce the error",
    )
    err_msg: ErrorMessage = Field(
        description="error message from ImandraX API, including header, location, and"
        " trace",
    )
    is_po_err: bool = Field(description="whether the error is a proof-obligation error")
    # Solution
    explanation: str | None = Field(
        default=None,
        description="explanation of the error",
    )
    solution: str = Field(description="solution")
    solution_description: str | None = Field()

    # Additional info
    prevention: str | None = Field(
        default=None,
        description="prevention hints",
    )
    additional_info: dict[str, str] = Field(
        default_factory=dict,
        description="additional info",
    )


class ItemTemplate(BaseModel):
    """Partial data for IML code"""

    # Meta data
    name: str
    id: str = Field(description="uuidv7", default_factory=lambda _: str(uuid7()))
    created_at: str = Field(
        description="created at",
        default_factory=lambda _: datetime.now(tz=UTC).isoformat(),
    )
    # Error
    kind: ErrorKind = Field(description="error kind")
    msg_str: str = Field(description="error message header from ImandraX API")
    tags: list[ErrorTag] = Field(description="error tags", default_factory=lambda _: [])
    # Context
    repro_iml: str = Field(
        description="minimum code to reproduce the error",
    )
    err_msg: ErrorMessage = Field(
        description="(the first) error message from ImandraX API, including header, "
        "location, and trace",
    )
    is_po_err: bool = Field(description="whether the error is a proof-obligation error")
    # # Solution
    # explanation: str | None = Field(
    #     default=None,
    #     description="explanation of the error",
    # )
    # solution: str = Field(description="solution")
    # # Additional info
    # prevention: str | None = Field(
    #     default=None,
    #     description="prevention hints",
    # )
    # additional_info: dict[str, str] = Field(
    #     default_factory=dict,
    #     description="additional info",
    # )

    model_config = ConfigDict(extra="allow")

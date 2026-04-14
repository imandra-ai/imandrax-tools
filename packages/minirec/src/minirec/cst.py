from dataclasses import dataclass
from typing import Literal

from iml_query.processing.decomp import DecompReqArgs
from iml_query.processing.vg import (
    VerifyReqArgs as VGReqArgs,
)
from pydantic import BaseModel

from .common import Loc


class TopLevelDefinition(BaseModel):
    name: str
    loc: Loc
    measure: str | None
    opaque: bool


@dataclass
class VGReq:
    kind: Literal['verify', 'instance']
    loc: Loc
    req_args: VGReqArgs


@dataclass
class DecompReq:
    loc: Loc
    req_args: DecompReqArgs

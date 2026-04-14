from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Literal, Self

from iml_query.processing.decomp import DecompReqArgs, decomp_capture_to_req
from iml_query.processing.vg import (
    VerifyReqArgs as VGReqArgs,
    instance_capture_to_req,
    verify_capture_to_req,
)
from iml_query.queries import (
    DecompCapture,
    InstanceCapture,
    VerifyCapture,
)
from pydantic import BaseModel

from .common import Loc

if TYPE_CHECKING:
    from tree_sitter import Range


def _range_to_loc(r: Range) -> Loc:
    return Loc(
        start_byte=r.start_byte,
        end_byte=r.end_byte,
        start_point=(r.start_point[0], r.start_point[1]),
        end_point=(r.end_point[0], r.end_point[1]),
    )


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

    @classmethod
    def from_capture(cls, cap: VerifyCapture | InstanceCapture) -> Self:
        match cap:
            case VerifyCapture():
                kind = 'verify'
                args, rng = verify_capture_to_req(cap)
            case InstanceCapture():
                kind = 'instance'
                args, rng = instance_capture_to_req(cap)
        return cls(kind=kind, loc=_range_to_loc(rng), req_args=args)


@dataclass
class DecompReq:
    loc: Loc
    req_args: DecompReqArgs

    @classmethod
    def from_capture(cls, cap: DecompCapture) -> Self:
        args, rng = decomp_capture_to_req(cap)
        return cls(loc=_range_to_loc(rng), req_args=args)

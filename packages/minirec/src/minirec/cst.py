from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Literal, Self

from iml_query.processing.base import resolve_nesting_definitions
from iml_query.processing.decomp import DecompReqArgs, decomp_capture_to_req
from iml_query.processing.vg import (
    VerifyReqArgs as VGReqArgs,
    instance_capture_to_req,
    verify_capture_to_req,
)
from iml_query.queries import (
    DecompCapture,
    InstanceCapture,
    MeasureCapture,
    OpaqueCapture,
    ValueDefCapture,
    VerifyCapture,
)
from iml_query.tree_sitter_utils import (
    get_nesting_relationship,
    unwrap_bytes,
)
from pydantic import BaseModel

from .common import (
    BaseDiag,
    Loc,
    NestedMeasureDiag,
    NestedRecursiveFunctionDiag,
)

if TYPE_CHECKING:
    from tree_sitter import Range


def range_to_loc(r: Range) -> Loc:
    return Loc(
        start_byte=r.start_byte,
        end_byte=r.end_byte,
        start_point=(r.start_point[0], r.start_point[1]),
        end_point=(r.end_point[0], r.end_point[1]),
    )


class TopLevelDefinition(BaseModel):
    name: str
    loc: Loc
    is_rec: bool
    is_opaque: bool
    measure: str | None


def parse_value_definitions(
    value_def_captures: list[ValueDefCapture],
    measure_captures: list[MeasureCapture],
    opaque_captures: list[OpaqueCapture],
) -> tuple[list[TopLevelDefinition], list[BaseDiag]]:
    top_defs = _captures_to_top_defs(
        value_def_captures, measure_captures, opaque_captures
    )

    diagnostics: list[BaseDiag] = []
    diagnostics.extend(_check_nested_measures(value_def_captures, measure_captures))
    diagnostics.extend(_check_nested_rec(value_def_captures))
    return top_defs, diagnostics


def _captures_to_top_defs(
    value_def_captures: list[ValueDefCapture],
    measure_captures: list[MeasureCapture],
    opaque_captures: list[OpaqueCapture],
) -> list[TopLevelDefinition]:
    """Extract top-level definitions from tree-sitter captures."""
    # Lookup maps
    measure_func_map: dict[str, MeasureCapture] = {
        unwrap_bytes(capture.function_name.text).decode('utf-8'): capture
        for capture in measure_captures
    }
    opaque_func_map: set[str] = {
        unwrap_bytes(capture.function_name.text).decode('utf-8')
        for capture in opaque_captures
    }

    top_defs: list[TopLevelDefinition] = []
    top_captures = [cap for cap in value_def_captures if cap.is_top_level]

    for top_capture in top_captures:
        top_name = top_capture.function_name
        top_def_node = top_capture.function_definition
        top_name_str = unwrap_bytes(top_name.text).decode('utf-8')

        # Detect measure attribute
        measure: None | str = None
        if top_name_str in measure_func_map:
            m_cap = measure_func_map[top_name_str]
            measure = unwrap_bytes(m_cap.measure_attr.text).decode('utf-8')

        # Detect opaque attribute
        opaque = top_name_str in opaque_func_map

        is_rec = top_capture.is_rec

        loc = Loc(
            start_byte=top_def_node.start_byte,
            end_byte=top_def_node.end_byte,
            start_point=(top_def_node.start_point[0], top_def_node.start_point[1]),
            end_point=(top_def_node.end_point[0], top_def_node.end_point[1]),
        )

        top_defs.append(
            TopLevelDefinition(
                name=top_name_str,
                loc=loc,
                is_rec=is_rec,
                measure=measure,
                is_opaque=opaque,
            )
        )

    return top_defs


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
        return cls(kind=kind, loc=range_to_loc(rng), req_args=args)


@dataclass
class DecompReq:
    loc: Loc
    req_args: DecompReqArgs

    @classmethod
    def from_capture(cls, cap: DecompCapture) -> Self:
        args, rng = decomp_capture_to_req(cap)
        return cls(loc=range_to_loc(rng), req_args=args)


# Rule-based checks
# ==================


def _check_nested_measures(
    value_def_captures: list[ValueDefCapture],
    measure_captures: list[MeasureCapture],
) -> list[NestedMeasureDiag]:
    """Find measure attributes on non-top-level functions."""
    top_captures = [cap for cap in value_def_captures if cap.is_top_level]
    top_level_names: set[str] = {
        unwrap_bytes(cap.function_name.text).decode('utf-8') for cap in top_captures
    }

    diagnostics: list[NestedMeasureDiag] = []
    for capture in measure_captures:
        func_name = unwrap_bytes(capture.function_name.text).decode('utf-8')
        if func_name in top_level_names:
            continue

        measure_def_node = capture.function_definition
        for top_capture in top_captures:
            top_def_node = top_capture.function_definition
            nesting_level = get_nesting_relationship(measure_def_node, top_def_node)
            if nesting_level > 0:
                loc = Loc(
                    start_byte=measure_def_node.start_byte,
                    end_byte=measure_def_node.end_byte,
                    start_point=(
                        measure_def_node.start_point[0],
                        measure_def_node.start_point[1],
                    ),
                    end_point=(
                        measure_def_node.end_point[0],
                        measure_def_node.end_point[1],
                    ),
                )
                diagnostics.append(
                    NestedMeasureDiag(
                        loc=loc,
                        function_name=func_name,
                        measure=unwrap_bytes(capture.measure_attr.text).decode('utf-8'),
                        top_function_name=unwrap_bytes(
                            top_capture.function_name.text
                        ).decode('utf-8'),
                        nesting_level=nesting_level,
                    )
                )

    return diagnostics


def _check_nested_rec(
    value_def_captures: list[ValueDefCapture],
) -> list[NestedRecursiveFunctionDiag]:
    """Find recursive functions that are nested (not top-level)."""
    nestings = resolve_nesting_definitions(value_def_captures)
    diagnostics: list[NestedRecursiveFunctionDiag] = []

    for nesting in nestings:
        child = nesting['child']
        if not child.is_rec:
            continue

        child_def_node = child.function_definition
        loc = Loc(
            start_byte=child_def_node.start_byte,
            end_byte=child_def_node.end_byte,
            start_point=(
                child_def_node.start_point[0],
                child_def_node.start_point[1],
            ),
            end_point=(
                child_def_node.end_point[0],
                child_def_node.end_point[1],
            ),
        )
        diagnostics.append(
            NestedRecursiveFunctionDiag(
                loc=loc,
                function_name=unwrap_bytes(child.function_name.text).decode('utf-8'),
                top_function_name=unwrap_bytes(
                    nesting['parent'].function_name.text
                ).decode('utf-8'),
                nesting_level=nesting['nesting_level'],
            )
        )

    return diagnostics

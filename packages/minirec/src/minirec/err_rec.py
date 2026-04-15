# ruff: noqa: RUF100, F401
from __future__ import annotations

from imandrax_api_models import Error, ErrorKind, EvalRes, Location
from pydantic import BaseModel

from .common import BaseDiag, InfixOperatorMissingParenthesesDiag


class EvalErrorItem(BaseModel):
    error: Error
    err_kind: ErrorKind

    @classmethod
    def from_error(cls, error: Error) -> EvalErrorItem:
        return cls(error=error, err_kind=ErrorKind.from_proto_kind(error.kind))


class EvalError(BaseModel):
    """Parsed eval error with enriched info."""

    errors: list[EvalErrorItem]
    po_errors: list[EvalErrorItem]

    def is_none(self) -> bool:
        return len(self.errors) == 0 and len(self.po_errors) == 0

    def first_error_kind(self) -> ErrorKind | None:
        if self.errors:
            return self.errors[0].err_kind
        return None


def _parse_eval_error(eval_res: EvalRes) -> EvalError:
    error_items = [EvalErrorItem.from_error(e) for e in eval_res.errors]
    po_error_items = [EvalErrorItem.from_error(e) for e in eval_res.po_errors]
    return EvalError(errors=error_items, po_errors=po_error_items)


def index_iml_by_loc(iml: str, loc: Location) -> tuple[str, str]:
    """
    Get the code snippet corresponding to the given location.

    Args:
        iml (str): The IML code.
        loc (Location): The location to index. (1-indexed)

    """
    lines = iml.splitlines()
    if loc.start is None or loc.stop is None:
        return '', ''
    start_line = loc.start.line - 1
    end_line = loc.stop.line - 1
    start_col = loc.start.col - 1
    end_col = loc.stop.col - 1

    line_narrowed = '\n'.join(lines[start_line : end_line + 1])
    col_narrowed = [
        line_narrowed[0][start_col:],
        *line_narrowed[1:-1],
        line_narrowed[-1][: end_col + 1],
    ]
    return '\n'.join(line_narrowed), '\n'.join(col_narrowed)


def check_infix_operator_missing_parentheses(
    iml: str, eval_res: EvalRes
) -> InfixOperatorMissingParenthesesDiag | None:
    eval_err = _parse_eval_error(eval_res)
    if eval_err.is_none() or eval_err.first_error_kind() != ErrorKind.SYNTAX_ERR:
        return None

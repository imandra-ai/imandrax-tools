# ruff: noqa: RUF100, F401
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import ClassVar, Self

from devtools import pprint
from imandrax_api_models import Error, ErrorKind, EvalRes, Location
from iml_query.queries import BaseCapture
from iml_query.tree_sitter_utils import (
    fmt_node,
    get_parser,
    mk_query,
    run_queries,
    run_query,
    unwrap_bytes,
)
from IPython.core.getipython import get_ipython
from pydantic import BaseModel
from pygments.unistring import c
from tree_sitter import Node

from minirec.cst import range_to_loc

from .common import BaseDiag, InfixOpMissingParenDiag


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


# ====================


@dataclass(slots=True, frozen=True)
class InfixOpMissingParenCapture(BaseCapture):
    QUERY: ClassVar[str] = """
    (value_definition
      "let"
      (ERROR
        ["land" "lor" "lxor" "lsl" "lsr" "asr" "mod"]) @op
      (let_binding))
    """

    op: Node

    @classmethod
    def from_code(cls, code: str) -> Self | None:
        # TODO: backport this to iml-query
        captures = run_query(query=cls.QUERY, code=code)
        if not captures:
            return None
        return cls.from_ts_capture(captures[0][1])

    def get_op(self) -> str:
        return unwrap_bytes(self.op.text).decode('utf8')


def check_infix_operator_missing_parentheses(
    iml: str, eval_res: EvalRes
) -> InfixOpMissingParenDiag | None:
    eval_err = _parse_eval_error(eval_res)
    if eval_err.is_none() or eval_err.first_error_kind() != ErrorKind.SYNTAX_ERR:
        return None

    if not (capture := InfixOpMissingParenCapture.from_code(iml)):
        return None
    op = capture.get_op()
    loc = range_to_loc(capture.op.range)
    return InfixOpMissingParenDiag(op=op, loc=loc)

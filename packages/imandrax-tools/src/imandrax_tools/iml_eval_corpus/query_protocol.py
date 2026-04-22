"""
Shared Protocol for corpus query modules.

Every `<category>/query/__main__.py` must expose three module-level names at
the bottom of the file (aliases are fine):

    RULE: BaseRule                        # the singleton rule instance
    Diag: type[BaseDiag]                  # the diag class (Diag.rule is RULE)
    check: Callable[[str, EvalRes], Diag | None]

A runtime conformance test verifies this contract before the repro tests run.
"""

from __future__ import annotations

from collections.abc import Callable
from typing import Protocol, runtime_checkable

from imandrax_api_models import EvalRes

from imandrax_tools.iml_eval_corpus.common import BaseDiag, BaseRule

CheckFn = Callable[[str, EvalRes], BaseDiag | None]


@runtime_checkable
class QueryModule(Protocol):
    RULE: BaseRule
    Diag: type[BaseDiag]
    check: CheckFn

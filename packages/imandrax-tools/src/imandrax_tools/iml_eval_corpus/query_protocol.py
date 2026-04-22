"""
Shared Protocol for corpus query modules.

Every `<category>/query` must expose three module-level names at
the bottom of the file (aliases are fine):

    RULE: BaseRule                        # the singleton rule instance
    Diag: type[BaseDiag]                  # the diag class (Diag.rule is RULE)
    check: Callable[[str, EvalRes], Diag | None]

"""

from __future__ import annotations

from collections.abc import Callable
from typing import Protocol, runtime_checkable

from imandrax_api_models import EvalRes

from imandrax_tools.iml_eval_corpus.common import BaseDiag, BaseRule

CheckFn = Callable[[str, EvalRes], BaseDiag | None]


@runtime_checkable
class QueryModule(Protocol):
    # Declared as properties so the Protocol attributes are read-only and
    # therefore covariant — a module exporting a BaseRule/BaseDiag subclass
    # still satisfies the contract.
    @property
    def RULE(self) -> BaseRule: ...
    @property
    def Diag(self) -> type[BaseDiag]: ...
    @property
    def check(self) -> CheckFn: ...

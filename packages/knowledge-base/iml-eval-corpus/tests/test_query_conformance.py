"""Precheck: every corpus `query/__main__.py` conforms to QueryModule Protocol."""

from __future__ import annotations

import inspect
from pathlib import Path

import pytest
from minirec.common import BaseDiag, BaseRule

from iml_eval_corpus.query_protocol import QueryModule

from .utils import discover_categories, load_query_module

CATEGORIES = discover_categories()


@pytest.mark.parametrize("category", CATEGORIES, ids=lambda p: p.name)
def test_query_module_conforms(category: Path) -> None:
    mod = load_query_module(category)

    # Presence
    assert hasattr(mod, "RULE"), f"{category.name}: missing `RULE`"
    assert hasattr(mod, "Diag"), f"{category.name}: missing `Diag`"
    assert hasattr(mod, "check"), f"{category.name}: missing `check`"

    # Types
    assert isinstance(mod.RULE, BaseRule), (
        f"{category.name}: `RULE` is not a BaseRule instance (got {type(mod.RULE)})"
    )
    assert isinstance(mod.Diag, type) and issubclass(mod.Diag, BaseDiag), (
        f"{category.name}: `Diag` is not a BaseDiag subclass"
    )
    assert callable(mod.check), f"{category.name}: `check` is not callable"

    # `check` takes exactly two params (iml, eval_res)
    sig = inspect.signature(mod.check)
    assert len(sig.parameters) == 2, (
        f"{category.name}: `check` must take 2 params, got {list(sig.parameters)}"
    )

    # Diag.rule matches the exported RULE (catches copy-paste drift)
    assert getattr(mod.Diag, "rule", None) is mod.RULE, (
        f"{category.name}: `Diag.rule` is not the same instance as `RULE`"
    )

    # Structural Protocol check (name + attribute presence)
    assert isinstance(mod, QueryModule), (
        f"{category.name}: does not satisfy QueryModule Protocol"
    )

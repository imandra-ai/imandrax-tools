# ai-disclosure: ai-assisted
"""
Parity tests for the sync/async clients in `imandrax_api_models.client`.

<agent>
`ImandraXClient` and `ImandraXAsyncClient` are hand-written twins: every method
of one must have a counterpart on the other with the same signature and the same
docstring, so that neither drifts when only one of them is edited.

The async docstrings may differ from the sync ones only by the `await ` in their
examples; nothing else is normalized away.
</agent>
"""

import inspect
from collections.abc import Callable
from typing import Any

import pytest
from imandrax_api_models.client import ImandraXAsyncClient, ImandraXClient


def _own_methods(cls: type) -> dict[str, Callable[..., Any]]:
    """Methods defined in the class body itself (not inherited)."""
    return {
        name: obj
        for name, obj in vars(cls).items()
        if inspect.isfunction(obj) and not name.startswith('__')
    }


SYNC_METHODS = _own_methods(ImandraXClient)
ASYNC_METHODS = _own_methods(ImandraXAsyncClient)
COMMON_METHODS = sorted(SYNC_METHODS.keys() & ASYNC_METHODS.keys())


def _normalize_doc(doc: str | None) -> str | None:
    if doc is None:
        return None
    # the only legitimate difference: `await client.foo()` in async examples
    return inspect.cleandoc(doc).replace('await ', '')


def test_same_methods() -> None:
    assert sorted(SYNC_METHODS) == sorted(ASYNC_METHODS) == COMMON_METHODS


@pytest.mark.parametrize('name', COMMON_METHODS)
def test_same_signature(name: str) -> None:
    sync_sig = inspect.signature(SYNC_METHODS[name])
    async_sig = inspect.signature(ASYNC_METHODS[name])
    assert str(sync_sig) == str(async_sig)


@pytest.mark.parametrize('name', COMMON_METHODS)
def test_same_docstring(name: str) -> None:
    sync_doc = _normalize_doc(SYNC_METHODS[name].__doc__)
    async_doc = _normalize_doc(ASYNC_METHODS[name].__doc__)
    assert sync_doc == async_doc

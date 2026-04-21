"""Each category's query catches its own `repro.iml` and if
`solution*.iml` is present — does not flag the solution.

The conformance precheck in `test_query_conformance.py` must pass first;
pytest runs these modules in alphabetical order, so any non-conforming
module will fail loudly there before this test module is reached.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from .utils import discover_categories, load_eval_res, load_query_module

CATEGORIES = discover_categories()
CATEGORIES_WITH_SOLUTION = [
    c
    for c in CATEGORIES
    if (c / "solution.iml").exists() and (c / "solutions_eval_res.json").exists()
]


@pytest.mark.parametrize("category", CATEGORIES, ids=lambda p: p.name)
def test_repro_is_caught(category: Path) -> None:
    mod = load_query_module(category)
    iml = (category / "repro.iml").read_text()
    eval_res = load_eval_res(category / "eval_res.json")
    diag = mod.check(iml, eval_res)
    assert diag is not None, (
        f"{category.name}: `check` returned None on its own repro.iml"
    )
    assert isinstance(diag, mod.Diag), (
        f"{category.name}: `check` returned {type(diag)}, expected {mod.Diag}"
    )


@pytest.mark.parametrize("category", CATEGORIES_WITH_SOLUTION, ids=lambda p: p.name)
def test_solution_not_flagged(category: Path) -> None:
    mod = load_query_module(category)
    iml = (category / "solution.iml").read_text()
    eval_res = load_eval_res(category / "solutions_eval_res.json", key="solution.iml")
    diag = mod.check(iml, eval_res)
    assert diag is None, (
        f"{category.name}: `check` unexpectedly flagged solution.iml with {diag!r}"
    )

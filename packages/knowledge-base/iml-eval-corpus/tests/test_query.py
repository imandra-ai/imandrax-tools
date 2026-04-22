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

# Each category may ship 0..N solutions, named `solution.iml`, `solution_2.iml`, ...
# Their eval results are all packed into a single `solutions_eval_res.json`
# keyed by filename. We parametrize over (category, solution_filename) pairs.
SOLUTION_CASES: list[tuple[Path, str]] = [
    (c, sol.name)
    for c in CATEGORIES
    if (c / "solutions_eval_res.json").exists()
    for sol in sorted(c.glob("solution*.iml"))
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


@pytest.mark.parametrize(
    ("category", "solution_name"),
    SOLUTION_CASES,
    ids=lambda v: v.name if isinstance(v, Path) else v,
)
def test_solution_not_flagged(category: Path, solution_name: str) -> None:
    mod = load_query_module(category)
    iml = (category / solution_name).read_text()
    eval_res = load_eval_res(category / "solutions_eval_res.json", key=solution_name)
    diag = mod.check(iml, eval_res)
    assert diag is None, (
        f"{category.name}/{solution_name}: `check` unexpectedly flagged with {diag!r}"
    )

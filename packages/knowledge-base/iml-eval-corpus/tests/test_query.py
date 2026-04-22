"""Each category's query catches its own `repro.iml` and if
`solution*.iml` is present it does not flag the solution.
"""

from __future__ import annotations

import importlib
import json
from pathlib import Path
from types import ModuleType
from typing import Any

import pytest
from imandrax_api_models import EvalRes

CORPUS_ROOT = Path(__file__).resolve().parent.parent / "corpus"


def discover_categories() -> list[Path]:
    """Return every sibling dir that looks like a corpus category.

    A category dir has a `query/__main__.py` and a `repro.iml`.  Dirs
    starting with `.` or `_` are skipped (templates, history, venv).
    """
    out: list[Path] = []
    for p in sorted(CORPUS_ROOT.iterdir()):
        if not p.is_dir():
            continue
        if p.name.startswith((".", "_")):
            continue
        if not (p / "query" / "__init__.py").exists():
            continue
        if not (p / "repro.iml").exists():
            continue
        out.append(p)
    return out


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


def load_query_module(category: Path) -> ModuleType:
    """Import `<category>/query` as a standalone module."""
    return importlib.import_module(f"corpus.{category.name}.query")


def load_eval_res(path: Path, *, key: str | None = None) -> EvalRes:
    """Load EvalRes from a corpus JSON file.

    Handles three observed shapes:
    - bare EvalRes
    - ``{"eval_res": EvalRes, ...}``                       (eval_res.json)
    - ``{"<filename.iml>": {"eval_res": EvalRes, ...}}``   (solutions_eval_res.json)

    Pass ``key`` (e.g. ``"solution.iml"``) to pick an entry from the filename-keyed shape.
    """
    payload: Any = json.loads(path.read_text())
    if isinstance(payload, dict):
        if key is not None and key in payload:
            payload = payload[key]  # pyright: ignore[reportUnknownVariableType]
        if isinstance(payload, dict) and "eval_res" in payload:
            return EvalRes.model_validate(payload["eval_res"])  # pyright: ignore[reportUnknownArgumentType]
    return EvalRes.model_validate(payload)


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

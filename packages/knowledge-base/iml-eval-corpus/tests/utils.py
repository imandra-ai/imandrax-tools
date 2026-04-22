"""Shared test utilities: discover category dirs and load their query modules."""

from __future__ import annotations

import importlib.util
import json
import sys
from pathlib import Path
from types import ModuleType
from typing import Any

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


def load_query_module(category: Path) -> ModuleType:
    """Import `<category>/query/__main__.py` as a standalone module."""
    mod_name = f"_corpus_query_{category.name.replace('-', '_')}"
    if mod_name in sys.modules:
        return sys.modules[mod_name]
    spec = importlib.util.spec_from_file_location(
        mod_name, category / "query" / "__init__.py"
    )
    assert spec is not None and spec.loader is not None, (
        f"could not build import spec for {category}"
    )
    mod = importlib.util.module_from_spec(spec)
    sys.modules[mod_name] = mod
    spec.loader.exec_module(mod)
    return mod


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

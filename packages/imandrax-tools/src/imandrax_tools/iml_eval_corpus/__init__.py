"""
Reusable building blocks for the iml-eval-corpus tooling.

Contains diagnostic primitives, IML CST helpers, and the query module
protocol. The corpus itself (under `corpus/`) is bundled at release time
via a sync step; use `corpus_path()` to locate it at runtime.
"""

from __future__ import annotations

from importlib.resources import as_file, files
from pathlib import Path


def corpus_path() -> Path:
    """
    Return the path to the bundled corpus directory.

    Works both from an installed wheel and from an editable checkout (after the
    release-sync script has populated `corpus/`). Raises FileNotFoundError if
    the corpus has not been synced.
    """
    resource = files(__name__) / 'corpus'
    with as_file(resource) as p:
        if not p.exists():
            raise FileNotFoundError(
                f'corpus not found at {p}; run the release-sync script to populate it',
            )
        return p

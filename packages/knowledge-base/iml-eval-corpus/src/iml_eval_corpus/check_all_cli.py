# pyright: basic
"""Dev CLI: evaluate an IML file via ImandraXClient and run corpus.check_all.

Usage:
    uv run iml-eval-corpus-check-all <path/to/file.iml>
"""

# ruff: noqa: E402

from __future__ import annotations

import json
import sys
from pathlib import Path
from typing import Annotated

# corpus/ lives as a sibling of src/ in this dev package; make it importable.
_PKG_ROOT = Path(__file__).resolve().parents[2]
if str(_PKG_ROOT) not in sys.path:
    sys.path.insert(0, str(_PKG_ROOT))

import typer
from imandrax_api import url_prod
from imandrax_api_models.client import ImandraXClient, get_imandrax_api_key
from imandrax_tools.iml_eval_corpus.common import BaseDiag
from pydantic import TypeAdapter

from corpus import check_all  # noqa: E402


def _run(
    iml: Annotated[Path, typer.Argument(help="Path to the IML file.")],
) -> None:
    src = iml.read_text()
    c = ImandraXClient(url=url_prod, auth_token=get_imandrax_api_key())
    eval_res = c.eval_src(src)

    diags = check_all(src, eval_res)
    out = {
        "diags": TypeAdapter(list[BaseDiag]).dump_python(diags, mode="json"),
        "eval_res": eval_res.model_dump(mode="json"),
    }
    typer.echo(json.dumps(out, indent=2))
    if not diags:
        raise typer.Exit(code=1)


def main() -> None:
    typer.run(_run)


if __name__ == "__main__":
    main()

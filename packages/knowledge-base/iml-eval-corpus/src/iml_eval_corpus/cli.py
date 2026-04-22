"""CLI helper for running a query/diag check against an IML file.

Each `query/__main__.py` defines a check function
`(iml: str, eval_res: EvalRes) -> BaseDiag | None`.  Wrap it with
`main_from_check(check_fn)` at the bottom of the module to expose a CLI:

    python -m measure-type-err.query <path/to/repro.iml>
    python -m measure-type-err.query <path/to/repro.iml> --eval-res <path.json>

Output is the diag as JSON on stdout, or an empty object `{}` when no match.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Annotated, Any, Callable, TypeVar

import typer
from imandrax_api_models import EvalRes

from iml_eval_corpus.common import BaseDiag

D = TypeVar("D", bound=BaseDiag)

CheckFn = Callable[[str, EvalRes], D | None]
CheckFn_1 = Callable[[str, EvalRes | None], D | None]


def _load_eval_res(path: Path) -> EvalRes:
    data: Any = json.loads(path.read_text())
    # Corpus JSON wraps EvalRes under an "eval_res" key; accept either shape.
    if isinstance(data, dict) and "eval_res" in data:
        inner: Any = data["eval_res"]  # pyright: ignore[reportUnknownVariableType]
        return EvalRes.model_validate(inner)
    return EvalRes.model_validate(data)


def main_from_check(check_fn: CheckFn[D]) -> None:
    """Expose `check_fn` as a Typer CLI and run it.

    Exits 0 on match (prints diag JSON), 1 on no match (prints `{}`).
    """

    def _run(
        iml: Annotated[Path, typer.Argument(help="Path to the IML file.")],
        eval_res: Annotated[
            Path | None,
            typer.Option(
                "--eval-res",
                help="Path to eval_res.json. Defaults to sibling of IML file.",
            ),
        ] = None,
    ) -> None:
        if eval_res is None or not eval_res.exists():
            raise typer.BadParameter(f"{eval_res} doesn't exist")
        eval_res_v: EvalRes = _load_eval_res(eval_res)

        diag = check_fn(iml.read_text(), eval_res_v)
        if diag is None:
            typer.echo("{}")
            raise typer.Exit(code=1)
        typer.echo(diag.model_dump_json(indent=2))

    _run.__doc__ = check_fn.__doc__ or "Run a diagnostic check on an IML file."
    typer.run(_run)

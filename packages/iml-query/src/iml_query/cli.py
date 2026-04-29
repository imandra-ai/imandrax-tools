# pyright: strict
# pyright: reportMissingTypeStubs=false
import sys
from pathlib import Path
from typing import Annotated

import structlog
import typer

from iml_query.multifile import gather_modules
from iml_query.tree_sitter_utils import (
    fmt_node,
    get_parser,
    run_query,
)

logger = structlog.getLogger(__name__)

app = typer.Typer(no_args_is_help=True)


@app.command()
def gather_module(path: Annotated[Path, typer.Argument()]) -> None:
    """Read IML code from a file or stdin."""
    iml = gather_modules(path)
    typer.echo(iml)


@app.command()
def query(
    path: Annotated[Path, typer.Argument()],
    ts_query: Annotated[str | None, typer.Option()] = None,
):
    iml = path.read_text() if path.name != '-' else sys.stdin.read()

    parser = get_parser()
    tree = parser.parse(bytes(iml, 'utf8'))

    if ts_query is not None:
        res = run_query(ts_query, code=iml)
        print(res[0][1])
    else:
        print(fmt_node(tree.root_node))


if __name__ == '__main__':
    app()

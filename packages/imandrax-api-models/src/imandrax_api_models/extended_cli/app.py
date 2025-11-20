import os
import sys
from pathlib import Path

import typer

from imandrax_api_models.client import get_imandrax_client
from imandrax_api_models.context_utils import format_eval_res
from imandrax_api_models.logging_utils import configure_logging

app = typer.Typer(name='ImandraX')


DEBUG = os.environ.get('DEBUG', '0') == '1'

if DEBUG:
    configure_logging('debug')
else:
    configure_logging('warning')


@app.command(
    name='eval',
)
def eval(
    iml_path: str | None = typer.Argument(
        None, help='Path of the IML file to evaluate'
    ),
):
    iml: str
    if iml_path is None:
        # Read from stdin if no path is provided
        iml = sys.stdin.read()
    else:
        if not Path(iml_path).exists():
            raise typer.BadParameter(f'IML file {iml_path} does not exist')
        iml = Path(iml_path).read_text()

    c = get_imandrax_client()
    eval_res = c.eval_src(src=iml)
    # breakpoint()
    typer.echo(format_eval_res(eval_res, iml))

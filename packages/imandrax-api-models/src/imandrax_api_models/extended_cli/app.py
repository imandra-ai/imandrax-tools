import os
import sys
from pathlib import Path

import typer
from iml_query.processing import (
    extract_decomp_reqs,
    extract_instance_reqs,
    extract_verify_reqs,
)
from iml_query.tree_sitter_utils import get_parser

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
        None,
        help='Path of the IML file to evaluate. Set to "-" to read from stdin.',
    ),
    strip_vg_requests: bool = typer.Option(
        True,
        help='Whether to strip verify and instance requests before evaluating.',
    ),
    strip_decomp_requests: bool = typer.Option(
        True,
        help='Whether to decomp requests before evaluating.',
    ),
):
    # Load IML
    iml: str
    if (iml_path is None) or (iml_path == '-'):
        # Read from stdin if no path is provided
        iml = sys.stdin.read()
    else:
        if not Path(iml_path).exists():
            raise typer.BadParameter(f'IML file {iml_path} does not exist')
        iml = Path(iml_path).read_text()

    if strip_vg_requests or strip_decomp_requests:
        tree = get_parser().parse(iml.encode('utf-8'))

        if strip_vg_requests:
            iml, tree, _verify_reqs = extract_verify_reqs(iml, tree)
            iml, tree, _instance_reqs = extract_instance_reqs(iml, tree)
        if strip_decomp_requests:
            iml, tree, _decomp_reqs = extract_decomp_reqs(iml, tree)

    c = get_imandrax_client()
    eval_res = c.eval_src(src=iml)
    typer.echo(format_eval_res(eval_res, iml))

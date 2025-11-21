# TODO(refactor): use Annotated
import os
import sys
from pathlib import Path
from typing import Literal, TypedDict, assert_never

import typer
from iml_query.processing import (
    extract_decomp_reqs,
    extract_instance_reqs,
    extract_verify_reqs,
)
from iml_query.tree_sitter_utils import get_parser

from imandrax_api_models.client import get_imandrax_async_client, get_imandrax_client
from imandrax_api_models.context_utils import format_eval_res
from imandrax_api_models.logging_utils import configure_logging

app = typer.Typer(name='ImandraX')


DEBUG = os.environ.get('DEBUG', '0') == '1'

if DEBUG:
    configure_logging('debug')
else:
    configure_logging('warning')


def _load_iml(path: str | None) -> str:
    if (path is None) or (path == '-'):
        # Read from stdin if no path is provided
        iml = sys.stdin.read()
    else:
        if not Path(path).exists():
            raise typer.BadParameter(f'IML file {path} does not exist')
        iml = Path(path).read_text()
    return iml


@app.command(
    name='eval',
)
def eval(
    file: str | None = typer.Argument(
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
    iml = _load_iml(file)

    if strip_vg_requests or strip_decomp_requests:
        tree = get_parser().parse(iml.encode('utf-8'))

        if strip_vg_requests:
            iml, tree, _verify_reqs, _ = extract_verify_reqs(iml, tree)
            iml, tree, _instance_reqs, _ = extract_instance_reqs(iml, tree)
        if strip_decomp_requests:
            iml, tree, _decomp_reqs = extract_decomp_reqs(iml, tree)

    c = get_imandrax_client()
    eval_res = c.eval_src(src=iml)
    typer.echo(format_eval_res(eval_res, iml))


# ====================


class VGItem(TypedDict):
    kind: Literal['verify', 'instance']
    src: str
    start_point: tuple[int, int]
    end_point: tuple[int, int]


def collect_vgs(iml: str) -> list[VGItem]:
    tree = get_parser().parse(iml.encode('utf-8'))
    iml, tree, verify_reqs, verify_req_ranges = extract_verify_reqs(iml, tree)
    iml, tree, instance_reqs, instance_req_ranges = extract_instance_reqs(iml, tree)

    # Collect
    vg_items: list[VGItem] = []
    for req, req_range in zip(verify_reqs, verify_req_ranges):
        vg_items.append(
            {
                'kind': 'verify',
                'src': req['src'],
                'start_point': (req_range.start_point[0], req_range.start_point[1]),
                'end_point': (req_range.end_point[0], req_range.end_point[1]),
            }
        )
    for req, req_range in zip(instance_reqs, instance_req_ranges):
        vg_items.append(
            {
                'kind': 'instance',
                'src': req['src'],
                'start_point': (req_range.start_point[0], req_range.start_point[1]),
                'end_point': (req_range.end_point[0], req_range.end_point[1]),
            }
        )
    vg_items.sort(key=lambda x: x['start_point'])
    return vg_items


@app.command(name='list-vg')
def list_vg(
    file: str | None = typer.Argument(
        None,
        help='Path of the IML file to check. Set to "-" to read from stdin.',
    ),
    # TODO(feature)
    json: bool = typer.Option(
        False,
        help='Whether to output the results in JSON format.',
    ),
):
    iml = _load_iml(file)

    vgs = collect_vgs(iml)

    for i, item in enumerate(vgs, 1):
        loc_str = f'{item["start_point"][0]}:{item["start_point"][1]}-{item["end_point"][0]}:{item["end_point"][1]}'
        typer.echo(f'{i}: {item["kind"]} ({loc_str}): {item["src"]}')


@app.command(name='check-vg')
async def check_vg(
    file: str | None = typer.Argument(
        None,
        help='Path of the IML file to check. Set to "-" to read from stdin.',
    ),
    index: list[int] = typer.Option(
        help='Name of the verification goal to check.',
    ),
    check_all: bool = typer.Option(
        False,
        help='Whether to check all verify requests in the IML file.',
    ),
):
    check_all = check_all or len(index) == 0

    iml = _load_iml(file)
    vgs = collect_vgs(iml)

    vgs: list[VGItem] = vgs if check_all else [vgs[i + 1] for i in index]
    async with get_imandrax_async_client() as c:
        tasks = []
        for vg in vgs:
            match vg['kind']:
                case 'verify':
                    tasks.append(c.verify_src(src=vg['src']))
                case 'instance':
                    tasks.append(c.instance_src(src=vg['src']))
                case _:
                    assert_never(vg)


@app.command(name='list-decomp')
def list_decomp(
    file: str | None = typer.Argument(
        None,
        help='Path of the IML file to check. Set to "-" to read from stdin.',
    ),
): ...


@app.command(name='check-decomp')
async def check_decomp(
    name: str | None = typer.Option(
        None,
        help='Name of the decomp request to check.',
    ),
    check_all: bool = typer.Option(
        False,
        help='Whether to check all decomp requests in the IML file.',
    ),
): ...

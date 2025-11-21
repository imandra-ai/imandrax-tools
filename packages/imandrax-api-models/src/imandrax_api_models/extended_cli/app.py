import asyncio
import json as jsonlib
import os
import sys
from pathlib import Path
from typing import Annotated, Literal, TypedDict, assert_never

import typer
from iml_query.processing import (
    extract_decomp_reqs,
    extract_instance_reqs,
    extract_verify_reqs,
)
from iml_query.processing.decomp import DecompReqArgs
from iml_query.tree_sitter_utils import get_parser

from imandrax_api_models.client import (
    ImandraXAsyncClient,
    get_imandrax_async_client,
    get_imandrax_client,
)
from imandrax_api_models.context_utils import (
    format_decomp_res,
    format_eval_res,
    format_vg_res,
)
from imandrax_api_models.logging_utils import configure_logging

app = typer.Typer(name='ImandraX')


DEBUG = os.environ.get('DEBUG', '0') == '1'
"""Env var to enable debug logging."""

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
    file: Annotated[
        str | None,
        typer.Argument(
            help='Path of the IML file to evaluate. Set to "-" to read from stdin.',
        ),
    ] = None,
    strip_vg_requests: Annotated[
        bool,
        typer.Option(
            help='Whether to strip verify and instance requests before evaluating.',
        ),
    ] = True,
    strip_decomp_requests: Annotated[
        bool,
        typer.Option(
            help='Whether to decomp requests before evaluating.',
        ),
    ] = True,
    json: Annotated[
        bool,
        typer.Option(
            help='Whether to output the results in JSON format.',
        ),
    ] = False,
):
    iml = _load_iml(file)

    # TODO(refactor): use eval_model
    if strip_vg_requests or strip_decomp_requests:
        tree = get_parser().parse(iml.encode('utf-8'))

        if strip_vg_requests:
            iml, tree, _verify_reqs, _ = extract_verify_reqs(iml, tree)
            iml, tree, _instance_reqs, _ = extract_instance_reqs(iml, tree)
        if strip_decomp_requests:
            iml, tree, _decomp_reqs, _ = extract_decomp_reqs(iml, tree)

    c = get_imandrax_client()
    eval_res = c.eval_src(src=iml)
    typer.echo(format_eval_res(eval_res, iml))


# ====================


class VGItem(TypedDict):
    kind: Literal['verify', 'instance']
    src: str
    start_point: tuple[int, int]
    end_point: tuple[int, int]


def _collect_vgs(iml: str) -> list[VGItem]:
    tree = get_parser().parse(iml.encode('utf-8'))
    iml, tree, verify_reqs, verify_req_ranges = extract_verify_reqs(iml, tree)
    iml, tree, instance_reqs, instance_req_ranges = extract_instance_reqs(iml, tree)

    # Collect
    vg_items: list[VGItem] = []
    for req, req_range in zip(verify_reqs, verify_req_ranges, strict=True):
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
    file: Annotated[
        str | None,
        typer.Argument(
            help='Path of the IML file to check. Set to "-" to read from stdin.',
        ),
    ] = None,
    json: Annotated[
        bool,
        typer.Option(
            help='Whether to output the results in JSON format.',
        ),
    ] = False,
):
    iml = _load_iml(file)

    vgs: list[VGItem] = _collect_vgs(iml)

    if not json:
        for i, item in enumerate(vgs, 1):
            loc_str = f'{item["start_point"][0]}:{item["start_point"][1]}-{item["end_point"][0]}:{item["end_point"][1]}'
            typer.echo(f'{i}: {item["kind"]} ({loc_str}): {item["src"]}')
    else:
        json_s = jsonlib.dumps(vgs, indent=2)
        typer.echo(json_s)


@app.command(name='check-vg')
def check_vg(
    file: Annotated[
        str | None,
        typer.Argument(
            help='Path of the IML file to check. Set to "-" to read from stdin.',
        ),
    ] = None,
    index: Annotated[
        list[int],
        typer.Option(
            help='Name of the verification goal to check.',
        ),
    ] = [],
    check_all: Annotated[
        bool,
        typer.Option(
            help='Whether to check all verify requests in the IML file.',
        ),
    ] = False,
    json: Annotated[
        bool,
        typer.Option(
            help='Whether to output the results in JSON format.',
        ),
    ] = False,
):
    async def _async_check_vg():
        iml = _load_iml(file)
        vgs = _collect_vgs(iml)

        index_: list[int] = (
            list(range(1, len(vgs) + 1)) if check_all or (len(index) == 0) else index
        )

        vg_with_idx: list[tuple[int, VGItem]] = [
            (i, vg) for (i, vg) in enumerate(vgs, 1) if i in index_
        ]

        async def _check_vg(vg: VGItem, i: int, c: ImandraXAsyncClient):
            match vg['kind']:
                case 'verify':
                    res = await c.verify_src(src=vg['src'])
                case 'instance':
                    res = await c.instance_src(src=vg['src'])
                case _:
                    assert_never(vg['kind'])
            typer.echo(f'{i}: {vg["kind"]} ({vg["src"]})')
            typer.echo(format_vg_res(res))

        async with get_imandrax_async_client() as c:
            eval_res = await c.eval_model(src=iml)
            typer.echo(format_eval_res(eval_res, iml))
            if eval_res.has_errors:
                typer.echo('Error(s) found in IML file. Exiting.')
                sys.exit(1)
                return
            typer.echo('\n' + '=' * 5 + 'VG' + '=' * 5 + '\n')
            tasks = [_check_vg(vg, i, c) for (i, vg) in vg_with_idx]
            await asyncio.gather(*tasks)

    asyncio.run(_async_check_vg())


# ====================


class DecompItem(TypedDict):
    req_args: DecompReqArgs
    start_point: tuple[int, int]
    end_point: tuple[int, int]


def _collect_decomps(iml: str) -> list[DecompItem]:
    tree = get_parser().parse(iml.encode('utf-8'))
    iml, tree, decomp_reqs, ranges = extract_decomp_reqs(iml, tree)

    decomp_items: list[DecompItem] = [
        DecompItem(
            req_args=req,
            start_point=range_.start_point,
            end_point=range_.end_point,
        )
        for req, range_ in zip(decomp_reqs, ranges, strict=True)
    ]

    decomp_items.sort(key=lambda x: x['start_point'])
    return decomp_items


@app.command(name='list-decomp')
def list_decomp(
    file: Annotated[
        str | None,
        typer.Argument(
            help='Path of the IML file to check. Set to "-" to read from stdin.',
        ),
    ] = None,
    json: Annotated[
        bool,
        typer.Option(
            help='Whether to output the results in JSON format.',
        ),
    ] = False,
):
    iml = _load_iml(file)
    decomps = _collect_decomps(iml)

    if not json:
        for i, item in enumerate(decomps, 1):
            typer.echo(f'{i}: {item["req_args"]["name"]}')
    else:
        json_s = jsonlib.dumps(decomps, indent=2)
        typer.echo(json_s)


@app.command(name='check-decomp')
def check_decomp(
    file: Annotated[
        str | None,
        typer.Argument(
            help='Path of the IML file to check. Set to "-" to read from stdin.',
        ),
    ] = None,
    index: Annotated[
        list[int],
        typer.Option(
            help='Index of the decomposition request to check.',
        ),
    ] = [],
    check_all: Annotated[
        bool,
        typer.Option(
            help='Whether to check all decomp requests in the IML file.',
        ),
    ] = False,
    json: Annotated[
        bool,
        typer.Option(
            help='Whether to output the results in JSON format.',
        ),
    ] = False,
):
    async def _async_check_decomp():
        iml = _load_iml(file)
        decomps = _collect_decomps(iml)

        index_: list[int] = (
            list(range(1, len(decomps) + 1))
            if check_all or (len(index) == 0)
            else index
        )

        decomp_with_idx: list[tuple[int, DecompItem]] = [
            (i, decomp) for (i, decomp) in enumerate(decomps, 1) if i in index_
        ]

        async def _check_decomp(decomp: DecompItem, i: int, c: ImandraXAsyncClient):
            typer.echo(f'{i}: decompose {decomp["req_args"]["name"]}')
            res = await c.decompose(**decomp['req_args'])
            typer.echo(format_decomp_res(res))

        async with get_imandrax_async_client() as c:
            eval_res = await c.eval_model(src=iml)
            typer.echo(format_eval_res(eval_res, iml))
            if eval_res.has_errors:
                typer.echo('Error(s) found in IML file. Exiting.')
                sys.exit(1)
                return

            typer.echo('\n' + '=' * 5 + 'Decomp' + '=' * 5 + '\n')
            tasks = [_check_decomp(decomp, i, c) for (i, decomp) in decomp_with_idx]
            await asyncio.gather(*tasks)

    asyncio.run(_async_check_decomp())

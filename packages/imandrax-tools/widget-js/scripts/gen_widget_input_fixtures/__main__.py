"""
Generate JS test fixtures for widgets from real API output.

Each fixture is the *exact widget input* -- the list the Python side syncs to
the frontend traitlet, not the raw API response:

- decomp -> `EnrichedDecomposeRes.region_group_views()`  (the `data` traitlet)
- tasks  -> `collect_tasks_artifacts(eval_res.tasks, c)`  (the `task_entries` traitlet)

Flag:
    --refresh: force re-calling the API and regenerating the fixture
"""

from __future__ import annotations

import asyncio
import json
import sys
from pathlib import Path
from typing import Any, Literal, cast

import imandrax_api
import yaml
from imandrax_api_models.artifacts import artifact_reprs_of_tasks
from imandrax_api_models.client import ImandraXClient, get_imandrax_async_client
from imandrax_api_models.region_decomp import EnrichedDecomposeRes
from imandrax_tools.idf.iter_decomp import Step, iter_decomp
from imandrax_tools.idf.viz_view import View

CURR_DIR = Path(__file__).resolve().parents[0]
PKG_JSON_DIR = CURR_DIR.parents[1]
OUT_DIR = PKG_JSON_DIR / 'test/fixtures/inputs/'

# IDF inputs are the `sm`/`tpl`/`message_flows` YAML files that drive
# `iter_decomp`; we reuse the ones the Python IDF tests already ship.
IDF_INPUTS_DIR = PKG_JSON_DIR.parent / 'tests/test_idf/data/idf_inputs'
# Which of those to emit widget fixtures for (name -> fixture stem `idf.<name>`).
IDF_FIXTURES = ['addx', 'choose', 'xy_template']

# ====================
# Fixture sources are named `<widget_type>.<name>.<additional_data>.iml`
#
# widget_type -> file_name -> name -> iml -> additional_data
inputs: list[tuple[Literal['decomp', 'tasks'], str, str, str, str | None]] = []

for f in CURR_DIR.glob('*.iml'):
    parts = f.stem.split('.')
    if parts[0] == 'decomp':
        inputs.append(('decomp', f.name, parts[1], f.read_text(), parts[2]))
    elif parts[0] == 'tasks':
        inputs.append(('tasks', f.name, parts[1], f.read_text(), None))
    else:
        print(f'Unrecognized file: {f.name}')
print(f'Found {len(inputs)} inputs.')


def _client() -> ImandraXClient:
    import os

    return ImandraXClient(
        url=imandrax_api.url_dev, auth_token=os.environ['IMANDRAX_API_KEY']
    )


def decomp_widget_input(decompose_name: str, iml: str) -> list[dict[str, Any]]:
    """The `RegionDecompWidget.data` traitlet: a `RegionGroupView` list."""
    c = _client()
    c.eval_src(iml)
    decomp_res = c.decompose(name=decompose_name, string_results=True, prune=True)
    enriched = EnrichedDecomposeRes.from_decomp_res(decomp_res)
    return [v.model_dump(mode='json') for v in enriched.region_group_views()]


def tasks_widget_input(iml: str) -> list[dict[str, Any]]:
    """The `TasksWidget.task_entries` traitlet: a `TaskEntry` list."""
    c = _client()
    eval_res = c.eval_src(iml)
    entries = artifact_reprs_of_tasks(eval_res.tasks, c)
    return [e.model_dump(mode='json') for e in entries]


def idf_widget_input(name: str) -> dict[str, Any]:
    """The `IDFWidget.data` traitlet: a serialized `View`."""
    import os

    data = yaml.safe_load((IDF_INPUTS_DIR / f'{name}.yml').read_text())

    async def _run() -> View:
        async with get_imandrax_async_client(
            auth_token=os.environ['IMANDRAX_API_KEY'], env='dev'
        ) as c:
            tag, steps_or_err = await iter_decomp(
                c,
                sm=data['sm'],
                tpl=data['tpl'],
                message_flows=data['message_flows'],
            )
            assert tag == 'left', f'IDF decomp failed: {steps_or_err}'
            return View.from_steps(cast(list[Step], steps_or_err))

    return asyncio.run(_run()).model_dump(mode='json')


def main() -> None:
    refresh = '--refresh' in sys.argv[1:]
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    for widget_type, file_name, name, iml, additional_data in inputs:
        out_path = OUT_DIR / f'{file_name}.widget_input.json'
        if not (refresh or not out_path.exists()):
            print(f'[{file_name}] using cached {out_path.relative_to(PKG_JSON_DIR)}')
            continue

        if widget_type == 'decomp':
            assert additional_data is not None
            widget_input = decomp_widget_input(additional_data, iml)
        elif widget_type == 'tasks':
            widget_input = tasks_widget_input(iml)
        else:
            raise ValueError(f'Unknown widget type: {widget_type}')

        out_path.write_text(json.dumps(widget_input, indent=2))
        print(f'[{file_name}] wrote {out_path.relative_to(PKG_JSON_DIR)}')

    # IDF fixtures: `idf.<name>.iml.widget_input.json` (the `.iml` segment keeps
    # the gallery's `<type>.<name>...` filename convention uniform).
    for name in IDF_FIXTURES:
        out_path = OUT_DIR / f'idf.{name}.iml.widget_input.json'
        if not (refresh or not out_path.exists()):
            print(f'[idf.{name}] using cached {out_path.relative_to(PKG_JSON_DIR)}')
            continue
        widget_input = idf_widget_input(name)
        out_path.write_text(json.dumps(widget_input, indent=2))
        print(f'[idf.{name}] wrote {out_path.relative_to(PKG_JSON_DIR)}')


if __name__ == '__main__':
    main()

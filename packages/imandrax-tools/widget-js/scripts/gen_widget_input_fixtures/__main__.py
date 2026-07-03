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

import json
import sys
from pathlib import Path
from typing import Any, Literal

import imandrax_api
from imandrax_api_models.client import ImandraXClient
from imandrax_api_models.region_decomp import EnrichedDecomposeRes

from imandrax_tools.widget._tasks import collect_tasks_artifacts

CURR_DIR = Path(__file__).resolve().parents[0]
PKG_JSON_DIR = CURR_DIR.parents[1]
OUT_DIR = PKG_JSON_DIR / 'test/fixtures/inputs/'

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
    entries = collect_tasks_artifacts(eval_res.tasks, c)
    return [e.model_dump(mode='json') for e in entries]


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


if __name__ == '__main__':
    main()

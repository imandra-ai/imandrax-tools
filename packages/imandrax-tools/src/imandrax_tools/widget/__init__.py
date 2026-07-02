"""
anywidget-based rendering of ImandraX results.

Two widgets, both backed by JS bundles under `widget/static`:

- `TasksWidget` -- a collapsible view of each task's pretty-printed artifacts.
  `register_repr_html(c)` attaches it to `EvalRes` / `CodeSnippetEvalResult`.
- `RegionDecompWidget` -- the region-decomposition treemap.
  `register_region_decomp_repr()` attaches it to `EnrichedDecomposeRes` /
  `DecomposeRes` (a plain `DecomposeRes` is enriched first).
"""

from __future__ import annotations

import asyncio
from pathlib import Path
from typing import Any, Protocol, assert_never

import anywidget
import traitlets
from imandrax_api_models import CodeSnippetEvalResult, EvalRes, Task
from imandrax_api_models.client import (
    ImandraXAsyncClient,
    ImandraXClient,
    async_get_task_artifacts,
    get_task_artifacts,
)
from imandrax_api_models.pp.xtype import to_string as string_of_xtype
from imandrax_api_models.region_decomp import RegionGroup

_DIST = Path(__file__).parent / 'static'


class HasTasks(Protocol):
    tasks: list[Task]


# Task-artifact widget
# ====================


def _task_entry(task: Task, artifacts: dict[str, Any]) -> dict[str, Any]:
    """Build one JSON-serialisable task entry for the `tasks` trait."""
    return {
        'id': getattr(task, 'id', '') or '',
        'kind': task.kind.value,
        'artifacts': [
            {'kind': a_kind, 'text': string_of_xtype(xval)}
            for a_kind, xval in artifacts.items()
        ],
    }


def collect_tasks_data(
    tasks: list[Task], c: ImandraXClient | ImandraXAsyncClient
) -> list[dict[str, Any]]:
    """Fetch + decode + pretty-print artifacts for each task into trait data."""
    match c:
        case ImandraXClient():
            return [_task_entry(t, get_task_artifacts(t, c)) for t in tasks]
        case ImandraXAsyncClient() as ac:
            # Bind the narrowed client to a fresh name: the closure below captures
            # it, and a captured `c` would revert to the un-narrowed union type.
            async def _gather() -> list[dict[str, Any]]:
                artifacts = await asyncio.gather(
                    *[async_get_task_artifacts(t, ac) for t in tasks]
                )
                return [_task_entry(t, a) for t, a in zip(tasks, artifacts)]

            return asyncio.run(_gather())
        case _:
            assert_never(c)


class TasksWidget(anywidget.AnyWidget):
    """Collapsible view of pretty-printed artifacts for a list of tasks."""

    _esm = _DIST / 'task.js'

    tasks = traitlets.List().tag(sync=True)  # pyright: ignore[reportAssignmentType]


def tasks_widget(
    tasks: list[Task], c: ImandraXClient | ImandraXAsyncClient
) -> TasksWidget:
    """Build a `TasksWidget` for the given tasks."""
    return TasksWidget(tasks=collect_tasks_data(tasks, c))


# Region-decomposition widget
# ====================


class RegionDecompWidget(anywidget.AnyWidget):
    """Treemap view of a region-group forest (see `widget-js/src/region_decomp`)."""

    _esm = _DIST / 'region_decomp.js'

    data = traitlets.List().tag(sync=True)  # pyright: ignore[reportAssignmentType]


def _region_group_node(rg: RegionGroup) -> dict[str, Any]:
    """
    A treemap node for one `RegionGroup`.

    Group-level fields (`label_path`, `constraints`, `weight`, `children`) feed
    the tiles and detail stats. A leaf additionally carries `region_stat`: its
    concrete region's display stats (`Region.stat()` -- invariant, example
    input/output), shown in the detail panel.
    """
    node: dict[str, Any] = {
        'label_path': rg.label_path,
        'constraints': rg.constraints,
        'weight': rg.weight,
        'children': [_region_group_node(c) for c in rg.children],
        'region_stat': None,
    }
    if rg.region is not None:
        node['region_stat'] = rg.region.non_group_stat()
    return node


def region_decomp_widget(enriched: Any) -> RegionDecompWidget:
    """Build a `RegionDecompWidget` from an `EnrichedDecomposeRes`."""
    return RegionDecompWidget(
        data=[_region_group_node(g) for g in enriched.region_groups]
    )


# Notebook integration
# ====================

_client: ImandraXClient | ImandraXAsyncClient | None = None


def register_region_decomp_repr() -> None:
    """
    Make `EnrichedDecomposeRes` / `DecomposeRes` render as a `RegionDecompWidget`.

    A plain `DecomposeRes` is enriched (grouped) first. Sets `_repr_mimebundle_`
    on both classes, replacing the former `_repr_html_` icicle rendering.
    """
    from imandrax_api_models.proto_models import DecomposeRes
    from imandrax_api_models.region_decomp import EnrichedDecomposeRes

    def repr_mimebundle(self: Any, include: Any = None, exclude: Any = None):
        enriched = (
            self
            if isinstance(self, EnrichedDecomposeRes)
            else EnrichedDecomposeRes.from_decomp_res(self)
        )
        if enriched.errors:
            return {'text/plain': str(enriched.errors)}
        widget = region_decomp_widget(enriched)
        # Call anywidget's impl explicitly, rather than via `widget._repr_mimebundle_`:
        # the latter resolves to a union with `ipywidgets.Widget._repr_mimebundle_`
        # that some type checkers can't bind `self` on.
        return anywidget.AnyWidget._repr_mimebundle_(
            widget, include=include, exclude=exclude
        )

    setattr(EnrichedDecomposeRes, '_repr_mimebundle_', repr_mimebundle)
    setattr(DecomposeRes, '_repr_mimebundle_', repr_mimebundle)


def register_repr_html(c: ImandraXClient | ImandraXAsyncClient) -> None:
    """
    Attach widget renderers to result types.

    `EvalRes` / `CodeSnippetEvalResult` render as a `TasksWidget`, and
    `EnrichedDecomposeRes` / `DecomposeRes` as a `RegionDecompWidget`. Sets
    `_repr_mimebundle_` (the ipywidgets display hook) on each class, delegating
    to a freshly built widget.
    """
    global _client
    _client = c

    def repr_mimebundle(self: HasTasks, include: Any = None, exclude: Any = None):
        assert _client is not None
        widget = tasks_widget(self.tasks, _client)
        return anywidget.AnyWidget._repr_mimebundle_(
            widget, include=include, exclude=exclude
        )

    setattr(EvalRes, '_repr_mimebundle_', repr_mimebundle)
    setattr(CodeSnippetEvalResult, '_repr_mimebundle_', repr_mimebundle)

    register_region_decomp_repr()

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

from pathlib import Path
from typing import Any, Self

import anywidget
import traitlets
from imandrax_api_models import CodeSnippetEvalResult, DecomposeRes, EvalRes
from imandrax_api_models.client import ImandraXAsyncClient, ImandraXClient
from imandrax_api_models.context_utils import string_of_model as xapi_to_string
from imandrax_api_models.region_decomp import EnrichedDecomposeRes

from imandrax_tools.widget._tasks import HasTasks, collect_tasks_artifacts

_DIST = Path(__file__).parent / 'static'


class TasksWidget(anywidget.AnyWidget):
    """Collapsible view of pretty-printed artifacts for a list of tasks."""

    _esm = _DIST / 'task.js'

    api_resp_with_tasks = traitlets.Any().tag(sync=True)  # pyright: ignore[reportAssignmentType]
    task_entries = traitlets.List().tag(sync=True)  # Derived field

    @classmethod
    def from_has_tasks(
        cls, obj: HasTasks, c: ImandraXClient | ImandraXAsyncClient
    ) -> Self:
        task_entries = collect_tasks_artifacts(obj.tasks, c)
        return cls(api_resp_with_tasks=obj, task_entries=task_entries)

    def _repr_mimebundle_(self, include: Any = None, exclude: Any = None) -> dict:
        if len(self.task_entries) == 0:
            return {'text/plain': xapi_to_string(self.api_resp_with_tasks)}
        else:
            return anywidget.AnyWidget._repr_mimebundle_(
                self.api_resp_with_tasks, include=include, exclude=exclude
            )


class RegionDecompWidget(anywidget.AnyWidget):
    """Treemap view of a region-group forest."""

    _esm = _DIST / 'region_decomp.js'

    decomp_res = traitlets.Any().tag(sync=True)

    @classmethod
    def from_decomp_res(cls, decomp_res: EnrichedDecomposeRes | DecomposeRes) -> Self:
        match decomp_res:
            case EnrichedDecomposeRes():
                return cls(
                    data=[
                        v.model_dump(mode='json')
                        for v in decomp_res.region_group_views()
                    ]
                )
            case DecomposeRes():
                enriched = EnrichedDecomposeRes.from_decomp_res(decomp_res)
                return cls(
                    data=[
                        v.model_dump(mode='json') for v in enriched.region_group_views()
                    ]
                )

    def _repr_mimebundle_(self, include: Any = None, exclude: Any = None):
        if self.decomp_res.errors:
            return {'text/plain': xapi_to_string(self.decomp_res)}
        else:
            # Only resolve to JS if there are no errors
            return anywidget.AnyWidget._repr_mimebundle_(
                self, include=include, exclude=exclude
            )


# Notebook integration
# ====================

_client: ImandraXClient | ImandraXAsyncClient | None = None


def register_tasks_widget(c: ImandraXClient | ImandraXAsyncClient) -> None:
    """
    Make `EvalRes` and `CodeSnippetEvalResult` render as a `TasksWidget`.
    """
    global _client
    _client = c

    def repr_mimebundle(self: HasTasks, include: Any = None, exclude: Any = None):
        assert _client is not None
        widget = TasksWidget.from_has_tasks(self, _client)
        return anywidget.AnyWidget._repr_mimebundle_(
            widget, include=include, exclude=exclude
        )

    setattr(EvalRes, '_repr_mimebundle_', repr_mimebundle)
    setattr(CodeSnippetEvalResult, '_repr_mimebundle_', repr_mimebundle)


def register_region_decomp_widget() -> None:
    """
    Make `EnrichedDecomposeRes` / `DecomposeRes` render as a `RegionDecompWidget`.
    """

    def repr_mimebundle(
        self: DecomposeRes | EnrichedDecomposeRes,
        include: Any = None,
        exclude: Any = None,
    ):
        widget = RegionDecompWidget.from_decomp_res(self)
        return anywidget.AnyWidget._repr_mimebundle_(
            widget, include=include, exclude=exclude
        )

    setattr(EnrichedDecomposeRes, '_repr_mimebundle_', repr_mimebundle)
    setattr(DecomposeRes, '_repr_mimebundle_', repr_mimebundle)


def register_widgets(c: ImandraXClient | ImandraXAsyncClient | None) -> None:
    """
    Attach widget renderers to result types.
    """
    register_region_decomp_widget()

    if c is not None:
        register_tasks_widget(c)

# pyright: reportPrivateUsage=false, reportUnknownMemberType=false, reportUnknownVariableType=false
"""
anywidget-based rendering of ImandraX results.

Two widgets, both backed by JS bundles under `widget/static`:

- `TasksWidget` -- a collapsible view of each task's pretty-printed artifacts.
  `register_tasks_widget(c)` attaches it to `EvalRes` / `CodeSnippetEvalResult`.
- `RegionDecompWidget` -- the region-decomposition treemap.
  `register_region_decomp_widget()` attaches it to `EnrichedDecomposeRes` /
  `DecomposeRes` (a plain `DecomposeRes` is enriched first).

Each widget also overrides `_repr_mimebundle_` to fall back to a `text/plain`
pretty-print when there is nothing to render (no tasks / decomposition errors).
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

from imandrax_tools.idf.viz_view import View as IDFView
from imandrax_tools.widget._tasks import HasTasks, collect_tasks_artifacts

_DIST = Path(__file__).parent / 'static'


class TasksWidget(anywidget.AnyWidget):
    """Collapsible view of pretty-printed artifacts for a list of tasks."""

    _esm = _DIST / 'task.js'

    # Synced to JS (the `task.js` bundle reads `task_entries`).
    task_entries = traitlets.List(traitlets.Dict()).tag(sync=True)

    # Non-JS fallback, not synced b/c a pydantic model is not
    # JSON-serialisable over the comm, and the front end never reads it.
    api_resp_with_tasks = traitlets.Any()

    @classmethod
    def from_has_tasks(
        cls, obj: HasTasks, c: ImandraXClient | ImandraXAsyncClient
    ) -> Self:
        entries = collect_tasks_artifacts(obj.tasks, c)
        return cls(
            task_entries=[e.model_dump(mode='json') for e in entries],
            api_resp_with_tasks=obj,
        )

    def _repr_mimebundle_(self, **kwargs: Any) -> Any:
        if len(self.task_entries) == 0:
            return {'text/plain': xapi_to_string(self.api_resp_with_tasks)}
        else:
            return anywidget.AnyWidget._repr_mimebundle_(self, **kwargs)


class RegionDecompWidget(anywidget.AnyWidget):
    """Treemap view of a region-group forest."""

    _esm = _DIST / 'region_decomp.js'

    # Synced to JS (the `region_decomp.js` bundle reads `data`)
    data = traitlets.List().tag(sync=True)  # pyright: ignore[reportAssignmentType]

    # Non-JS fallback
    decomp_res = traitlets.Any()  # pyright: ignore[reportAssignmentType]

    @classmethod
    def from_decomp_res(cls, decomp_res: EnrichedDecomposeRes | DecomposeRes) -> Self:
        enriched = (
            decomp_res
            if isinstance(decomp_res, EnrichedDecomposeRes)
            else EnrichedDecomposeRes.from_decomp_res(decomp_res)
        )
        return cls(
            data=[v.model_dump(mode='json') for v in enriched.region_group_views()],
            decomp_res=enriched,
        )

    def _repr_mimebundle_(self, **kwargs: Any) -> Any:
        if self.decomp_res.errors:
            return {'text/plain': xapi_to_string(self.decomp_res)}
        else:
            # Only resolve to JS if there are no errors.
            return anywidget.AnyWidget._repr_mimebundle_(self, **kwargs)


class IDFWidget(anywidget.AnyWidget):
    """Two-panel graph of an iterative-decomposition (IDF) region tree."""

    _esm = _DIST / 'idf.js'

    # Synced to JS (the `idf.js` bundle reads `data` -- a serialized `View`).
    data = traitlets.Dict().tag(sync=True)  # pyright: ignore[reportAssignmentType]

    # Non-JS fallback
    view = traitlets.Any()  # pyright: ignore[reportAssignmentType]

    @classmethod
    def from_view(cls, view: IDFView) -> Self:
        return cls(data=view.model_dump(mode='json'), view=view)

    def _repr_mimebundle_(self, **kwargs: Any) -> Any:
        if not self.view.steps:
            return {'text/plain': repr(self.view)}
        else:
            return anywidget.AnyWidget._repr_mimebundle_(self, **kwargs)


# Notebook integration
# ====================

_client: ImandraXClient | ImandraXAsyncClient | None = None


def register_tasks_widget(c: ImandraXClient | ImandraXAsyncClient) -> None:
    """
    Make `EvalRes` and `CodeSnippetEvalResult` render as a `TasksWidget`.
    """
    global _client
    _client = c

    def repr_mimebundle(self: HasTasks, **kwargs: Any) -> Any:
        assert _client is not None
        widget = TasksWidget.from_has_tasks(self, _client)
        # Delegate to the widget's own hook so its text fallback still applies.
        return widget._repr_mimebundle_(**kwargs)

    setattr(EvalRes, '_repr_mimebundle_', repr_mimebundle)
    setattr(CodeSnippetEvalResult, '_repr_mimebundle_', repr_mimebundle)


def register_region_decomp_widget() -> None:
    """
    Make `EnrichedDecomposeRes` / `DecomposeRes` render as a `RegionDecompWidget`.
    """

    def repr_mimebundle(
        self: DecomposeRes | EnrichedDecomposeRes, **kwargs: Any
    ) -> Any:
        widget = RegionDecompWidget.from_decomp_res(self)
        return widget._repr_mimebundle_(**kwargs)

    setattr(EnrichedDecomposeRes, '_repr_mimebundle_', repr_mimebundle)
    setattr(DecomposeRes, '_repr_mimebundle_', repr_mimebundle)


def register_idf_widget() -> None:
    """
    Make an IDF `View` render as an `IDFWidget`.
    """

    def repr_mimebundle(self: IDFView, **kwargs: Any) -> Any:
        widget = IDFWidget.from_view(self)
        return widget._repr_mimebundle_(**kwargs)

    setattr(IDFView, '_repr_mimebundle_', repr_mimebundle)


def register_widgets(c: ImandraXClient | ImandraXAsyncClient | None) -> None:
    """
    Attach widget renderers to result types.
    """
    register_region_decomp_widget()
    register_idf_widget()

    if c is not None:
        register_tasks_widget(c)

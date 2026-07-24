# pyright: reportPrivateUsage=false, reportUnknownMemberType=false, reportUnknownVariableType=false
"""
anywidget-based rendering of ImandraX results.

Widgets are backed by JS bundles under `widget/static`:

Each widget also overrides `_repr_mimebundle_` to fall back to a `text/plain`
pretty-print when there is nothing to render (no tasks / decomposition errors).
"""
# TODO: have a more systematic way to handle failed case (api resp)

from __future__ import annotations

from pathlib import Path
from typing import Any, Self

import anywidget
import traitlets
from imandrax_api_models import DecomposeRes
from imandrax_api_models.artifacts import TasksRepr, artifact_reprs_of_tasks
from imandrax_api_models.client import ImandraXAsyncClient, ImandraXClient
from imandrax_api_models.context_utils import string_of_model as xapi_to_string
from imandrax_api_models.region_decomp import EnrichedDecomposeRes

from imandrax_tools.idf.viz_view import View as IDFView
from imandrax_tools.widget_types import HasTasks

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
        entries = artifact_reprs_of_tasks(obj.tasks, c)
        return cls(
            task_entries=[e.model_dump(mode='json') for e in entries],
            api_resp_with_tasks=obj,
        )

    @classmethod
    def from_tasks_repr(cls, obj: TasksRepr) -> Self:
        return cls(task_entries=[e.model_dump(mode='json') for e in obj.tasks])

    def _repr_mimebundle_(self, **kwargs: Any) -> Any:
        if len(self.task_entries) == 0:
            return {'text/plain': xapi_to_string(self.api_resp_with_tasks)}
        else:
            return anywidget.AnyWidget._repr_mimebundle_(self, **kwargs)


class RegionDecompWidget(anywidget.AnyWidget):
    """Treemap view of a region-group forest."""

    _esm = _DIST / 'region_decomp.js'

    # Synced to JS (the `region_decomp.js` bundle reads `data`)
    data = traitlets.List().tag(sync=True)

    # Non-JS fallback
    decomp_res = traitlets.Any()

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
    data = traitlets.Dict().tag(sync=True)

    # Non-JS fallback
    view = traitlets.Any()

    @classmethod
    def from_view(cls, view: IDFView) -> Self:
        return cls(data=view.model_dump(mode='json'), view=view)

    def _repr_mimebundle_(self, **kwargs: Any) -> Any:
        if not self.view.steps:
            return {'text/plain': repr(self.view)}
        else:
            return anywidget.AnyWidget._repr_mimebundle_(self, **kwargs)

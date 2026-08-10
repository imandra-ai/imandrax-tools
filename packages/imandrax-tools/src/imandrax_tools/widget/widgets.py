# pyright: reportPrivateUsage=false, reportUnknownMemberType=false, reportUnknownVariableType=false
"""
anywidget-based rendering of ImandraX results.

Widgets are backed by JS bundles under `widget/static`:

Some widgets carry optional `pre` / `post` YAML
panels (rendered by the same front end as `JsonableWidget`).

`IDFWidget` still uses the older `_repr_mimebundle_` text fallback.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, Self

import anywidget
import traitlets
from imandrax_api_models import Art, DecomposeRes
from imandrax_api_models.artifacts import TasksRepr, artifact_reprs_of_tasks
from imandrax_api_models.client import ImandraXAsyncClient, ImandraXClient
from imandrax_api_models.context_utils import (
    FormattableModel,
    JSONValue,
    jsonable_of_model,
)
from imandrax_api_models.region_decomp import DecomposeRes_, EnrichedDecomposeRes
from imandrax_api_models.yaml_utils import to_yaml_str

from imandrax_tools.idf.viz_view import View as IDFView
from imandrax_tools.widget_types import HasTasks

_DIST = Path(__file__).parent / 'static'


class JsonableWidget(anywidget.AnyWidget):
    """
    Collapsible, syntax-highlighted view of any JSON-able value, shown as YAML.

    The general-purpose widget: anything that can be reduced to a `JSONValue`
    (`context_utils.jsonable_of_model`, a `model_dump(mode='json')`, a plain
    dict) can be displayed.

    Python-side owns the formatting: the value is rendered by a yaml dumper
    (e.g. `yaml_utils.to_yaml_str`)

    Current behavior: The frontend recovers the fold structure from indentation.
    """

    _esm = _DIST / 'jsonable.js'

    label = traitlets.Unicode('').tag(sync=True)
    yaml_str = traitlets.Unicode().tag(sync=True)

    @classmethod
    def from_json_value(cls, v: JSONValue, label: str = '') -> Self:
        return cls(label=label, yaml_str=to_yaml_str(v))

    @classmethod
    def from_api_model(cls, model: FormattableModel, label: str = '') -> Self:
        return cls(label=label, yaml_str=to_yaml_str(jsonable_of_model(model)))

    def _repr_mimebundle_(self, **kwargs: Any) -> Any:
        return anywidget.AnyWidget._repr_mimebundle_(self, **kwargs)


class TasksWidget(anywidget.AnyWidget):
    """Collapsible view of pretty-printed artifacts for a list of tasks."""

    _esm = _DIST / 'task.js'

    task_entries = traitlets.List(
        traitlets.Dict(), allow_none=True, default_value=None
    ).tag(sync=True)
    pre = traitlets.Unicode('').tag(sync=True)
    post = traitlets.Unicode('').tag(sync=True)

    @classmethod
    def from_has_tasks(
        cls,
        obj: HasTasks,
        c: ImandraXClient | ImandraXAsyncClient,
        pre: str = '',
        post: str = '',
    ) -> Self:
        entries = artifact_reprs_of_tasks(obj.tasks, c)
        return cls(
            task_entries=[e.model_dump(mode='json') for e in entries],
            pre=pre,
            post=post,
        )

    @classmethod
    def from_tasks_repr(cls, obj: TasksRepr, pre: str = '', post: str = '') -> Self:
        return cls(
            task_entries=[e.model_dump(mode='json') for e in obj.tasks],
            pre=pre,
            post=post,
        )


class RegionDecompWidget(anywidget.AnyWidget):
    """Treemap view of a region-group forest."""

    _esm = _DIST / 'region_decomp.js'

    data = traitlets.List(traitlets.Any(), allow_none=True, default_value=None).tag(
        sync=True
    )
    pre = traitlets.Unicode('').tag(sync=True)
    post = traitlets.Unicode('').tag(sync=True)

    @classmethod
    def from_decomp_res(
        cls,
        decomp_res: EnrichedDecomposeRes | DecomposeRes,
        pre: str = '',
        post: str = '',
    ) -> Self:
        enriched = (
            decomp_res
            if isinstance(decomp_res, EnrichedDecomposeRes)
            else EnrichedDecomposeRes.from_decomp_res(decomp_res)
        )
        return cls(
            data=[v.model_dump(mode='json') for v in enriched.region_group_views()],
            pre=pre,
            post=post,
        )

    @classmethod
    def from_decomp_res_(
        cls, decomp_res: DecomposeRes_, pre: str = '', post: str = ''
    ) -> Self:
        """
        _

        Raises:
            TypeError
                If the artifact is not a list of region group views.

        """
        region_group_views = decomp_res.artifact
        match region_group_views:
            case None:
                raise TypeError('Artifact is None')
            case Art():
                raise TypeError('Regions are not parsed from artifact')
            case _:
                return cls(
                    data=[r.model_dump(mode='json') for r in region_group_views],
                    pre=pre,
                    post=post,
                )


class IDFWidget(anywidget.AnyWidget):
    """Two-panel graph of an iterative-decomposition (IDF) region tree."""

    # TODO: add the same pre and post slots. remove view

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

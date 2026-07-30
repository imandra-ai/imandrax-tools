# pyright: reportPrivateUsage=false, reportUnknownMemberType=false, reportUnknownVariableType=false

from __future__ import annotations

from typing import Any, cast

from imandrax_api_models import CodeSnippetEvalResult, DecomposeRes, EvalRes
from imandrax_api_models.client import ImandraXAsyncClient, ImandraXClient
from imandrax_api_models.context_utils import FormattableModel, jsonable_of_model
from imandrax_api_models.region_decomp import EnrichedDecomposeRes
from imandrax_api_models.yaml_utils import to_yaml_str

from imandrax_tools.idf.viz_view import View as IDFView
from imandrax_tools.widget_types import HasTasks

from .widgets import IDFWidget, RegionDecompWidget, TasksWidget

_client: ImandraXClient | ImandraXAsyncClient | None = None


def _yaml_of(model: FormattableModel) -> str:
    """The whole result as YAML, for a widget's `pre` panel."""
    return to_yaml_str(jsonable_of_model(model))


def register_tasks_widget(c: ImandraXClient | ImandraXAsyncClient) -> None:
    """
    Make `EvalRes` and `CodeSnippetEvalResult` render as a `TasksWidget`.

    With no tasks to show -- an eval that errored out -- the whole result goes
    into the widget's `pre` panel as YAML, so the failure stays visible instead
    of rendering as an empty tasks view.
    """
    global _client
    _client = c

    def repr_mimebundle(self: HasTasks, **kwargs: Any) -> Any:
        assert _client is not None
        widget = TasksWidget.from_has_tasks(self, _client)
        # Keyed off the entries rather than `self.tasks`: a task whose artifacts
        # were all excluded yields no entry, and an empty panel either way. The
        # panel is dropped (not left to say "No tasks.") because the YAML now in
        # `pre` reports the task list along with everything else.
        if not widget.task_entries:
            # `allow_none=True` on the traitlet, which the stubs do not model.
            widget.task_entries = None  # type: ignore
            widget.pre = _yaml_of(cast(FormattableModel, self))
        return widget._repr_mimebundle_(**kwargs)

    setattr(EvalRes, '_repr_mimebundle_', repr_mimebundle)
    setattr(CodeSnippetEvalResult, '_repr_mimebundle_', repr_mimebundle)


def register_region_decomp_widget() -> None:
    """
    Make `EnrichedDecomposeRes` / `DecomposeRes` render as a `RegionDecompWidget`.

    An errored decomposition has no region groups to lay out, so the whole result
    goes into the widget's `pre` panel as YAML -- what the `text/plain` fallback
    used to cover.
    """

    def repr_mimebundle(
        self: DecomposeRes | EnrichedDecomposeRes, **kwargs: Any
    ) -> Any:
        widget = RegionDecompWidget.from_decomp_res(self)
        has_regions = bool(widget.data)
        if not has_regions:
            # Nothing to lay out: drop the panel, the YAML carries the whole result.
            # `allow_none=True` on the traitlet, which the stubs do not model.
            widget.data = None  # type: ignore
        # `pre` is keyed off the errors too, not just off the panel: a result can
        # report errors *and* still lay out region groups, and those errors would
        # otherwise go unreported -- the `text/plain` fallback used to show them.
        if self.errors or not has_regions:
            widget.pre = _yaml_of(self)
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

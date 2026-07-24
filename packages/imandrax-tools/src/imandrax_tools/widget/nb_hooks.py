# pyright: reportPrivateUsage=false, reportUnknownMemberType=false, reportUnknownVariableType=false

from __future__ import annotations

from typing import Any

from imandrax_api_models import CodeSnippetEvalResult, DecomposeRes, EvalRes
from imandrax_api_models.client import ImandraXAsyncClient, ImandraXClient
from imandrax_api_models.region_decomp import EnrichedDecomposeRes

from imandrax_tools.idf.viz_view import View as IDFView
from imandrax_tools.widget_types import HasTasks

from .widgets import IDFWidget, RegionDecompWidget, TasksWidget

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

"""
anywidget-based rendering of ImandraX task artifacts.

`register_repr_html` attaches a Jupyter widget renderer to `EvalRes` and
`CodeSnippetEvalResult`, so displaying one of those objects in a notebook shows
an interactive, collapsible view of each task's decoded artifacts instead of a
raw HTML dump.
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

from ..pp.xtype import to_string as string_of_xtype

_STATIC = Path(__file__).parent / 'static'


class HasTasks(Protocol):
    tasks: list[Task]


# Data collection
# ====================


def _task_entry(task: Task, artifacts: dict[str, Any]) -> dict[str, Any]:
    """Build one JSON-serialisable task entry for the `tasks_data` trait."""
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
    """Fetch + decode + stringify artifacts for each task into trait data."""
    match c:
        case ImandraXClient():
            return [_task_entry(t, get_task_artifacts(t, c)) for t in tasks]
        case ImandraXAsyncClient():

            async def _gather() -> list[dict[str, Any]]:
                artifacts = await asyncio.gather(
                    *[async_get_task_artifacts(t, c) for t in tasks]
                )
                return [_task_entry(t, a) for t, a in zip(tasks, artifacts)]

            return asyncio.run(_gather())
        case _:
            assert_never(c)


# Widget
# ====================


class TasksWidget(anywidget.AnyWidget):
    """Collapsible view of decoded artifacts for a list of ImandraX tasks."""

    _esm = _STATIC / 'tasks.js'
    _css = _STATIC / 'tasks.css'

    tasks_data = traitlets.List().tag(sync=True)  # pyright: ignore[reportAssignmentType]


def tasks_widget(
    tasks: list[Task], c: ImandraXClient | ImandraXAsyncClient
) -> TasksWidget:
    """Build a `TasksWidget` for the given tasks."""
    return TasksWidget(tasks_data=collect_tasks_data(tasks, c))


# Notebook integration
# ====================

_client: ImandraXClient | ImandraXAsyncClient | None = None


def register_repr_html(c: ImandraXClient | ImandraXAsyncClient) -> None:
    """
    Make `EvalRes` / `CodeSnippetEvalResult` render as a `TasksWidget`.

    Sets `_repr_mimebundle_` (the ipywidgets display hook) on both classes,
    delegating to a freshly built widget so the notebook frontend renders the
    interactive view.
    """
    global _client
    _client = c

    def repr_mimebundle(self: HasTasks, include: Any = None, exclude: Any = None):
        assert _client is not None
        widget = tasks_widget(self.tasks, _client)
        return widget._repr_mimebundle_(include=include, exclude=exclude)

    setattr(EvalRes, '_repr_mimebundle_', repr_mimebundle)
    setattr(CodeSnippetEvalResult, '_repr_mimebundle_', repr_mimebundle)

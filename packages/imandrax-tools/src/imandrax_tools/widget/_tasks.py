from __future__ import annotations

import asyncio
from typing import Any, Protocol, assert_never

import imandrax_api.lib as xtype
from imandrax_api_models import Task
from imandrax_api_models.client import (
    ImandraXAsyncClient,
    ImandraXClient,
    async_get_task_artifacts,
    get_task_artifacts,
)
from imandrax_api_models.pp.xtype import (
    config_items_of_art,
    to_string as string_of_xtype,
)
from pydantic import BaseModel

STATUS_EMOJI = {
    'success': '✅',
    'error': '❌',
    'warning': '⚠️',
    'info': 'ℹ️',
    'in_progress': '🚧',
    'pending': '⏳',
    'running': '⏱️',
    'skipped': '⏭️',
    'unknown': '❓',
    'healthy': '🟢',
    'degraded': '🟡',
    'down': '🔴',
}


def status_emoji_of_art(kind: str, xval: Any) -> str | None:
    match kind, xval:
        case 'po_res', _:
            if isinstance(xval, xtype.Tasks_PO_res_shallow_poly):
                if 'success' in type(xval.res).__name__:
                    return STATUS_EMOJI['success']
                elif isinstance(xval.res, xtype.Tasks_PO_res_error_No_proof):
                    return STATUS_EMOJI['warning']
                else:
                    return STATUS_EMOJI['error']
            else:
                return STATUS_EMOJI['unknown']
        case _, _:
            return None


class HasTasks(Protocol):
    tasks: list[Task]


class ArtifactEntry(BaseModel):
    kind: str
    text: str
    icon: str | None


class TaskEntry(BaseModel):
    id: str
    kind: str
    artifacts: list[ArtifactEntry]


def _mk_task_entry(task: Task, artifacts: dict[str, Any]) -> TaskEntry:
    config_items: dict[str, Any] = {}
    for a_kind, xval in artifacts.items():
        # NOTE: simple right-win merge. Conflicts are not handled.
        config_items.update(config_items_of_art(a_kind, xval))

    return TaskEntry(
        # TODO: should we really allow empty task ids? shouldn't we raise a hard error here?
        id=task.id.id if task.id else '',
        kind=task.kind.value,
        artifacts=[
            ArtifactEntry(
                kind=a_kind,
                text=string_of_xtype(xval, **dict(config_items)),
                icon=status_emoji_of_art(a_kind, xval),
            )
            for a_kind, xval in artifacts.items()
        ],
    )


def collect_tasks_artifacts(
    tasks: list[Task], c: ImandraXClient | ImandraXAsyncClient
) -> list[TaskEntry]:
    """Fetch + decode + pretty-print artifacts for each task into trait data."""
    match c:
        case ImandraXClient():
            return [_mk_task_entry(t, get_task_artifacts(t, c)) for t in tasks]
        case ImandraXAsyncClient() as ac:

            async def _gather() -> list[TaskEntry]:
                artifacts = await asyncio.gather(
                    *[async_get_task_artifacts(t, ac) for t in tasks]
                )
                return [_mk_task_entry(t, a) for t, a in zip(tasks, artifacts)]

            return asyncio.run(_gather())
        case _:
            assert_never(c)

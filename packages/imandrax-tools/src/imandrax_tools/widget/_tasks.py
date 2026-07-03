from __future__ import annotations

import asyncio
from typing import Any, Protocol, assert_never

from imandrax_api_models import Task
from imandrax_api_models.client import (
    ImandraXAsyncClient,
    ImandraXClient,
    async_get_task_artifacts,
    get_task_artifacts,
)
from imandrax_api_models.pp.xtype import to_string as string_of_xtype
from pydantic import BaseModel


class HasTasks(Protocol):
    tasks: list[Task]


class ArtifactEntry(BaseModel):
    kind: str
    text: str


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

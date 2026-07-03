from __future__ import annotations

import asyncio
from typing import Any, Protocol, TypedDict, assert_never

from imandrax_api_models import Task
from imandrax_api_models.client import (
    ImandraXAsyncClient,
    ImandraXClient,
    async_get_task_artifacts,
    get_task_artifacts,
)
from imandrax_api_models.pp.xtype import to_string as string_of_xtype
from pydantic import TypeAdapter


class HasTasks(Protocol):
    tasks: list[Task]


class ArtifactEntry(TypedDict):
    kind: str
    pp: str


class TaskEntry(TypedDict):
    id: str
    kind: str
    artifacts: list[ArtifactEntry]


TaskEntries = TypeAdapter(list[TaskEntry])


def _mk_task_entry(task: Task, artifacts: dict[str, Any]) -> TaskEntry:
    return {
        'id': getattr(task, 'id', '') or '',
        'kind': task.kind.value,
        'artifacts': [
            {'kind': a_kind, 'pp': string_of_xtype(xval)}
            for a_kind, xval in artifacts.items()
        ],
    }


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

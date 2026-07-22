"""Widget related types, defined without optional dependencies required"""

from __future__ import annotations

import asyncio
from collections.abc import Mapping
from typing import Any, assert_never

from pydantic import BaseModel, Field

from imandrax_api_models import Task
from imandrax_api_models.client import (
    ImandraXAsyncClient,
    ImandraXClient,
    async_get_task_artifacts,
    get_task_artifacts,
)
from imandrax_api_models.pp.xtype import (
    config_items_of_art,
    to_string as xtype_to_string,
)

type XValue = Any


class ArtifactEntry(BaseModel):
    kind: str
    repr: str = Field(description='Pretty-printed imandrax_api.lib value')


class TaskEntry(BaseModel):
    id: str
    kind: str
    artifacts: list[ArtifactEntry]


def mk_task_entry(task: Task, artifacts: Mapping[str, XValue]) -> TaskEntry:
    """
    _

    Args:
        task: _
        artifacts: map from artifact kind to xvalue.

    """
    if task.id is None:
        raise ValueError('task id is missing')

    config_items: dict[str, Any] = {}
    for a_kind, xval in artifacts.items():
        # simple right-win merge. Conflicts are not handled.
        config_items.update(config_items_of_art(a_kind, xval))

    artifact_entries: list[ArtifactEntry] = []
    for a_kind, xval in artifacts.items():
        artifact_entries.append(
            ArtifactEntry(
                kind=a_kind,
                repr=xtype_to_string(xval, **config_items),
            )
        )

    return TaskEntry(
        id=task.id.id,
        kind=task.kind.value,
        artifacts=artifact_entries,
    )


def artifact_reprs_of_tasks(
    tasks: list[Task], c: ImandraXClient | ImandraXAsyncClient
) -> list[TaskEntry]:
    """Fetch + decode + pretty-print artifacts for each task into trait data."""
    match c:
        case ImandraXClient():
            return [mk_task_entry(t, get_task_artifacts(t, c)) for t in tasks]
        case ImandraXAsyncClient() as ac:

            async def _gather() -> list[TaskEntry]:
                artifacts = await asyncio.gather(
                    *[async_get_task_artifacts(t, ac) for t in tasks]
                )
                return [mk_task_entry(t, a) for t, a in zip(tasks, artifacts)]

            return asyncio.run(_gather())
        case _:
            assert_never(c)

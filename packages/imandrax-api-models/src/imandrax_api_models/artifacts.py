"""Widget related types, defined without optional dependencies required"""

from __future__ import annotations

import asyncio
from collections.abc import Mapping
from typing import (
    Any,
    assert_never,
)

import imandrax_api.lib as xtype
from pydantic import BaseModel, Field

from imandrax_api_models import (
    Task,
    TaskKind,
)
from imandrax_api_models.client import (
    ImandraXAsyncClient,
    ImandraXClient,
    async_get_task_artifacts,
    get_task_artifacts,
)
from imandrax_api_models.context_utils import (
    JSONArray,
    JSONObject,
)
from imandrax_api_models.pp.xtype import (
    Printer as XtypePrinter,
    config_items_of_art,
    to_string as xtype_to_string,
)

type XValue = Any


class ArtifactEntry(BaseModel):
    kind: str
    repr: str = Field(description='Pretty-printed imandrax_api.lib value')

    def to_json(self) -> JSONObject:
        return {self.kind: self.repr}


class TaskEntry(BaseModel):
    """Repr for one single task"""

    idx: int | None = Field(default=None)
    id: str
    kind: str
    artifacts: list[ArtifactEntry]
    other: JSONObject = Field(default_factory=dict)

    @property
    def name(self) -> str:
        buf = 'TASK_'
        if self.idx is not None:
            buf += f'{self.idx}_'
        buf += f'{self.kind.removeprefix("TASK_")}'
        return buf


class TasksRepr(BaseModel):
    """Repr of a collection of tasks and their artifacts (typically from one call)"""

    tasks: list[TaskEntry]
    other: JSONObject = Field(default_factory=dict)

    def to_json(self) -> JSONObject:
        res: JSONObject = {}
        for task in self.tasks:
            res[task.name] = [art.to_json() for art in task.artifacts]
        res |= self.other
        return res


def repr_tasks(
    arts: list[tuple[int, Task, Mapping[str, Any]]],
) -> tuple[list[TaskEntry], JSONObject]:
    """
    Transforms the artifacts to printable JSON representation.

    For PO tasks whose res is a successful proof, we aggregate their overviews
    into the same entry.

    Args:
        arts: List of (task-index, task, art-kind -> xval map)

    """
    tasks_repr: list[TaskEntry] = []
    proved_po_tasks: JSONArray = []
    for i, task, art_kind_to_xval in arts:
        if (
            task.kind == TaskKind.TASK_CHECK_PO
            and isinstance(
                (po_res := art_kind_to_xval.get('po_res', None)),
                xtype.Tasks_PO_res_shallow_poly,
            )
            and isinstance(po_res.res, xtype.Tasks_PO_res_success_Proof)  # pyright: ignore
        ):
            po_task_overview = XtypePrinter.overview_of_Tasks_PO_task_t_poly(
                art_kind_to_xval['po_task'],
                fold_sym_if_exists=True,
            )
            proved_po_tasks.append(f'({i}) {po_task_overview}')
            continue

        for a_kind, xval in art_kind_to_xval.items():
            xval_str = xtype_to_string(xval, summarize_po_task=True)
            if task.id is None:
                raise ValueError(f'Task {i} has no id')
            task_repr = TaskEntry(
                idx=i,
                id=task.id.id,
                kind=task.kind.value,
                artifacts=[ArtifactEntry(kind=a_kind, repr=xval_str)],
            )
            tasks_repr.append(task_repr)

    return tasks_repr, {'proved_po_tasks': proved_po_tasks}


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

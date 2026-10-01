"""Widget related types, defined without optional dependencies required"""

from __future__ import annotations

import asyncio
from collections.abc import Mapping
from typing import Any, Literal, Self, assert_never

import imandrax_api.lib as xtype
from pydantic import BaseModel, Field

from imandrax_api_models import Task, TaskKind
from imandrax_api_models.client import (
    ImandraXAsyncClient,
    ImandraXClient,
    async_get_task_artifacts,
    get_task_artifacts,
)
from imandrax_api_models.context_utils import (
    JSONObject,
)
from imandrax_api_models.pp.xtype import (
    to_string as xtype_to_string,
)

type XValue = Any

type ArtifactKind = Literal[
    'eval_task',
    'eval_res',
    'po_task',
    'po_res',
    'decomp_task',
    'decomp_res',
    'report',
    'show',
]
AVAILABLE_ARTIFACTS: dict[TaskKind, set[ArtifactKind]] = {
    TaskKind.TASK_EVAL: {'eval_task', 'eval_res', 'show'},
    TaskKind.TASK_CHECK_PO: {'po_task', 'po_res', 'report', 'show'},
    TaskKind.TASK_DECOMP: {'decomp_task', 'decomp_res', 'report', 'show'},
    TaskKind.TASK_PROOF_CHECK: {'show'},
}


class ArtifactEntry(BaseModel):
    kind: str
    repr: str = Field(description='Pretty-printed imandrax_api.lib value')

    def to_json(self) -> JSONObject:
        return {self.kind: self.repr}


type TaskLevel = Literal['debug', 'info', 'warning', 'error']
"""Task result attention level, derived from its result artifact

- error: task failure
- warning: an answer that isn't a plain success (refuted, bounded verification)
- info: regular success
- debug: not interesting
"""


def assess_artifacts(
    task: Task,
    artifacts: Mapping[str, XValue],
) -> tuple[TaskLevel, dict[str, Any]]:
    """
    Calculate a task's attention level and the artifact pp config

    Returns:
        - 0: the task attention level
        - 1: pp config for artifacts

    """
    pp_config: dict[str, Any] = {}
    level: TaskLevel = 'info'
    task_kind = task.kind
    match task_kind:
        case TaskKind.TASK_CHECK_PO:
            po_res: xtype.Tasks_PO_res_Shallow | None = artifacts.get('po_res')
            if po_res is None:
                return level, pp_config
            match po_res.res:
                case xtype.Tasks_PO_res_success_Proof():
                    pp_config |= {
                        'summarize_po_task': True,
                        'hide_po_res_success_cases': True,
                    }
                case (
                    xtype.Tasks_PO_res_success_Instance()
                    | xtype.Tasks_PO_res_success_Test_ok()
                ):
                    pass
                case xtype.Tasks_PO_res_error_No_proof(arg=no_proof) if (
                    no_proof.counter_model is not None
                ):
                    level = 'warning'
                case xtype.Tasks_PO_res_success_Verified_upto():
                    level = 'warning'
                case (
                    xtype.Tasks_PO_res_error_No_proof()
                    | xtype.Tasks_PO_res_error_Unsat()
                    | xtype.Tasks_PO_res_error_Invalid_model()
                    | xtype.Tasks_PO_res_error_Error()
                ):
                    level = 'error'
                case _:
                    assert_never(po_res.res)
        case TaskKind.TASK_EVAL:
            eval_res: xtype.Tasks_Eval_res | None = artifacts.get('eval_res')
            if eval_res is None:
                return level, pp_config
            match eval_res.res:
                case xtype.Error_Error_core():
                    level = 'error'
                case xtype.Tasks_Eval_res_success():
                    pass
                case _:
                    assert_never(eval_res.res)

        case TaskKind.TASK_DECOMP:
            decomp_res: xtype.Tasks_Decomp_res_Shallow | None = artifacts.get(
                'decomp_res'
            )
            if decomp_res is None:
                return level, pp_config
            match decomp_res.res:
                case xtype.Tasks_Decomp_res_error_Error():
                    level = 'error'
                case xtype.Tasks_Decomp_res_success():
                    pass
                case _:
                    assert_never(decomp_res.res)
        case _:
            pass

    return level, pp_config


class TaskEntry(BaseModel):
    """Repr for one single task"""

    id: str
    kind: str
    artifacts: list[ArtifactEntry]
    level: TaskLevel = Field(description='Task result attention level')
    from_sym: str | None = Field(
        default=None, description='Symbol the task originates from, if known'
    )
    other: JSONObject = Field(default_factory=dict)

    @property
    def name(self) -> str:
        comps = self.id.split(':')
        hash = ':'.join(comps[2:])
        return ':'.join([comps[0], comps[1], hash[:6]])

    @classmethod
    def make(cls, task: Task, artifacts: Mapping[str, XValue]) -> Self:
        """
        Create task artifact representations.

        - Encodes pp config interaction between artifacts from a task.
        - The most commont one: we'd like to summarize PO task if the PO result is a success proof.
        """
        level, pp_config = assess_artifacts(task, artifacts)

        art_entries: list[ArtifactEntry] = []
        for a_kind, xval in artifacts.items():
            xval_str = xtype_to_string(xval, **pp_config)
            art_entries.append(ArtifactEntry(kind=a_kind, repr=xval_str))

        po_task = artifacts.get('po_task')
        from_sym = (
            po_task.from_sym
            if isinstance(po_task, xtype.Tasks_PO_task_t_poly)
            else None
        )

        if task.id is None:
            raise ValueError(f'Task has no id: {task!s}')
        return cls(
            id=task.id.id,
            kind=task.kind.value,
            artifacts=art_entries,
            level=level,
            from_sym=from_sym,
        )


class TasksDataRepr(BaseModel):
    """A collection of tasks and their artifact representations"""

    tasks: list[TaskEntry]
    other: JSONObject = Field(default_factory=dict)
    """Additional metadata to be displayed"""

    @property
    def is_nil(self) -> bool:
        return len(self.tasks) == 0

    def to_json(self, skip_task_without_artifacts: bool = False) -> JSONObject:
        # (task-name -> art-kind -> art-repr) | other-info
        res: JSONObject = {}
        for task in self.tasks:
            if skip_task_without_artifacts and len(task.artifacts) == 0:
                continue
            res[task.name] = [art.to_json() for art in task.artifacts]
        res |= self.other
        return res


def artifact_reprs_of_tasks(
    tasks: list[Task], c: ImandraXClient | ImandraXAsyncClient
) -> list[TaskEntry]:
    """Fetch + decode + pretty-print artifacts for each task into trait data."""
    match c:
        case ImandraXClient():
            return [TaskEntry.make(t, get_task_artifacts(t, c)) for t in tasks]
        case ImandraXAsyncClient() as ac:

            async def _gather() -> list[TaskEntry]:
                # `async_get_task_artifacts` no longer manages the client
                # context, so hold the session open for the whole batch.
                async with ac as c_:
                    artifacts = await asyncio.gather(
                        *[async_get_task_artifacts(t, c_) for t in tasks]
                    )
                return [TaskEntry.make(t, a) for t, a in zip(tasks, artifacts)]

            return asyncio.run(_gather())
        case _:
            assert_never(c)

from enum import Enum

from pydantic import BaseModel, Field


class TaskKind(Enum):
    TASK_UNSPECIFIED = 'TASK_UNSPECIFIED'
    TASK_EVAL = 'TASK_EVAL'
    TASK_CHECK_PO = 'TASK_CHECK_PO'
    TASK_PROOF_CHECK = 'TASK_PROOF_CHECK'
    TASK_DECOMP = 'TASK_DECOMP'


class TaskID(BaseModel):
    id: str = Field(description='The task identifier')


class Task(BaseModel):
    id: TaskID | None = Field(default=None)
    kind: TaskKind


class Origin(BaseModel):
    from_sym: str = Field(description='Symbol from which the task originated')
    count: int = Field(description='A counter for tasks for this symbol')

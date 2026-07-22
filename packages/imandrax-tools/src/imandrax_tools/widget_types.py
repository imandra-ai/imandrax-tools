"""Widget related types, defined without optional dependencies required"""

from __future__ import annotations

from typing import Protocol

from imandrax_api_models import Task
from imandrax_api_models.artifacts import TaskEntry
from imandrax_api_models.region_decomp import RegionGroupView

from imandrax_tools.idf.viz_view import View as IDFView

__all__ = (
    'TaskEntry',
    'IDFView',
    'RegionGroupView',
)


class HasTasks(Protocol):
    tasks: list[Task]

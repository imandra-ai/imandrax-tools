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


def status_emoji_of_art_repr(art_kind: str, repr: str) -> str | None:
    if art_kind == 'po_res':
        if 'res=POSuccessProof' in repr:
            return STATUS_EMOJI['success']
        elif 'res=POErrorNoProof' in repr:
            return STATUS_EMOJI['warning']
        else:
            return STATUS_EMOJI['error']
    return None


class HasTasks(Protocol):
    tasks: list[Task]

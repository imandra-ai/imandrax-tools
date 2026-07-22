"""Widget related types, defined without optional dependencies required"""

from __future__ import annotations

from typing import Protocol

from imandrax_api_models import Task

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

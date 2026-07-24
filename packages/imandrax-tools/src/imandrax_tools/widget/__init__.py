"""
anywidget-based rendering of ImandraX results.
"""

from __future__ import annotations

from .nb_hooks import register_widgets
from .widgets import IDFWidget, RegionDecompWidget, TasksWidget

__all__ = (
    'TasksWidget',
    'RegionDecompWidget',
    'IDFWidget',
    'register_widgets',
)

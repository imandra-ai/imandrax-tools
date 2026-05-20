"""
ImandraX Tools - Common utilities for interacting with ImandraX / IML.

Umbrella package that bundles iml-query and imandrax-api-models.

Usage:
    from imandrax_tools.api_models import ...
    from imandrax_tools.iml_query import ...
"""

from __future__ import annotations

import os
from pathlib import Path
from typing import TYPE_CHECKING

import structlog

from . import api_models, goal_state, iml_query

if TYPE_CHECKING:
    from imandrax_api_models import Task
    from imandrax_api_models.client import ImandraXClient

__all__ = ('api_models', 'goal_state', 'iml_query')
logger = structlog.get_logger(__name__)

FAILING_PO_RES_SAVE_PATH: Path | None = (
    Path(p) if (p := os.getenv('FAILING_PO_RES_SAVE_PATH')) is not None else None
)


def get_goal_state_opt(c: ImandraXClient, task: Task) -> str | None:
    """
    Attempt to fetch the goal state from a task's po_res artifact.

    Returns the formatted goal state string, or None if unavailable.
    """
    from tempfile import NamedTemporaryFile

    from imandrax_api_models import ArtifactZip

    from imandrax_tools.goal_state import (
        GoalStateCounterModel,
        GoalStateProved,
        format_goal_state_from_zip,
    )

    art_list_res: list[str] = c.list_artifacts(task).kinds
    if 'po_res' not in art_list_res:
        return None

    po_res_art_zip: ArtifactZip | None = None
    try:
        po_res_art_zip = c.get_artifact_zip(task, kind='po_res')
        with NamedTemporaryFile(mode='w+b', suffix='.zip') as f:
            f.write(po_res_art_zip.art_zip)
            f.flush()
            goal_state_s = format_goal_state_from_zip(f.name)
        return goal_state_s
    except (GoalStateCounterModel, GoalStateProved) as e:
        logger.info(f'Goal state error: {e!r}')
        if FAILING_PO_RES_SAVE_PATH is not None and po_res_art_zip is not None:
            FAILING_PO_RES_SAVE_PATH.write_bytes(po_res_art_zip.art_zip)
        return None
    except Exception:
        raise

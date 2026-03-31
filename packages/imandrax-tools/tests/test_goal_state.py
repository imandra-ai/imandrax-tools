"""Tests for imandrax_tools.goal_state Python bindings."""

import zipfile
from pathlib import Path

from imandrax_tools import goal_state

TEST_DATA = Path(__file__).parent / 'test_data'
PO_RES_ZIP = TEST_DATA / 'po_res_art.zip'

EXPECTED = '1 subgoal -\n ⊢ List.length (x @ y) = List.length x + List.length y\n\n'


def test_format_goal_state_from_zip() -> None:
    result = goal_state.format_goal_state_from_zip(str(PO_RES_ZIP))
    assert result == EXPECTED


def test_format_goal_state_from_bytes() -> None:
    with zipfile.ZipFile(PO_RES_ZIP) as zf:
        data = zf.read('data.twine')
    result = goal_state.format_goal_state_from_bytes(data)
    assert result == EXPECTED


def test_goal_state_proved_is_exception() -> None:
    assert issubclass(goal_state.GoalStateProved, Exception)


def test_goal_state_counter_model_is_exception() -> None:
    assert issubclass(goal_state.GoalStateCounterModel, Exception)

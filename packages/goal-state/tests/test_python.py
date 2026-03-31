"""Python binding tests for goal_state."""

import pathlib

import goal_state

TEST_DATA = pathlib.Path(__file__).parent / "test_data"
PO_RES_ZIP = TEST_DATA / "po_res_art.zip"


def test_format_goal_state_from_zip():
    result = goal_state.format_goal_state_from_zip(str(PO_RES_ZIP))
    assert (
        result
        == "1 subgoal -\n ⊢ List.length (x @ y) = List.length x + List.length y\n\n"
    )


def test_format_goal_state_from_bytes():
    import zipfile

    with zipfile.ZipFile(PO_RES_ZIP) as zf:
        data = zf.read("data.twine")

    result = goal_state.format_goal_state_from_bytes(data)
    assert (
        result
        == "1 subgoal -\n ⊢ List.length (x @ y) = List.length x + List.length y\n\n"
    )


def test_goal_state_proved_exception_is_raised():
    """GoalStateProved is a subclass of Exception and can be caught."""
    assert issubclass(goal_state.GoalStateProved, Exception)


def test_goal_state_counter_model_exception_is_raised():
    assert issubclass(goal_state.GoalStateCounterModel, Exception)

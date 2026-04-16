from typing import Protocol


class GoalStateRec(Protocol):
    def __call__(self, iml: str, goal_state: str) -> str: ...

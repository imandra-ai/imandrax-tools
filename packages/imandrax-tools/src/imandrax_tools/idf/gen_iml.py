from __future__ import annotations

from dataclasses import dataclass
from typing import Final, final

INITIAL_STATE: Final[str] = 'SM.initial_state'
IS_VALID: Final[str] = 'SM.is_valid'
STEP: Final[str] = 'SM.step'
CONCRETE: Final[str] = 'TPL.concrete'


@final
@dataclass
class Guard:
    index: int
    prev_guard: Guard | None
    prev_target: Target | None
    curr_template: str

    def fun_name(self) -> str:
        return f'guard_{self.index!s}'

    def iml(self) -> str:
        if self.prev_guard is None:
            pre = 'true'
        else:
            pre = f'{self.prev_guard.fun_name()} events'

        if self.prev_target is None:
            prev_state = INITIAL_STATE
        else:
            prev_state = f'{self.prev_target.fun_name()} events'

        return f"""
let {self.fun_name()} events =
  match List.nth {self.index!s} events with
  | Some event_i when
    {CONCRETE} {self.curr_template} event_i ({prev_state}) ->
    {pre} && {IS_VALID} event_i ({prev_state})
  | _ -> false
"""


@final
@dataclass
class Target:
    index: int
    prev_target: Target | None

    def fun_name(self) -> str:
        return f'target_{self.index!s}'

    def iml(self) -> str:
        if self.prev_target is None:
            prev_state = INITIAL_STATE
        else:
            prev_state = f'{self.prev_target.fun_name()} events'
        return f"""
let {self.fun_name()} events =
  match List.nth {self.index!s} events with
  | Some event_i ->
    {STEP} event_i ({prev_state})
  | _ -> {INITIAL_STATE}
"""


def gen_decomp_funs(templates: list[str]) -> list[tuple[Guard, Target]]:
    # TODO: docstring
    decomp_funs: list[tuple[Guard, Target]] = []
    prev_guard = None
    prev_target = None
    for i, template in enumerate(templates):
        if i != 0:
            prev_guard = decomp_funs[i - 1][0]
            prev_target = decomp_funs[i - 1][1]
        guard = Guard(i, prev_guard, prev_target, template)
        target = Target(i, prev_target)
        decomp_funs.append((guard, target))
    return decomp_funs

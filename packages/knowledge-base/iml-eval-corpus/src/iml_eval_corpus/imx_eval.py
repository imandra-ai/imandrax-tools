# pyright: basic
import json
import sys
from pathlib import Path

from imandrax_api import url_prod
from imandrax_api_models import (
    EvalRes,
    TaskKind,
)
from imandrax_api_models.client import ImandraXClient, get_imandrax_api_key

# from imandrax_tools import try_get_goal_state


def _eval_src(src: str) -> tuple[EvalRes, str | None]:
    c = ImandraXClient(url=url_prod, auth_token=get_imandrax_api_key())
    eval_res = c.eval_src(src)

    check_po_tasks = [t for t in eval_res.tasks if t.kind == TaskKind.TASK_CHECK_PO]

    gs_opt: str | None = None
    if eval_res.success and check_po_tasks:
        # gs_opt = try_get_goal_state(c, check_po_tasks[0])
        gs_opt = None
    return eval_res, gs_opt


class InvalidArgs(Exception):
    pass


def main():
    usage = f"{__name__} <iml_path>"
    args = sys.argv
    if not len(args) == 2:
        raise InvalidArgs(usage)
    p = args[1]
    iml = Path(p).read_text()
    eval_res, gs_opt = _eval_src(iml)

    res = {"eval_res": eval_res.model_dump(mode="json"), "goal-state": gs_opt}
    print(json.dumps(res, indent=2))


if __name__ == "__main__":
    main()

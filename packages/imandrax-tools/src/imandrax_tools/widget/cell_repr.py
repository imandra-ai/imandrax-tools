"""Composed cell widget, rendering results for one IML snippet"""

from __future__ import annotations

import anywidget
from imandrax_api_models import Art
from imandrax_api_models.client import ImandraXClient
from imandrax_api_models.context_utils import format_eval_res, jsonable_of_model
from imandrax_api_models.proto_models.decomp import decomp_of_cst
from imandrax_api_models.region_decomp import DecomposeRes_
from imandrax_api_models.yaml_utils import to_yaml_str
from iml_query.processing import get_decomp_reqs_

from imandrax_tools.widget import JsonableWidget, RegionDecompWidget, TasksWidget


def eval_widget(
    c: ImandraXClient, iml: str
) -> tuple[TasksWidget | JsonableWidget, bool]:
    """Eval `iml` (VGs and tests included, decomps left out) -> (widget, ok)."""
    eval_res = c.eval_model(src=iml, with_vgs=True, with_tests=True)

    if len(eval_res.errors) > 0:
        return JsonableWidget.from_json_value(format_eval_res(eval_res, iml)), False
    elif len(eval_res.tasks) == 0:
        return JsonableWidget.from_json_value(format_eval_res(eval_res, iml)), True
    else:
        return TasksWidget.from_has_tasks(eval_res, c, pre=''), True


def decomp_widgets(c: ImandraXClient, iml: str) -> list[RegionDecompWidget]:
    """One widget per `[@@decomp ...]` in `iml`, in source order."""
    _rest_iml, reqs, ranges = get_decomp_reqs_(iml)

    widgets: list[RegionDecompWidget] = []

    iter = zip(reqs, ranges, strict=True)
    iter = sorted(iter, key=lambda p: p[1].start_point)

    for req, _ in iter:
        plan, name, timeout = decomp_of_cst(req)
        res = DecomposeRes_.from_decomp_res(
            c.decompose_full(d=plan, string_results=True, compute_timeout=timeout)
        )
        if res.artifact is None or isinstance(res.artifact, Art):
            pre = to_yaml_str({'decomp': name, 'decomp_res': jsonable_of_model(res)})
            widgets.append(RegionDecompWidget(pre=pre))
        else:
            widgets.append(RegionDecompWidget.from_decomp_res_(res, title=name))
    return widgets


def cell_widgets(c: ImandraXClient, iml: str) -> list[anywidget.AnyWidget]:
    wgts: list[anywidget.AnyWidget] = []
    tasks_w, ok = eval_widget(c, iml)

    wgts.append(tasks_w)

    if ok:
        wgts.extend(decomp_widgets(c, iml))

    # TODO(refa): if-else statements
    if len(wgts) >= 2 and isinstance(wgts[0], JsonableWidget):
        wgts = wgts[1:]

    return wgts

"""Tests for term evaluation and eval value pretty-printer"""

import os
import re
from collections.abc import Callable

import pytest
from imandrax_api_models.client import (
    get_imandrax_client,
    get_task_artifacts,
)
from imandrax_api_models.evaluate import evaluate
from imandrax_api_models.pp.xtype import to_string as xval_to_string
from inline_snapshot import snapshot

redact_anchor: Callable[[str], str] = lambda s: re.sub(
    r"Anchor\('[^']*'\)", r"Anchor('...')", s
)

redact_eval_stats: Callable[[str], str] = lambda s: re.sub(
    r'EvalStats\([^)]*\)', r'EvalStats(...)', s
)


@pytest.mark.vcr
def test_good():

    c = get_imandrax_client(auth_token=os.environ['IMANDRAX_API_KEY'] or None)

    iml = """
    let f x = x + 1

    let rec f_rec x = if x < 0 then 0 else x + f_rec (x - 1)

    let g x = if (x + 1 < 0) then f x else f (f x)
    """

    term = 'g 100'

    _body_eval_res, leval_res = evaluate(c, iml, term)

    leval_res.tasks
    assert len(leval_res.tasks) == 1, 'Expected 1 task'
    task_id = leval_res.tasks[0].id
    assert task_id is not None
    assert task_id.id.startswith('task:eval'), 'Expected eval task'

    art = get_task_artifacts(task=leval_res.tasks[0], c=c)
    assert set(art.keys()) == {'eval_task', 'eval_res'}

    assert redact_anchor(xval_to_string(art['eval_task'])) == snapshot(
        "EvalTask(term=Term('g 100'), anchor=Anchor('...'))"
    )
    assert redact_eval_stats(xval_to_string(art['eval_res'])) == snapshot(
        "EvalRes(res=EvalValue('102'), stats=EvalStats(...))"
    )


@pytest.mark.vcr
def test_bad_body_raises():
    c = get_imandrax_client(auth_token=os.environ['IMANDRAX_API_KEY'] or None)

    iml = """
    let f x = x + 1

    let rec f_rec x = if x < 0 then 0 else x + f_rec (x - 1)

    let g x = if (x + 1 < 0) then f x else f (f x)

    let f_bad : int -> int = fun x -> x + 1.0
    """

    term = 'g 2'

    try:
        _body_eval_res, _leval_res = evaluate(c, iml, term)
    except ValueError as e:
        assert str(e) == snapshot("""\
model body failed to load, refusing to evaluate 'g 2'
{ Kind.name = "TypeErr" }: Application failed: expected argument of type `int`
but got (1.0 : real)\
""")


@pytest.mark.vcr
def test_bad_term_becomes_true_unconditionally():
    """A upstream bug, behavior pinned here"""
    c = get_imandrax_client(auth_token=os.environ['IMANDRAX_API_KEY'] or None)

    iml = """
    let f x = x + 1

    let rec f_rec x = if x < 0 then 0 else x + f_rec (x - 1)

    let g x = if (x + 1 < 0) then f x else f (f x)
    """

    term = 'g true'

    _body_eval_res, leval_res = evaluate(c, iml, term)

    leval_res.tasks
    assert len(leval_res.tasks) == 1, 'Expected 1 task'
    task_id = leval_res.tasks[0].id
    assert task_id is not None
    assert task_id.id.startswith('task:eval'), 'Expected eval task'

    art = get_task_artifacts(task=leval_res.tasks[0], c=c)
    assert set(art.keys()) == {'eval_task', 'eval_res'}

    assert redact_anchor(xval_to_string(art['eval_task'])) == snapshot(
        "EvalTask(term=Term('true'), anchor=Anchor('...'))"
    )
    assert redact_eval_stats(xval_to_string(art['eval_res'])) == snapshot(
        "EvalRes(res=EvalValue('true'), stats=EvalStats(...))"
    )

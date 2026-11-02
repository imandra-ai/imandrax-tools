"""`[@@decomp ...]` attribute (iml-query CST) -> `decompose_full` plan."""

import pytest
from imandrax_api_models.proto_models.decomp import (
    Combine,
    Decomp,
    Enumerate,
    EnumerationStyle,
    Merge,
    Prune,
    by_name,
    decomp_of_cst,
)
from iml_query.processing import get_decomp_reqs_


def _plan_of(iml: str) -> Decomp:
    _iml, reqs, _ranges = get_decomp_reqs_(iml)
    assert len(reqs) == 1
    plan, _name, _timeout = decomp_of_cst(reqs[0])
    return plan


@pytest.mark.parametrize(
    ('payload', 'expected'),
    [
        pytest.param('top () |>> prune', Prune(d=by_name('g')), id='prune'),
        pytest.param('top () |>> combine', Combine(d=by_name('g')), id='combine'),
        pytest.param('~| (top ())', Combine(d=by_name('g')), id='prefix-combine'),
        pytest.param(
            'top ~basis:[[%id a]] () |>> enumerate (top () [%id a])',
            Enumerate(
                d_f=by_name('g', basis=['a']),
                ds=[by_name('a')],
                style=EnumerationStyle.Additive,
            ),
            id='enumerate',
        ),
        pytest.param(
            'top () |>> enumerate_all ~style:Multiplicative'
            ' [top () [%id a]; top ~prune:true () [%id b]]',
            Enumerate(
                d_f=by_name('g'),
                ds=[by_name('a'), by_name('b', prune=True)],
                style=EnumerationStyle.Multiplicative,
            ),
            id='enumerate-all',
        ),
        pytest.param(
            'top () << top () [%id a] |>> prune',
            Prune(d=Merge(d1=by_name('g'), d2=by_name('a'))),
            id='merge-then-prune',
        ),
    ],
)
def test_decomp_of_cst(payload: str, expected: Decomp):
    assert _plan_of(f'let g x = x\n[@@decomp {payload}]') == expected

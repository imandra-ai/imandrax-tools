"""
Run the existing 44 match_price cases against the slim (num) backend.

The cases live in six_swiss_js/test_js_match_price.py and are driven through
`six_swiss_js.match_price`. Rather than duplicate them, this module rebinds
that name to the slim backend and re-runs each case, so any behavioural drift
between the zarith and num preludes shows up as a failure here.
"""

import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'six_swiss_js'))

import six_swiss_slim  # type: ignore
import test_js_match_price as cases  # type: ignore

CASE_FNS = sorted(
    (name for name in dir(cases) if name.startswith('test_case_')),
    key=lambda n: int(n.rsplit('_', 1)[1]),
)


def test_all_44_cases_are_found():
    assert len(CASE_FNS) == 44, f'expected 44 cases, found {len(CASE_FNS)}'


@pytest.mark.parametrize('case_name', CASE_FNS)
def test_slim_backend_matches(case_name, monkeypatch):
    # The case functions call the `match_price` bound in their own module
    # namespace; point it at the slim backend for the duration of the call.
    monkeypatch.setattr(cases, 'match_price', six_swiss_slim.match_price)
    getattr(cases, case_name)()

"""Tests for proof obligation result markdown rendering."""

import os

import pytest
from imandrax_api import Client, url_prod
from imandrax_api.lib import (
    Anchor_Eval,
    Const_Const_bool,
    Const_Const_z,
    Sub_anchor,
    Upto_N_steps,
)
from inline_snapshot import snapshot

from imandrax_api_models import VerifyRes
from imandrax_api_models.po_res_md import (
    anchor_name,
    format_const,
    format_sub_anchor,
    format_upto,
    to_markdown,
)

# IML test code snippets
IML_CODE_SIMPLE = """\
let square (x : int) : int = x * x

let is_positive (x : int) : bool = x > 0
"""

IML_CODE_WITH_THEOREM = """\
let add (x : int) (y : int) : int = x + y

theorem add_comm (x : int) (y : int) =
    add x y = add y x
[@@auto]
"""

IML_CODE_WITH_FAILING_THEOREM = """\
let add (x : int) (y : int) : int = x + y

theorem add_wrong (x : int) (y : int) =
    add x y > x + y + 1
[@@auto]
"""

VERIFY_SRC_SIMPLE = 'fun x -> x > 0 ==> square x > 0'


@pytest.fixture
def client() -> Client:
    """Create an API client for testing."""
    api_key = os.environ.get('IMANDRAX_API_KEY')
    if not api_key:
        pytest.skip('IMANDRAX_API_KEY not set')
    return Client(url=url_prod, auth_token=api_key)


# Unit tests for formatting functions


def test_anchor_name_eval():
    """Test anchor_name with Anchor_Eval."""
    anchor = Anchor_Eval(arg=42)
    result = anchor_name(anchor)
    assert result == 'Eval 42'


def test_format_upto():
    """Test format_upto."""
    upto = Upto_N_steps(arg=100)
    result = format_upto(upto)
    assert result == '100 steps'


def test_format_sub_anchor():
    """Test format_sub_anchor."""
    sub_anchor_obj = Sub_anchor(fname='test_file.ml', anchor='test_anchor')
    result = format_sub_anchor(sub_anchor_obj)
    assert result == 'test_file.ml#test_anchor'


def test_format_const_int():
    """Test format_const with integer."""
    const = Const_Const_z(arg=42)
    result = format_const(const)
    assert result == '42'


def test_format_const_true():
    """Test format_const with true."""
    const = Const_Const_bool(arg=True)
    result = format_const(const)
    assert result == 'true'


def test_format_const_false():
    """Test format_const with false."""
    const = Const_Const_bool(arg=False)
    result = format_const(const)
    assert result == 'false'


# Integration tests with real API calls


@pytest.mark.skipif(
    not os.environ.get('IMANDRAX_API_KEY'), reason='IMANDRAX_API_KEY not set'
)
def test_verify_with_markdown_output(client: Client):
    """Test verifying code and converting PO results to markdown."""
    # Evaluate the IML code first and verify a property
    _ = client.eval_src(IML_CODE_SIMPLE)

    verify_res_msg = client.verify_src(VERIFY_SRC_SIMPLE)
    verify_res = VerifyRes.model_validate(verify_res_msg)

    # Check that we got a result (either proved, refuted, verified_upto, or error)
    assert (
        verify_res.proved is not None
        or verify_res.refuted is not None
        or verify_res.verified_upto is not None
        or verify_res.errors
    )

    # If we have proof results, convert them to markdown
    if verify_res.proved and verify_res.proved.results:
        # Test the first PO result with snapshot
        po_result = verify_res.proved.results[0]
        markdown = to_markdown(po_result.raw)
        assert markdown == snapshot()


@pytest.mark.skipif(
    not os.environ.get('IMANDRAX_API_KEY'), reason='IMANDRAX_API_KEY not set'
)
def test_verify_with_proof_markdown(client: Client):
    """Test that proved theorems show 'Proved' or QED in markdown."""
    verify_res_msg = client.verify_src(
        'let f x = x + 1 [@@program] fun x -> f x > x [@@auto]'
    )
    verify_res = VerifyRes.model_validate(verify_res_msg)

    if verify_res.proved and verify_res.proved.results:
        po_result = verify_res.proved.results[0]
        markdown = to_markdown(po_result.raw)
        assert markdown == snapshot()


# @pytest.mark.skipif(
#     not os.environ.get('IMANDRAX_API_KEY'), reason='IMANDRAX_API_KEY not set'
# )
def test_markdown_no_unicode(client: Client):
    """Test markdown rendering without unicode characters."""
    verify_res_msg = client.verify_src(
        'let f x = x + 1 [@@program] fun x -> f x >= x [@@auto]'
    )
    verify_res = VerifyRes.model_validate(verify_res_msg)

    if verify_res.proved and verify_res.proved.results:
        po_result = verify_res.proved.results[0]
        markdown_unicode = to_markdown(po_result.raw, unicode=True)
        markdown_no_unicode = to_markdown(po_result.raw, unicode=False)

        # Snapshot the unicode version
        assert markdown_unicode == snapshot()

        # Snapshot the non-unicode version
        assert markdown_no_unicode == snapshot()

        # Verify no unicode symbols in non-unicode version
        assert '✓' not in markdown_no_unicode
        assert '✗' not in markdown_no_unicode

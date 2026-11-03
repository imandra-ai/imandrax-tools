from __future__ import annotations

import re
from typing import TYPE_CHECKING

import iml_query.processing.decomp as decomp
import pytest
from iml_query.processing.decomp import (
    CompoundMerge,
    DecompParsingError,
    DecompReqArgs_,
    Top,
    apply_decomp,
    extract_decomp_reqs_,
    iml_of_decomp,
    iml_of_lazy_ret,
    iml_of_top,
    insert_decomp_req_,
    merge,
)
from iml_query.tree_sitter_utils import get_parser
from inline_snapshot import snapshot

if TYPE_CHECKING:
    import tree_sitter


class TestIMLOfTop:
    def test_iml_of_top_empty(self):
        assert iml_of_top(Top()) == snapshot('top ()')

    def test_iml_of_top_all_labels(self):
        t = Top(
            assuming='simple_branch',
            basis=['simple_branch', 'f'],
            rule_specs=['simple_branch'],
            prune=True,
            ctx_simp=True,
            lift_bool='Default',
        )
        assert iml_of_top(t) == snapshot(
            'top ~assuming:[%id simple_branch] ~basis:[[%id simple_branch]; [%id f]] ~rule_specs:[[%id simple_branch]] ~prune:true ~ctx_simp:true ~lift_bool:Default ()'
        )

    def test_iml_of_top_only_prune(self):
        assert iml_of_top(Top(prune=True)) == snapshot('top ~prune:true ()')

    def test_iml_of_top_ctx_simp_false(self):
        assert iml_of_top(Top(ctx_simp=False)) == snapshot('top ~ctx_simp:false ()')

    def test_iml_of_top_empty_lists(self):
        # Empty basis / rule_specs should not produce empty `[]` labels
        assert iml_of_top(Top(basis=[], rule_specs=[])) == snapshot(
            'top ~basis:[] ~rule_specs:[] ()'
        )


# iml_of_decomp / iml_of_lazy_ret
# ====================


def test_iml_of_decomp_top():
    assert iml_of_decomp(Top(prune=True)) == snapshot('top ~prune:true ()')


def test_iml_of_lazy_ret():
    lr = apply_decomp(Top(prune=True), 'foo')
    assert iml_of_lazy_ret(lr) == snapshot('top ~prune:true () [%id foo]')


def test_iml_of_decomp_merge():
    lhs = Top(prune=True)
    rhs = apply_decomp(Top(ctx_simp=True), 'bar')
    d = merge(lhs, rhs)
    assert iml_of_decomp(d) == snapshot(
        'top ~prune:true () << top ~ctx_simp:true () [%id bar]'
    )


def test_iml_of_decomp_nested_merge():
    inner = merge(Top(prune=True), apply_decomp(Top(ctx_simp=True), 'a'))
    outer = merge(inner, apply_decomp(Top(), 'b'))
    assert iml_of_decomp(outer) == snapshot(
        'top ~prune:true () << top ~ctx_simp:true () [%id a] << top () [%id b]'
    )


class TestInsertDecompReq_:
    @staticmethod
    def _parse(iml: str):
        parser = get_parser()
        return parser.parse(bytes(iml, encoding='utf8'))

    def test_insert_decomp_req_new_top(self):
        iml = """\
let simple_branch x =
if x = 1 || x = 2 then x + 1 else x - 1
"""
        tree = self._parse(iml)
        req = DecompReqArgs_(name='simple_branch', decomp=Top(prune=True))
        new_iml, _ = insert_decomp_req_(iml, tree, req)
        assert new_iml == snapshot("""\
let simple_branch x =
if x = 1 || x = 2 then x + 1 else x - 1
[@@decomp top ~prune:true ()]
""")

    def test_insert_decomp_req_new_merge(self):
        iml = """\
let foo x = x + 1
"""
        tree = self._parse(iml)
        d = merge(Top(prune=True), apply_decomp(Top(ctx_simp=True), 'bar'))
        req = DecompReqArgs_(name='foo', decomp=d)
        new_iml, _ = insert_decomp_req_(iml, tree, req)
        assert new_iml == snapshot("""\
let foo x = x + 1
[@@decomp top ~prune:true () << top ~ctx_simp:true () [%id bar]]
""")

    def test_insert_decomp_req_new_missing_function_raises(self):
        iml = 'let foo x = x + 1\n'
        tree = self._parse(iml)
        req = DecompReqArgs_(name='does_not_exist', decomp=Top())
        try:
            insert_decomp_req_(iml, tree, req)
        except ValueError as e:
            assert 'does_not_exist' in str(e)
        else:
            raise AssertionError('expected ValueError')


class TestTimeoutAttr:
    """
    `[@@timeout n]` alongside `[@@decomp ...]` on the same binding.

    In the whole-file path that one attribute budgets both the decomp and the
    binding's POs, so extraction lifts it into the request and leaves it in
    the source, keeping the extracted and whole-file paths equivalent.
    """

    @staticmethod
    def _parse(iml: str) -> tree_sitter.Tree:
        parser = get_parser()
        return parser.parse(bytes(iml, encoding='utf8'))

    def test_extract_timeout_after_decomp(self):
        iml = """\
let f x = if x > 0 then (-1) else 1
[@@decomp top ()]
[@@timeout 120]"""
        leftover, _tree, reqs, _ranges = extract_decomp_reqs_(iml, self._parse(iml))
        assert reqs == [{'name': 'f', 'decomp': Top(), 'timeout': 120}]
        assert leftover == snapshot(
            'let f x = if x > 0 then (-1) else 1\n\n[@@timeout 120]'
        )

    def test_extract_timeout_before_decomp(self):
        """ImandraX ignores attribute order, so extraction must too."""
        iml = """\
let f x = if x > 0 then (-1) else 1
[@@timeout 120]
[@@decomp top ()]"""
        leftover, _tree, reqs, _ranges = extract_decomp_reqs_(iml, self._parse(iml))
        assert reqs == [{'name': 'f', 'decomp': Top(), 'timeout': 120}]
        assert leftover == snapshot(
            'let f x = if x > 0 then (-1) else 1\n[@@timeout 120]\n'
        )

    def test_extract_no_timeout(self):
        iml = """\
let f x = if x > 0 then (-1) else 1
[@@decomp top ()]"""
        _leftover, _tree, reqs, _ranges = extract_decomp_reqs_(iml, self._parse(iml))
        assert reqs == [{'name': 'f', 'decomp': Top()}]

    def test_extract_timeout_with_labels(self):
        iml = """\
let f x = if x > 0 then (-1) else 1
[@@decomp top ~prune:true ~ctx_simp:false ()]
[@@timeout 7]"""
        _leftover, _tree, reqs, _ranges = extract_decomp_reqs_(iml, self._parse(iml))
        assert reqs == [
            {
                'name': 'f',
                'decomp': Top(prune=True, ctx_simp=False),
                'timeout': 7,
            }
        ]

    def test_timeout_on_other_binding_is_not_picked_up(self):
        """A `[@@timeout]` is scoped to its own binding, nothing else."""
        iml = """\
let rec g y = y
[@@timeout 9]
let f x = if x > 0 then (-1) else 1
[@@decomp top ()]"""
        _leftover, _tree, reqs, _ranges = extract_decomp_reqs_(iml, self._parse(iml))
        assert reqs == [{'name': 'f', 'decomp': Top()}]

    def test_insert_always_emits_timeout(self):
        iml = 'let foo x = x + 1\n'
        req = DecompReqArgs_(name='foo', decomp=Top(prune=True), timeout=30)
        new_iml, _ = insert_decomp_req_(iml, self._parse(iml), req)
        assert new_iml == snapshot("""\
let foo x = x + 1
[@@decomp top ~prune:true ()]
[@@timeout 30]
""")

    def test_insert_omits_timeout_when_absent(self):
        iml = 'let foo x = x + 1\n'
        req = DecompReqArgs_(name='foo', decomp=Top(prune=True))
        new_iml, _ = insert_decomp_req_(iml, self._parse(iml), req)
        assert new_iml == snapshot("""\
let foo x = x + 1
[@@decomp top ~prune:true ()]
""")

    def test_insert_emits_timeout_for_composed_decomp(self):
        """The timeout rides on the binding, not on any one decomp term."""
        iml = 'let foo x = x + 1\n'
        d = merge(Top(prune=True), apply_decomp(Top(ctx_simp=True), 'bar'))
        req = DecompReqArgs_(name='foo', decomp=d, timeout=45)
        new_iml, _ = insert_decomp_req_(iml, self._parse(iml), req)
        assert new_iml == snapshot("""\
let foo x = x + 1
[@@decomp top ~prune:true () << top ~ctx_simp:true () [%id bar]]
[@@timeout 45]
""")


class TestExtractDecompReq_:
    """
    Composite extraction: `extract_decomp_reqs_` understands `<<` and `<|<`.

    The old `extract_decomp_reqs` raises `NotImplementedError` on these.
    """

    @staticmethod
    def _parse(iml: str) -> tree_sitter.Tree:
        parser = get_parser()
        return parser.parse(bytes(iml, encoding='utf8'))

    def _extract_one(self, iml: str) -> DecompReqArgs_:
        _leftover, _tree, reqs, _ranges = extract_decomp_reqs_(iml, self._parse(iml))
        assert len(reqs) == 1
        return reqs[0]

    def test_extract_bare_top(self):
        req = self._extract_one('let f x = x\n[@@decomp top ~prune:true ()]')
        assert req == {'name': 'f', 'decomp': Top(prune=True)}

    def test_extract_merge(self):
        req = self._extract_one(
            'let f x = x\n'
            '[@@decomp top ~prune:true () << top ~ctx_simp:true () [%id bar]]'
        )
        assert req == {
            'name': 'f',
            'decomp': merge(Top(prune=True), apply_decomp(Top(ctx_simp=True), 'bar')),
        }

    def test_extract_nested_merge_is_left_associative(self):
        req = self._extract_one(
            'let f x = x\n'
            '[@@decomp top ~prune:true () << top ~ctx_simp:true () [%id a]'
            ' << top () [%id b]]'
        )
        inner = merge(Top(prune=True), apply_decomp(Top(ctx_simp=True), 'a'))
        assert req == {
            'name': 'f',
            'decomp': merge(inner, apply_decomp(Top(), 'b')),
        }

    def test_extract_compound_merge(self):
        req = self._extract_one('let f x = x\n[@@decomp top () <|< top () [%id c]]')
        assert req == {
            'name': 'f',
            'decomp': CompoundMerge(m=Top(), d1=apply_decomp(Top(), 'c')),
        }

    def test_extract_does_not_confuse_basis_ids_with_merge_id(self):
        """`~basis:[[%id g]]` also holds `[%id ...]`, but not the merge one."""
        req = self._extract_one(
            'let f x = x\n'
            '[@@decomp top ~assuming:[%id g] ~basis:[[%id g]; [%id h]] ()'
            ' << top () [%id d]]'
        )
        assert req == {
            'name': 'f',
            'decomp': merge(
                Top(assuming='g', basis=['g', 'h']),
                apply_decomp(Top(), 'd'),
            ),
        }

    def test_extract_keeps_module_path_of_basis_and_rule_specs(self):
        """`[%id Lib.g]` is kept as `Lib.g`, not truncated to `g`."""
        req = self._extract_one(
            'let f x = x\n'
            '[@@decomp top ~basis:[[%id Lib.g]; [%id A.B.h]; [%id k]] ~rule_specs:[[%id Lib.r]] ()]'
        )
        assert req == {
            'name': 'f',
            'decomp': Top(basis=['Lib.g', 'A.B.h', 'k'], rule_specs=['Lib.r']),
        }

    def test_extract_with_timeout(self):
        req = self._extract_one(
            'let f x = x\n[@@decomp top () << top () [%id bar]]\n[@@timeout 60]'
        )
        assert req == {
            'name': 'f',
            'decomp': merge(Top(), apply_decomp(Top(), 'bar')),
            'timeout': 60,
        }

    @pytest.mark.parametrize(
        ('iml', 'expected_msg'),
        [
            pytest.param(
                'let f x = x\n[@@decomp top () << top ()]',
                'must be applied to an `[%id ...]`',
                id='rhs-missing-id',
            ),
            pytest.param(
                'let f x = x\n[@@decomp top () [%id z]]',
                'not a decomp result',
                id='payload-already-applied',
            ),
            pytest.param(
                'let f x = x\n[@@decomp top () >>> top () [%id z]]',
                'unsupported decomp operator',
                id='unsupported-operator',
            ),
            pytest.param(
                'let f x = x\n[@@decomp top () |>> frobnicate]',
                'unsupported refiner',
                id='unknown-refiner',
            ),
            pytest.param(
                'let f x = x\n[@@decomp top () |>> enumerate_all [top () [%id a]]]',
                'needs `~style',
                id='enumerate-all-missing-style',
            ),
            pytest.param(
                'let f x = x\n[@@decomp top () |>> enumerate_all ~style:Both [top () [%id a]]]',
                '`Additive` or `Multiplicative`',
                id='enumerate-all-bad-style',
            ),
            pytest.param(
                'let f x = x\n[@@decomp top () |>> enumerate_all'
                ' ~style:Additive ~style:Multiplicative [top () [%id a]]]',
                'takes `~style` once',
                id='enumerate-all-repeated-style',
            ),
            pytest.param(
                'let f x = x\n[@@decomp top () |>> enumerate_all ~style [top () [%id a]]]',
                '`Additive` or `Multiplicative`',
                id='enumerate-all-punned-style',
            ),
            pytest.param(
                'let f x = x\n[@@decomp top () |>> enumerate (top ())]',
                'must be applied to an `[%id ...]`',
                id='enumerate-missing-id',
            ),
            pytest.param(
                'let f x = x\n[@@decomp prune ()]',
                'expected `top',
                id='payload-not-top',
            ),
        ],
    )
    def test_extract_rejects_malformed_payload(self, iml: str, expected_msg: str):
        with pytest.raises(DecompParsingError, match=re.escape(expected_msg)):
            extract_decomp_reqs_(iml, self._parse(iml))


class TestDecompReqRoundTrip_:
    """`extract_decomp_reqs_` -> `insert_decomp_req_` preserves the decomp."""

    @staticmethod
    def _parse(iml: str) -> tree_sitter.Tree:
        parser = get_parser()
        return parser.parse(bytes(iml, encoding='utf8'))

    @pytest.mark.parametrize(
        'attr',
        [
            '[@@decomp top ()]',
            '[@@decomp top ~prune:true ()]',
            '[@@decomp top ~prune:true () << top ~ctx_simp:true () [%id bar]]',
            '[@@decomp top () << top () [%id a] << top () [%id b]]',
            '[@@decomp top () <|< top () [%id c]]',
            '[@@decomp top ~assuming:[%id g] ~basis:[[%id g]; [%id h]] ()]',
            '[@@decomp top () |>> prune]',
            '[@@decomp top () |>> combine]',
            '[@@decomp ~| (top ~basis:[[%id a]] ())]',
            '[@@decomp top () |>> enumerate (top () [%id a])]',
            '[@@decomp top () |>> enumerate_all ~style:Multiplicative [top () [%id a]; top () [%id b]]]',
            '[@@decomp ~| (top () << top () [%id a]) |>> prune]',
            '[@@decomp ~| (top ()) << top () [%id a]]',
        ],
    )
    def test_round_trip_preserves_attr(self, attr: str):
        iml = f'let f x = x\n{attr}\n'
        leftover, _tree, reqs, _ranges = extract_decomp_reqs_(iml, self._parse(iml))
        rebuilt, _ = insert_decomp_req_(leftover, self._parse(leftover), reqs[0])
        assert rebuilt.strip() == iml.strip()


class TestExtractRefiners:
    """`|>>` refiners (`prune`, `combine`, `enumerate`, `enumerate_all`) and `~|`."""

    @staticmethod
    def _parse(iml: str) -> tree_sitter.Tree:
        parser = get_parser()
        return parser.parse(bytes(iml, encoding='utf8'))

    def _decomp_of(self, payload: str) -> decomp.Decomp:
        iml = f'let f x = x\n[@@decomp {payload}]'
        _leftover, _tree, reqs, _ranges = extract_decomp_reqs_(iml, self._parse(iml))
        assert len(reqs) == 1
        return reqs[0]['decomp']

    def test_prune(self):
        assert self._decomp_of('top () |>> prune') == decomp.Prune(m=Top())

    def test_combine_infix(self):
        assert self._decomp_of('top () |>> combine') == decomp.Combine(m=Top())

    def test_combine_prefix(self):
        assert self._decomp_of('~| (top ~basis:[[%id g]] ())') == decomp.Combine(
            m=Top(basis=['g']), prefix=True
        )

    def test_enumerate(self):
        assert self._decomp_of(
            'top ~basis:[[%id a]] () |>> enumerate (top () [%id a])'
        ) == decomp.Enumerate(
            m=Top(basis=['a']), ds=[apply_decomp(Top(), 'a')], style=None
        )

    def test_enumerate_all(self):
        assert self._decomp_of(
            'top () |>> enumerate_all ~style:Additive'
            ' [top () [%id a]; top ~prune:true () [%id b]]'
        ) == decomp.Enumerate(
            m=Top(),
            ds=[apply_decomp(Top(), 'a'), apply_decomp(Top(prune=True), 'b')],
            style='Additive',
        )

    @pytest.mark.parametrize(
        'style',
        ['(Multiplicative)', 'Decomp.Multiplicative', '((Decomp.Multiplicative))'],
    )
    def test_enumerate_all_style_spellings(self, style: str):
        assert self._decomp_of(
            f'top () |>> enumerate_all ~style:{style} [top () [%id a]]'
        ) == decomp.Enumerate(
            m=Top(), ds=[apply_decomp(Top(), 'a')], style='Multiplicative'
        )

    def test_refiners_are_left_associative_with_merge(self):
        assert self._decomp_of(
            'top () << top () [%id g] |>> prune |>> combine'
        ) == decomp.Combine(m=decomp.Prune(m=merge(Top(), apply_decomp(Top(), 'g'))))

    def test_merge_after_prefix_combine(self):
        assert self._decomp_of('~| (top ()) << top () [%id g]') == merge(
            decomp.Combine(m=Top(), prefix=True), apply_decomp(Top(), 'g')
        )


class TestComments:
    """A comment anywhere in a decomp payload parses as if it were not there."""

    @staticmethod
    def _decomp_of(payload: str) -> decomp.Decomp:
        iml = f'let f x = x\n[@@decomp {payload}]'
        _leftover, _tree, reqs, _ranges = extract_decomp_reqs_(
            iml, get_parser().parse(bytes(iml, encoding='utf8'))
        )
        assert len(reqs) == 1
        return reqs[0]['decomp']

    @pytest.mark.parametrize(
        'payload',
        [
            pytest.param('(* c *) top () << top () [%id a] |>> prune', id='start'),
            pytest.param('top () << top () [%id a] |>> prune (* c *)', id='end'),
            pytest.param('top () (* c *) << top () [%id a] |>> prune', id='before-op'),
            pytest.param('top () << (* c *) top () [%id a] |>> prune', id='after-op'),
            pytest.param('top () << top () (* c *) [%id a] |>> prune', id='before-id'),
            pytest.param('(top () << top () [%id a]) |>> prune', id='no-comment'),
            pytest.param('( (* c *) top () << top () [%id a]) |>> prune', id='paren'),
            pytest.param(
                '(top () << top () [%id a] (* c *)) |>> prune', id='paren-end'
            ),
        ],
    )
    def test_merge_then_prune(self, payload: str):
        assert self._decomp_of(payload) == decomp.Prune(
            m=merge(Top(), apply_decomp(Top(), 'a'))
        )

    def test_prefix_combine(self):
        assert self._decomp_of('~| (* c *) (top ())') == decomp.Combine(
            m=Top(), prefix=True
        )

    def test_enumerate(self):
        assert self._decomp_of(
            'top () |>> enumerate (* c *) (top () [%id a])'
        ) == decomp.Enumerate(m=Top(), ds=[apply_decomp(Top(), 'a')])

    @pytest.mark.parametrize(
        'payload',
        [
            pytest.param(
                'top () |>> enumerate_all (* c *) ~style:Additive [top () [%id a]; top () [%id b]]',
                id='before-style',
            ),
            pytest.param(
                'top () |>> enumerate_all ~style:(* c *)Additive [top () [%id a]; top () [%id b]]',
                id='in-style',
            ),
            pytest.param(
                'top () |>> enumerate_all ~style:Additive (* c *) [top () [%id a]; top () [%id b]]',
                id='before-list',
            ),
            pytest.param(
                'top () |>> enumerate_all ~style:Additive [(* c *) top () [%id a]; top () [%id b]]',
                id='list-start',
            ),
            pytest.param(
                'top () |>> enumerate_all ~style:Additive [top () [%id a]; (* c *) top () [%id b]]',
                id='between-items',
            ),
            pytest.param(
                'top () |>> enumerate_all ~style:Additive [top () [%id a]; top () [%id b] (* c *)]',
                id='list-end',
            ),
        ],
    )
    def test_enumerate_all(self, payload: str):
        assert self._decomp_of(payload) == decomp.Enumerate(
            m=Top(),
            ds=[apply_decomp(Top(), 'a'), apply_decomp(Top(), 'b')],
            style='Additive',
        )

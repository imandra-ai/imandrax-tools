from inline_snapshot import snapshot

from iml_query.processing.decomp import (
    DecompReqArgs_,
    Top,
    apply_decomp,
    iml_of_decomp,
    iml_of_lazy_ret,
    iml_of_top,
    insert_decomp_req_,
    merge,
)
from iml_query.tree_sitter_utils import get_parser


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
        assert iml_of_top(Top(ctx_simp=False)) == snapshot(
            'top ~ctx_simp:false ()'
        )

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

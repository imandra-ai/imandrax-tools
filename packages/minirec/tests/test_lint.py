# ruff: noqa: RUF100, F401
from inline_snapshot import snapshot

from minirec.common import Loc, NestedMeasureDiag, NestedRecursiveFunctionDiag
from minirec.cst import parse_iml
from minirec.db import IML_NESTED_MEASURE, IML_NESTED_REC
from minirec.err_rec import check_infix_op_missing_paren


def test_nested_measure():
    _, diags, _, _ = parse_iml(IML_NESTED_MEASURE)
    assert diags == snapshot(
        [
            NestedRecursiveFunctionDiag(
                loc=Loc(
                    start_byte=106, end_byte=443, start_point=(3, 2), end_point=(12, 41)
                ),
                function_name='helper',
                top_function_name='build_fib',
                nesting_level=2,
            ),
            NestedMeasureDiag(
                loc=Loc(
                    start_byte=106, end_byte=443, start_point=(3, 2), end_point=(12, 41)
                ),
                function_name='helper',
                measure='[@@measure Ordinal.of_int (n - curr_i)]',
                top_function_name='build_fib',
                nesting_level=2,
            ),
        ]
    )


def test_nested_rec():
    _, diags, _, _ = parse_iml(IML_NESTED_REC)
    assert diags == snapshot(
        [
            NestedRecursiveFunctionDiag(
                loc=Loc(
                    start_byte=106, end_byte=401, start_point=(3, 2), end_point=(11, 19)
                ),
                function_name='helper',
                top_function_name='build_fib',
                nesting_level=2,
            )
        ]
    )

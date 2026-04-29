from imandrax_api_models import Error, ErrorMessage, EvalRes, Location, Position
from inline_snapshot import snapshot

from imandrax_tools.iml_eval_corpus.common import Loc
from imandrax_tools.iml_eval_corpus.corpus import check_all
from imandrax_tools.iml_eval_corpus.corpus.infix_op_missing_paren.query import (
    InfixOpMissingParenDiag,
)
from imandrax_tools.iml_eval_corpus.corpus.nested_measure.query import NestedMeasureDiag
from imandrax_tools.iml_eval_corpus.corpus.nested_rec.query import (
    NestedRecursiveFunctionDiag,
)
from imandrax_tools.iml_eval_corpus.format import format_diagnostics

IML = """\
let build_fib (f : int list) (i : int) (n : int) : int list =
  let non_rec_irrelevant_f x = x + 1
  in
  let rec helper curr_f curr_i =
    if curr_i > n then
      curr_f
    else
      match (List.nth (curr_i - 1) curr_f, List.nth (curr_i - 2) curr_f) with
      | (Some prev1, Some prev2) ->
          let new_f = curr_f @ [prev1 + prev2] in
          helper new_f (curr_i + 1)
      | _ -> curr_f
  [@@measure Ordinal.of_int (n - curr_i)]
  in
  helper f i

let land : int -> int -> int = fun (x : int) (y : int) -> 0 [@@opaque]
"""
"""A piece of IML that contains multiple diagnostics"""


# def test_get_eval_res():
#     from imandrax_api_models.client import get_imandrax_client

#     c = get_imandrax_client()
#     eval_res = c.eval_src(src=IML)
#     assert eval_res == snapshot()


EVAL_RES = EvalRes(
    success=False,
    errors=[
        Error(
            msg=ErrorMessage(
                msg='syntax error',
                locs=[
                    Location(
                        file='<none>',
                        start=Position(line=17, col=5),
                        stop=Position(line=17, col=8),
                    )
                ],
                backtrace="""\
Raised at Imandrax_ocaml_parse_base__Imandrax_parse.Make.wrap in file "src/ocaml-parse/base/imandrax_parse.ml", line 80, characters 4-49
Called from Imandrax_ocaml_parse_base__Imandrax_parse.Make.wrap_and_rw.(fun) in file "src/ocaml-parse/base/imandrax_parse.ml", line 142, characters 6-32
""",
            ),
            kind='{ Kind.name = "SyntaxErr" }',
            process='imandrax-server',
        )
    ],
)


def test_multidiags():
    diags = check_all(IML, EVAL_RES)
    assert diags == snapshot(
        [
            InfixOpMissingParenDiag(
                loc=Loc(
                    start_byte=467,
                    end_byte=473,
                    start_point=(17, 5),
                    end_point=(17, 11),
                ),
                op='land :',
            ),
            NestedMeasureDiag(
                loc=Loc(
                    start_byte=106, end_byte=443, start_point=(4, 3), end_point=(13, 42)
                ),
                function_name='helper',
                measure='[@@measure Ordinal.of_int (n - curr_i)]',
                top_function_name='build_fib',
                nesting_level=2,
            ),
            NestedRecursiveFunctionDiag(
                loc=Loc(
                    start_byte=106, end_byte=443, start_point=(4, 3), end_point=(13, 42)
                ),
                function_name='helper',
                top_function_name='build_fib',
                nesting_level=2,
            ),
        ]
    )
    assert len(diags) == 3
    s = format_diagnostics(diags, IML)
    assert s == snapshot("""\
1. infix-op-missing-paren(error): Infix operator missing parenthesis
->loc:17:5-17:11
15 |   helper f i
16 | \n\
17 | let land : int -> int -> int = fun (x : int) (y : int) -> 0 [@@opaque]
   |     ^^^^^^
18 | \n\
help: `land :` is an infix operator. When used in let-binding, it needs to be enclosed in parentheses.
E.g. `let ( land ) = <new-definition>`

2. nested-measure-attribute(warning): Nested measure attribute
->loc:4:3-13:42
 2 |     let non_rec_irrelevant_f x = x + 1
 3 |     in
 4 | /   let rec helper curr_f curr_i =
 5 | |     if curr_i > n then
 6 | |       curr_f
 7 | |     else
 8 | |       match (List.nth (curr_i - 1) curr_f, List.nth (curr_i - 2) curr_f) with
 9 | |       | (Some prev1, Some prev2) ->
10 | |           let new_f = curr_f @ [prev1 + prev2] in
11 | |           helper new_f (curr_i + 1)
12 | |       | _ -> curr_f
13 | |   [@@measure Ordinal.of_int (n - curr_i)]
   | |_________________________________________^
14 |     in
15 |     helper f i
help: Measure attribute `[@@measure Ordinal.of_int (n - curr_i)]` should be attached to a top-level function instead of nested function `helper`.

3. nested-recursive-function(warning): Nested recursive function
->loc:4:3-13:42
 2 |     let non_rec_irrelevant_f x = x + 1
 3 |     in
 4 | /   let rec helper curr_f curr_i =
 5 | |     if curr_i > n then
 6 | |       curr_f
 7 | |     else
 8 | |       match (List.nth (curr_i - 1) curr_f, List.nth (curr_i - 2) curr_f) with
 9 | |       | (Some prev1, Some prev2) ->
10 | |           let new_f = curr_f @ [prev1 + prev2] in
11 | |           helper new_f (curr_i + 1)
12 | |       | _ -> curr_f
13 | |   [@@measure Ordinal.of_int (n - curr_i)]
   | |_________________________________________^
14 |     in
15 |     helper f i
help: Recursive function `helper` nested in `build_fib` might cause proof-obligation difficulty.

""")

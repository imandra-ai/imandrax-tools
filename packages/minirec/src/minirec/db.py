"""
A database of IML source code and eval result that is cached with snapshot + env trick.

Set UNTRUSTING=1 and run pytest to update the snapshot cache.

"""

from __future__ import annotations

import os

from imandrax_api import url_prod
from imandrax_api_models import Error, ErrorMessage, EvalRes, Location, Position
from imandrax_api_models.client import ImandraXClient, get_imandrax_api_key
from inline_snapshot import get_snapshot_value, snapshot

UNTRUSTING = os.environ.get('UNTRUSTING') in ['1', 'true', 'True']


def _eval_src(src: str) -> EvalRes:
    c = ImandraXClient(url=url_prod, auth_token=get_imandrax_api_key())
    return c.eval_src(src)


def test() -> EvalRes:
    iml = IML_INFIX_OP_MISSING_PAREN

    ss: EvalRes = snapshot()
    if UNTRUSTING:
        assert ss == _eval_src(iml)

    return get_snapshot_value(ss)


# ====================

IML_NESTED_MEASURE = """\
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
  helper f i"""

IML_NESTED_REC = """\
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
  in
  helper f i"""


# ====================


IML_INFIX_OP_MISSING_PAREN = """\
let land : int -> int -> int = fun (x : int) (y : int) -> 0 [@@opaque]"""


EVAL_RES_INFIX_OP_MISSING_PAREN = EvalRes(
    success=False,
    errors=[
        Error(
            msg=ErrorMessage(
                msg='syntax error',
                locs=[
                    Location(
                        file='<none>',
                        start=Position(line=1, col=5),
                        stop=Position(line=1, col=8),
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

# ====================

"""
A database of IML source code and eval result that is cached with snapshot + env trick.

Set UNTRUSTING=1 and run pytest to update the snapshot cache.

"""

from __future__ import annotations

import os

from imandrax_api import url_prod
from imandrax_api_models import (
    Error,
    ErrorMessage,
    EvalRes,
    Location,
    Position,
    Task,
    TaskID,
    TaskKind,
)
from imandrax_api_models.client import ImandraXClient, get_imandrax_api_key
from imandrax_tools import try_get_goal_state
from inline_snapshot import get_snapshot_value, snapshot

UNTRUSTING = os.environ.get('UNTRUSTING') in ['1', 'true', 'True']


def _eval_src(src: str) -> tuple[EvalRes, str | None]:
    c = ImandraXClient(url=url_prod, auth_token=get_imandrax_api_key())
    eval_res = c.eval_src(src)

    check_po_tasks = [t for t in eval_res.tasks if t.kind == TaskKind.TASK_CHECK_PO]

    gs_opt: str | None = None
    if eval_res.success and check_po_tasks:
        gs_opt = try_get_goal_state(c, check_po_tasks[0])
    return eval_res, gs_opt


def test() -> EvalRes:
    iml = IML_HAS_GOAL_STATE

    ss: EvalRes = snapshot()
    if UNTRUSTING:
        eval_res, _gs_opt = _eval_src(iml)
        assert ss == eval_res

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

IML_DECOMP_ASSUMING_SIG_MISMATCH = """
let precond_wrong x = x >. (1.0 /. 2.0)

let f_wrong x = if x > 0 then (if x > 1 then 1 else 0) else (if x > -1 then -1 else 0)
[@@decomp top ~assuming:[%id precond_wrong] ()]"""

IML_DECOMP_ASSUMING_SIG_MISMATCH_NON_EXAMPLE = """
let precond x = x > (1 / 2)

let f x = if x > 0 then (if x > 1 then 1 else 0) else (if x > -1 then -1 else 0)
[@@decomp top ~assuming:[%id precond] ()]"""

# eval_model_res should be good (sucess=True)
# no po_results, eval is true, has decomp task
# trace keyword: Inject_asm, Imandrax_decomp__Strategy,
# tasks has decomp
EVAL_RES_DECOMP_ASSUMING_SIG_MISMATCH = EvalRes(
    success=True,
    messages=[
        """\
Error{ Kind.name = "TacticEvalErr" }:
  File "src/decomp/strategy.ml", line 428, characters 8-14: Assertion failed
  backtrace:
  Raised at Imandrax_decomp__Strategy.Inject_asm.mk_asm_sko_d in file "src/decomp/strategy.ml", lines 428-430, characters 8-56
  Called from Imandrax_decomp__Strategy.Inject_asm.top in file "src/decomp/strategy.ml", line 481, characters 38-63
  Called from Imandrax_decomp__Strategy.strategy_with_asm in file "src/decomp/strategy.ml", line 503, characters 14-59
  Called from Imandrax_decomp__Strategy.strategy_with_asm in file "src/decomp/strategy.ml", lines 492-506, characters 6-7
  Called from Imandrax_reasoning__Eval_decomp.eval_direct_ in file "src/reasoning/eval_decomp.ml", lines 293-294, characters 4-76
  Called from Stdlib__Fun.protect in file "fun.ml", line 34, characters 8-15
  Re-raised at Stdlib__Fun.protect in file "fun.ml", line 39, characters 6-52
  Called from Imandrax_reasoning__Eval_decomp.with_decomp_state in file "src/reasoning/eval_decomp.ml", line 260, characters 8-12
  Called from Imandrax_reasoning__Eval_decomp.Eval_.eval_decomp_ in file "src/reasoning/eval_decomp.ml", line 495, characters 19-33
  Called from Imandrax_leval__Eval.apply_custom in file "src/leval/eval.ml", line 247, characters 2-28
  Called from Imandrax_leval__Eval.eval' in file "src/leval/eval.ml" (inlined), line 406, characters 43-63
  Called from Imandrax_leval__Eval.eval in file "src/leval/eval.ml", line 409, characters 11-22
  Called from Imandrax_reasoning__Eval_decomp.eval_term_ in file "src/reasoning/eval_decomp.ml", line 628, characters 10-54
  Called from Imandrax_reasoning__Eval_decomp.eval in file "src/reasoning/eval_decomp.ml", line 711, characters 6-31
  Called from Imandrax_session__Decomp_runner.run_decomp_directly in file "src/session/decomp_runner.ml", lines 35-36, characters 4-62
  Called from Stdlib__Fun.protect in file "fun.ml", line 34, characters 8-15
  Re-raised at Stdlib__Fun.protect in file "fun.ml", line 39, characters 6-52
  Called from Imandrakit_log__Trace_async.with_span_real_ in file "vendor/imandrakit/src/log/trace_async.ml", line 78, characters 14-46
  Re-raised at Imandrakit_log__Trace_async.with_span_real_ in file "vendor/imandrakit/src/log/trace_async.ml", line 85, characters 6-40
  Called from Imandrakit_error__Error.try_catch in file "vendor/imandrakit/src/error/error.ml", line 44, characters 9-15\
"""
    ],
    tasks=[
        Task(
            id=TaskID(id='task:decomp:Rcz2VDPaCDtbQiXe0P8EvOnPYpK864Qx9wJCQWX8B4o='),
            kind=TaskKind.TASK_DECOMP,
        )
    ],
)

# ====================

IML_HAS_GOAL_STATE = """\
(* Number of subsets of a (finite) set in Imandra, rep'd as lists.
   Theorem 52 in the 100 Theorems list.

   Grant Passmore, Imandra
 *)

(* prepend x to every subset in a family *)
let rec prepend (x:'a) (sets:'a list list) : 'a list list =
  match sets with
  | [] -> []
  | s::ss -> (x::s) :: prepend x ss
[@@measure (Ordinal.of_int (List.length sets))] [@@by auto]

(* Powerset of a list: P([]) = [[]]; P(x::xs) = P(xs) @ (map (x::) (P(xs))) *)
let rec powerset (xs:'a list) : 'a list list =
  match xs with
  | [] -> [[]]
  | x::xt ->
      let ps = powerset xt in
      ps @ (prepend x ps)
[@@measure (Ordinal.of_int (List.length xs))] [@@by auto]

let rec pow (a:int) (n:int) : int =
  if n <= 0 then 1 else a * pow a (n - 1)
[@@measure (Ordinal.of_int n)] [@@by auto]

theorem pow_zero a = pow a 0 = 1 [@@by auto]

theorem pow_succ a n =
  n >= 0 ==> pow a (n + 1) = a * pow a n
[@@by intros @> auto]

theorem pow2_step n =
  n >= 0 ==> pow 2 (n + 1) = 2 * pow 2 n
[@@by intros @> [%use pow_succ 2 n] @> auto]

lemma len_append x y =
  List.length (x @ y) = List.length x + List.length y
    (*
[@@by auto] [@@rw]  (* With this line, it can be proved automatically *)

lemma len_prepend x xs =
  List.length (prepend x xs) = List.length xs
[@@by auto] [@@rw]

theorem powerset_len xs =
  List.length (powerset xs) = pow 2 (List.length xs)
[@@by auto] *)"""

import imandrax_api.lib as xtype
import pytest
from inline_snapshot import snapshot

import imandrax_api_models.pp.pretty as Pp
from imandrax_api_models.pp.goal_state import (
    goal_state_doc_of_po_res,
    po_res_of_art_zip,
)

IML = """\
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
[@@by auto] *)
"""


@pytest.fixture
def po_res_list() -> list[xtype.Tasks_PO_res_Shallow]:
    import os

    from imandrax_api_models.client import get_imandrax_client
    from imandrax_api_models.proto_models import TaskKind

    c = get_imandrax_client(auth_token=os.environ['IMANDRAX_API_KEY'] or None)
    eval_res = c.eval_src(IML)
    po_tasks = [t for t in eval_res.tasks if t.kind == TaskKind.TASK_CHECK_PO]
    po_res_zips = [c.get_artifact_zip(task, kind='po_res') for task in po_tasks]
    return [po_res_of_art_zip(zip.art_zip) for zip in po_res_zips]


@pytest.mark.vcr
def test_goal_state_pp(po_res_list: list[xtype.Tasks_PO_res_Shallow]):
    snapshots: list[str] = []
    for i, po_res in enumerate(po_res_list):
        match goal_state_doc_of_po_res(po_res):
            case ('left', doc):
                snapshots.append(Pp.pretty(88, doc))
            case ('right', msg):
                snapshots.append(f'No goal state for {i=}; got: {msg=}')
    assert snapshots == snapshot(
        [
            """\
1 subgoal
- ⊢length (x append y) = length x + length y\
""",
            "No goal state for i=1; got: msg='Proof found'",
            "No goal state for i=2; got: msg='Proof found'",
            "No goal state for i=3; got: msg='Proof found'",
            "No goal state for i=4; got: msg='Proof found'",
            "No goal state for i=5; got: msg='Proof found'",
            "No goal state for i=6; got: msg='Proof found'",
        ]
    )

# %%
from IPython.core.getipython import get_ipython

if ip := get_ipython():
    ip.run_line_magic('reload_ext', 'autoreload')
    ip.run_line_magic('autoreload', '2')

import os
from pathlib import Path

import dotenv
import imandrax_api.bindings as xbinding
from imandrax_api import Client

from imandrax_api_models import Task
from imandrax_api_models.client import ImandraXClient
from imandrax_api_models.pp_goal_state import pp_goal_state

curr_dir = Path.cwd() if ip else Path(__file__).parent

dotenv.load_dotenv()


# %%
c = ImandraXClient(auth_token=os.environ['IMANDRAX_API_KEY'])


# %%
IML = """
(* Prove that the number of subsets of a list xs is 2^{|xs|}:
   List.length (powerset xs) = Int.pow 2 (List.length xs). *)

(* Generate powerset (all subsets) of a list *)
let rec powerset xs =
  match xs with
  | [] -> [[]]
  | x :: rest ->
    let ps = powerset rest in
    ps @ List.map (fun s -> x :: s) ps

(* Helper lemma: length of map preserves list length *)
lemma len_map f xs =
  List.length (List.map f xs) = List.length xs
[@@by induct ~on_vars:["xs"] ()]

(* Helper lemma: 2 * 2^n = 2^(n+1) *)
lemma pow_double n =
  n >= 0 ==> 2 * Int.pow 2 n = Int.pow 2 (n + 1)
[@@by induct ~on_vars:["n"] ()]

(* Main theorem: number of subsets = 2^length *)
theorem num_subsets xs =
  List.length (powerset xs) = Int.pow 2 (List.length xs)
[@@by induct ~on_vars:["xs"] ()]
"""

IML_2 = """
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
"""

eval_res = c.eval_src(IML)


# %%
task: Task = eval_res.tasks[0]  # TODO(Q): is this always the first task?
po_res_art_zip: xbinding.api_pb2.ArtifactZip = Client.get_artifact_zip(
    c,
    task.to_proto(),
    kind='po_res',
)
goal_state_s = pp_goal_state(po_res_art_zip)

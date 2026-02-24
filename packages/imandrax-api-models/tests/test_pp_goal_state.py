# %%
import os

import imandrax_api.bindings as xbinding
import pytest
from imandrax_api import Client, url_dev, url_prod  # noqa: F401, RUF100
from inline_snapshot import snapshot

from imandrax_api_models import Task
from imandrax_api_models.client import ImandraXClient
from imandrax_api_models.pp_goal_state import PO_RES_PP_BIN_PATH, pp_goal_state


@pytest.mark.skipif(
    PO_RES_PP_BIN_PATH is None, reason='PO_res pretty-printer binary not set in env'
)
def test_pp_goal_state():
    c = ImandraXClient(
        url=url_dev,
        auth_token=os.environ['IMANDRAX_API_KEY'],
    )

    IML = """
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

    task: Task = eval_res.tasks[0]
    po_res_art_zip: xbinding.api_pb2.ArtifactZip = Client.get_artifact_zip(
        c,
        task.to_proto(),
        kind='po_res',
    )

    assert po_res_art_zip.art_zip, 'No data in po_res zip'

    goal_state_s = pp_goal_state(po_res_art_zip)

    assert goal_state_s == snapshot("""\
1 subgoal -
 \n\
|------------------------------------------------------------
 (List.length (List.append x y))
 =
 ((List.length x) + (List.length y))


""")

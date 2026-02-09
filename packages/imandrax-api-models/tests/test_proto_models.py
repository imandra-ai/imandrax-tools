import os
from typing import cast

import imandrax_api.lib as xtypes
import pytest
from dirty_equals import IsBytes, IsStr
from google.protobuf.message import Message
from imandrax_api import Client, bindings as xbinding, url_dev, url_prod  # noqa: F401
from inline_snapshot import snapshot

from imandrax_api_models import (
    ArtifactListResult,
    ArtifactZip,
    DecomposeRes,
    EvalRes,
    GetDeclsRes,
    InstanceRes,
    ModelType,
    Task,
    TaskKind,
    TypecheckRes,
    VerifyRes,
)


class IsArtifactData(IsBytes):
    def __init__(self):
        super().__init__(min_length=1)


class IsTaskID(IsStr):
    def __init__(self):
        super().__init__(regex=r'task:.*')


@pytest.fixture
def c() -> Client:
    c = Client(
        url=url_prod,
        # url=url_dev,
        auth_token=os.environ['IMANDRAX_API_KEY'],
    )
    return c


IML_CODE = """\
let f (x : int) : int =
if x > 99 then
    100
else if 70 > x && x > 23 then
    89 + x
else if x > 20 then
    x + 20
else if x > -2 then
    103
else
    99

let g (x : int) : int =
if x < 0 then
    f x
else
    x + 5"""
IML_CODE_INVALID = """\
let f (x : int) : int =
    true\
"""
VERIFY_SRC = 'fun x -> g x > 0'
VERIFY_SRC_REFUTED = 'fun x -> g x <= 0'
DECOMPOSE_NAME = 'g'

IML_CODE_FOR_GET_DECLS = """
type direction = North | South | East | West

type position = { x: int; y: int; z: real }

type movement =
  | Stay of position
  | Move of position * direction
"""

IML_CODE_WITH_GOAL_STATE = """
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


def test_eval_res(c: Client):
    eval_res_msg: Message = c.eval_src(IML_CODE)
    eval_res = EvalRes.model_validate(eval_res_msg)
    assert eval_res.model_dump() == snapshot(
        {
            'success': True,
            'messages': [],
            'errors': [],
            'tasks': [],
            'po_results': [],
            'eval_results': [],
            'decomp_results': [],
        }
    )


def test_eval_err(c: Client):
    eval_res_msg: Message = c.eval_src(IML_CODE_INVALID)
    eval_res = EvalRes.model_validate(eval_res_msg)
    assert eval_res.model_dump() == snapshot(
        {
            'success': True,
            'messages': [],
            'errors': [
                {
                    'msg': {
                        'msg': """\
Expression is expected to have type `int`,
but its inferred type is `bool`.
The subtypes `int` and `bool` do not match.\
""",
                        'locs': [
                            {
                                'file': '<none>',
                                'start': {'line': 2, 'col': 5},
                                'stop': {'line': 2, 'col': 8},
                            }
                        ],
                        'backtrace': None,
                    },
                    'kind': '{ Kind.name = "TypeErr" }',
                    'stack': [],
                    'process': 'imandrax-server',
                }
            ],
            'tasks': [],
            'po_results': [],
            'eval_results': [],
            'decomp_results': [],
        }
    )


def test_verify_src(c: Client):
    _ = c.eval_src(IML_CODE)
    verify_res_msg = c.verify_src(VERIFY_SRC)
    verify_res = VerifyRes.model_validate(verify_res_msg)
    assert verify_res.model_dump() == snapshot(
        {
            'unknown': None,
            'err': None,
            'proved': {
                'proof_pp': """\
{ id = 1;
  concl =
  \n\
  |----------------------------------------------------------------------
   g x > 0
  ;
  view =
  T_deduction {
    premises =
    [("p",
      [{ id = 0;
         concl =
         \n\
         |----------------------------------------------------------------------
          g x > 0
         ; view = T_deduction {premises = []} }
        ])
      ]}
  }\
"""
            },
            'refuted': None,
            'verified_upto': None,
            'errors': [],
            'task': None,
        }
    )


def test_verify_refuted(c: Client):
    _ = c.eval_src(IML_CODE)
    verify_res_msg = c.verify_src(VERIFY_SRC_REFUTED)
    verify_res = VerifyRes.model_validate(verify_res_msg)
    assert verify_res.model_dump() == snapshot(
        {
            'unknown': None,
            'err': None,
            'proved': None,
            'refuted': {
                'model': {
                    'm_type': ModelType.Counter_example,
                    'src': """\
module M = struct

  let x = 0

 end
""",
                    'artifact': {
                        'kind': 'mir.model',
                        'data': IsArtifactData(),
                        'api_version': 'v18',
                        'storage': [],
                    },
                }
            },
            'verified_upto': None,
            'errors': [],
            'task': None,
        }
    )


@pytest.mark.flaky(reruns=2)
def test_instance_src(c: Client):
    _ = c.eval_src(IML_CODE)
    instance_res_msg = c.instance_src(VERIFY_SRC_REFUTED)
    instance_res = InstanceRes.model_validate(instance_res_msg)
    assert instance_res.model_dump() == snapshot(
        {
            'unknown': None,
            'err': None,
            'unsat': {
                'proof_pp': """\
{ id = 1;
  concl =
  \n\
  |----------------------------------------------------------------------
   not (g x <= 0)
  ;
  view =
  T_deduction {
    premises =
    [("p",
      [{ id = 0;
         concl =
         \n\
         |----------------------------------------------------------------------
          not (g x <= 0)
         ; view = T_deduction {premises = []} }
        ])
      ]}
  }\
"""
            },
            'sat': None,
            'errors': [],
            'task': None,
        }
    )


def test_instance_unsat(c: Client):
    _ = c.eval_src(IML_CODE)
    instance_res_msg = c.instance_src(VERIFY_SRC)
    instance_res = InstanceRes.model_validate(instance_res_msg)
    assert instance_res.model_dump() == snapshot(
        {
            'unknown': None,
            'err': None,
            'unsat': None,
            'sat': {
                'model': {
                    'm_type': ModelType.Instance,
                    'src': """\
module M = struct

  let x = 0

 end
""",
                    'artifact': {
                        'kind': 'mir.model',
                        'data': IsArtifactData(),
                        'api_version': 'v18',
                        'storage': [],
                    },
                }
            },
            'errors': [],
            'task': None,
        }
    )


def test_decompose(c: Client):
    _ = c.eval_src(IML_CODE)
    decompose_res_msg = c.decompose(DECOMPOSE_NAME)
    decompose_res = DecomposeRes.model_validate(decompose_res_msg)
    assert decompose_res.model_dump() == snapshot(
        {
            'artifact': {
                'kind': 'mir.fun_decomp',
                'data': IsArtifactData(),
                'api_version': 'v18',
                'storage': [],
            },
            'err': None,
            'errors': [],
            'task': {
                'id': {
                    'id': IsTaskID(),
                },
                'kind': TaskKind.TASK_DECOMP,
            },
            'regions_str': [
                {
                    'constraints_str': ['x <= (-2)'],
                    'invariant_str': '99',
                    'model_str': {'x': '(-2)'},
                    'model_eval_str': '99',
                },
                {
                    'constraints_str': ['x = (-1)'],
                    'invariant_str': '103',
                    'model_str': {'x': '(-1)'},
                    'model_eval_str': '103',
                },
                {
                    'constraints_str': ['x >= 0'],
                    'invariant_str': 'x + 5',
                    'model_str': {'x': '0'},
                    'model_eval_str': '5',
                },
            ],
        }
    )


def test_typecheck(c: Client):
    typecheck_res_msg = c.typecheck(IML_CODE)
    typecheck_res = TypecheckRes.model_validate(typecheck_res_msg)
    assert typecheck_res.model_dump() == snapshot(
        {
            'success': True,
            'types': [
                {'name': 'g', 'ty': 'int -> int', 'line': 13, 'column': 1},
                {'name': 'f', 'ty': 'int -> int', 'line': 1, 'column': 1},
            ],
            'errors': [],
        }
    )


def test_typecheck_err(c: Client):
    typecheck_res_msg = c.typecheck(IML_CODE_INVALID)
    typecheck_res = TypecheckRes.model_validate(typecheck_res_msg)
    assert typecheck_res.model_dump() == snapshot(
        {
            'success': False,
            'types': [{'name': 'f', 'ty': 'int -> int', 'line': 1, 'column': 1}],
            'errors': [
                {
                    'msg': {
                        'msg': """\
Expression is expected to have type `int`,
but its inferred type is `bool`.
The subtypes `int` and `bool` do not match.\
""",
                        'locs': [
                            {
                                'file': '<none>',
                                'start': {'line': 2, 'col': 5},
                                'stop': {'line': 2, 'col': 8},
                            }
                        ],
                        'backtrace': None,
                    },
                    'kind': '{ Kind.name = "TypeErr" }',
                    'stack': [],
                    'process': 'imandrax-server',
                }
            ],
        }
    )


def test_get_decls(c: Client):
    _ = c.eval_src(IML_CODE_FOR_GET_DECLS)
    get_decls_res_data = c.get_decls(['direction', 'position', 'movement', 'else'])
    get_decls_res = GetDeclsRes.model_validate(get_decls_res_data)

    assert get_decls_res.model_dump(by_alias=True) == snapshot(
        {
            'decls': [
                {
                    'name': 'movement',
                    'artifact': {
                        'kind': 'mir.decl',
                        'data': IsArtifactData(),
                        'api_version': 'v18',
                        'storage': [],
                    },
                    'str': None,
                },
                {
                    'name': 'position',
                    'artifact': {
                        'kind': 'mir.decl',
                        'data': IsArtifactData(),
                        'api_version': 'v18',
                        'storage': [],
                    },
                    'str': None,
                },
                {
                    'name': 'direction',
                    'artifact': {
                        'kind': 'mir.decl',
                        'data': IsArtifactData(),
                        'api_version': 'v18',
                        'storage': [],
                    },
                    'str': None,
                },
            ],
            'not_found': ['else'],
        }
    )


def test_task(c: Client):
    eval_res = c.eval_src(IML_CODE_WITH_GOAL_STATE)
    task_pb = cast(xbinding.task_pb2.Task, eval_res.tasks[0])  # pyright: ignore[reportUnknownMemberType]
    task = Task.model_validate(task_pb)
    assert task.model_dump() == snapshot(
        {
            'id': {'id': IsTaskID()},
            'kind': TaskKind.TASK_CHECK_PO,
        }
    )


def test_art_list_res(c: Client):
    eval_res = c.eval_src(IML_CODE_WITH_GOAL_STATE)
    task_pb = cast(xbinding.task_pb2.Task, eval_res.tasks[0])  # pyright: ignore[reportUnknownMemberType]
    art_list_res_pb = c.list_artifacts(task_pb)
    art_list_res = ArtifactListResult.model_validate(art_list_res_pb)
    assert art_list_res == snapshot(
        ArtifactListResult(kinds=['report', 'po_res', 'po_task', 'show'])
    )


def test_art_zip(c: Client):
    eval_res = c.eval_src(IML_CODE_WITH_GOAL_STATE)
    task_pb = cast(xbinding.task_pb2.Task, eval_res.tasks[0])  # pyright: ignore[reportUnknownMemberType]
    art_zip_pb = c.get_artifact_zip(task=task_pb, kind='po_res')
    art_zip = ArtifactZip.model_validate(art_zip_pb)
    art = art_zip.to_artifact()
    assert isinstance(art, xtypes.Tasks_PO_res_shallow_poly)

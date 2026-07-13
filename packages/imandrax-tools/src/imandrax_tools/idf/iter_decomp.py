# pyright: reportUnknownMemberType=false, reportUnknownVariableType=false, reportUnknownArgumentType=false
# ^ Reason: imandrax_api.bindings is untyped
from __future__ import annotations

import textwrap
from dataclasses import dataclass
from typing import Any, Literal, Never, TypedDict

from imandrax_api.bindings import artmsg_pb2, simple_api_pb2, simple_api_twirp_async
from imandrax_api_models import DecomposeRes, EvalRes
from imandrax_api_models.client import ImandraXAsyncClient

from .gen_iml import Guard, Target, gen_decomp_funs


@dataclass
class IDFError:
    def raise_(self) -> Never:
        raise RuntimeError(str(self))


@dataclass
class SignatureMismatch(IDFError):
    name: Literal['StateMachine', 'Template']


@dataclass
class EvalError(IDFError):
    message: str
    eval_res: EvalRes


@dataclass
class DecomposeError(IDFError):
    message: str
    metadata: Any | None = None


def check_mod_sig(mod: str, sig: str) -> bool:
    return True


def wrap_mod(name: str, defns: str) -> str:
    """Wrap the given IML definitions in a module with the given name."""
    return f"""\
module {name} = struct
{textwrap.indent(defns, '  ')}
end
"""


# API Decomp utils
# ====================


Decomp = simple_api_pb2.DecomposeReqFull.Decomp


def from_artifact(art: artmsg_pb2.Art) -> Decomp:
    return Decomp(from_artifact=art)


def by_name(
    name: str,
    assuming: str | None = None,
    basis: list[str] | None = None,
    rule_specs: list[str] | None = None,
    prune: bool | None = None,
    ctx_simp: bool | None = None,
    lift_bool: simple_api_pb2.LiftBool | None = None,
) -> Decomp:
    d_by_name = simple_api_pb2.DecomposeReqFull.ByName(
        name=name,
        assuming=assuming,
        basis=basis,
        rule_specs=rule_specs,
        lift_bool=lift_bool,
    )
    if prune is not None:
        d_by_name.prune = prune
    if ctx_simp is not None:
        d_by_name.ctx_simp = ctx_simp
    return Decomp(by_name=d_by_name)


def merge(d1: Decomp, d2: Decomp) -> Decomp:
    return Decomp(merge=simple_api_pb2.DecomposeReqFull.Merge(d1=d1, d2=d2))


async def merge_decompose(
    c: ImandraXAsyncClient,
    name: str,
    assuming: str | None = None,
    basis: list[str] | None = None,
    rule_specs: list[str] | None = None,
    prune: bool | None = None,
    ctx_simp: bool | None = None,
    lift_bool: simple_api_pb2.LiftBool | None = None,
    string_results: bool | None = None,
    *,
    merge_from: simple_api_pb2.DecomposeRes | None = None,
) -> simple_api_pb2.DecomposeRes | IDFError:
    d = by_name(
        name=name,
        basis=basis,
        assuming=assuming,
        rule_specs=rule_specs,
        prune=prune,
        ctx_simp=ctx_simp,
        lift_bool=lift_bool,
    )
    if merge_from is not None:
        assert merge_from.artifact is not None, 'No artifact in merge_from'
        art_model = merge_from.artifact
        art = artmsg_pb2.Art(
            kind=art_model.kind,
            data=art_model.data,
            api_version=art_model.api_version,
            storage=[
                artmsg_pb2.StorageEntry(key=s.key, value=s.value)
                for s in art_model.storage
            ],
        )
        d2 = from_artifact(art)
        d = merge(d, d2)

    # pyright ignore note: will be removed until this funciton is upstreamed
    simple_client: simple_api_twirp_async.AsyncSimpleClient = c._client  # pyright: ignore[reportPrivateUsage]
    req = simple_api_pb2.DecomposeReqFull(
        session=c._sesh,  # pyright: ignore[reportPrivateUsage]
        decomp=d,
    )
    if string_results is not None:
        req.string_results = string_results
    res = await simple_client.decompose_full(
        ctx=c.mk_context(),
        request=req,
        timeout=c._timeout,  # pyright: ignore[reportPrivateUsage]
    )
    return res


# ====================

type Either[L, R] = tuple[Literal['left'], L] | tuple[Literal['right'], R]


class Step(TypedDict):
    """Decomp result with metadata"""

    decomp_res: DecomposeRes
    message: str
    guard_iml: str
    target_iml: str
    # <decomp_req>
    name: str
    assuming: str
    basis: list[str]
    merge_with: str | None
    # </decomp_req>


async def iter_decomp(
    c: ImandraXAsyncClient,
    sm: str,
    tpl: str,
    message_flows: list[str],
    lift_bool: simple_api_pb2.LiftBool | None = simple_api_pb2.LiftBool.Equalities,
) -> Either[list[Step], IDFError]:
    # Check module signature
    if not check_mod_sig(sm, ''):
        return ('right', SignatureMismatch(name='StateMachine'))
    if not check_mod_sig(tpl, ''):
        return ('right', SignatureMismatch(name='Template'))

    # Eval SM and TPL
    sm_eval_res = await c.eval_src(wrap_mod('SM', sm))
    if not sm_eval_res.success:
        return ('right', EvalError('SM eval error', sm_eval_res))

    tpl_eval_res = await c.eval_src(wrap_mod('TPL', tpl))
    if not tpl_eval_res.success:
        return ('right', EvalError('TPL eval error', tpl_eval_res))

    # Decomp
    decomp_funs: list[tuple[Guard, Target]] = gen_decomp_funs(message_flows)

    steps: list[Step] = []
    decomp_pb_res_by_step: list[simple_api_pb2.DecomposeRes] = []

    for i, (msg, (guard, target)) in enumerate(
        zip(message_flows, decomp_funs, strict=True)
    ):
        assuming: str = guard.fun_name()
        basis: list[str]
        if i == 0:
            basis = []
        else:
            prev_target_name = decomp_funs[i - 1][1].fun_name()
            basis = [prev_target_name]

        guard_iml = guard.iml()
        target_iml = target.iml()

        g_eval_res = await c.eval_src(guard_iml)
        if not g_eval_res.success:
            return ('right', EvalError(f'Guard eval error at step {i}', g_eval_res))
        t_eval_res = await c.eval_src(target_iml)
        if not t_eval_res.success:
            return ('right', EvalError(f'Target eval error at step {i}', t_eval_res))

        prev_decomp_res = None if i == 0 else decomp_pb_res_by_step[-1]
        decomp_pb_res = await merge_decompose(
            c=c,
            name=target.fun_name(),
            assuming=assuming,
            basis=basis,
            prune=True,
            string_results=True,
            lift_bool=lift_bool,
            merge_from=prev_decomp_res,
        )

        if isinstance(decomp_pb_res, IDFError):
            return ('right', decomp_pb_res)

        decomp_pb_res_by_step.append(decomp_pb_res)
        step = Step(
            message=msg,
            decomp_res=DecomposeRes.model_validate(decomp_pb_res),
            guard_iml=guard_iml,
            target_iml=target_iml,
            name=target.fun_name(),
            assuming=assuming,
            basis=basis,
            merge_with=steps[-1]['name'] if i > 0 else None,
        )
        steps.append(step)

    return ('left', steps)
    # return ('left', [DecomposeRes.model_validate(res) for res in decomp_pb_res_by_step])
    # tree = build_idf_tree(decomp_res_by_step, message_flows)
    # return ('left', IDFData(tree=tree))

"""
Tests for the pydantic decomposition plan.

The plans are checked against `imandrax_api.client.decomp`, which builds the
same protos directly: if the two disagree, the mirror has drifted from the
wire format. All offline -- no session needed.
"""

import imandrax_api.bindings.simple_api_pb2 as xsimple_api_pb2
from imandrax_api.client import decomp as proto_decomp
from imandrax_api_models import Art, Decomp, LiftBool, decomp
from pydantic import TypeAdapter

PLAN: TypeAdapter[Decomp] = TypeAdapter(Decomp)

ART = Art(kind='mir.fun_decomp', data=b'\x00twine', api_version='v20')


def test_by_name_minimal():
    assert decomp.by_name('f').to_proto() == proto_decomp.by_name('f')


def test_by_name_all_options():
    plan = decomp.by_name(
        'f',
        assuming='sc',
        basis=['g', 'h'],
        rule_specs=['rs'],
        prune=True,
        ctx_simp=False,
        lift_bool=LiftBool.Equalities,
    )
    assert plan.to_proto() == proto_decomp.by_name(
        'f',
        assuming='sc',
        basis=['g', 'h'],
        rule_specs=['rs'],
        prune=True,
        ctx_simp=False,
        lift_bool=xsimple_api_pb2.LiftBool.Equalities,
    )


def test_unset_optionals_stay_unset():
    """`None` must not be sent as a default -- the server distinguishes them."""
    msg = decomp.by_name('f').to_proto().by_name
    assert not msg.HasField('prune')
    assert not msg.HasField('ctx_simp')
    assert not msg.HasField('lift_bool')
    assert not msg.HasField('assuming')


def test_explicit_false_is_sent():
    msg = decomp.by_name('f', prune=False, ctx_simp=False).to_proto().by_name
    assert msg.HasField('prune')
    assert msg.prune is False
    assert msg.HasField('ctx_simp')
    assert msg.ctx_simp is False


def test_from_artifact():
    assert decomp.from_artifact(ART).to_proto() == proto_decomp.from_artifact(
        ART.to_proto()
    )


def test_binary_ops():
    d1, p1 = decomp.by_name('f'), proto_decomp.by_name('f')
    d2, p2 = decomp.by_name('g'), proto_decomp.by_name('g')
    assert decomp.merge(d1, d2).to_proto() == proto_decomp.merge(p1, p2)
    assert decomp.compound_merge(d1, d2).to_proto() == proto_decomp.compound_merge(
        p1, p2
    )


def test_unary_ops():
    d, p = decomp.by_name('f'), proto_decomp.by_name('f')
    assert decomp.prune(d).to_proto() == proto_decomp.prune(p)
    assert decomp.combine(d).to_proto() == proto_decomp.combine(p)


def test_let_and_get():
    plan = decomp.let(
        [('a', decomp.by_name('f')), ('b', decomp.prune(decomp.by_name('g')))],
        decomp.merge(decomp.get('a'), decomp.get('b')),
    )
    expected = proto_decomp.let(
        [
            ('a', proto_decomp.by_name('f')),
            ('b', proto_decomp.prune(proto_decomp.by_name('g'))),
        ],
        proto_decomp.merge(proto_decomp.get('a'), proto_decomp.get('b')),
    )
    assert plan.to_proto() == expected


def test_json_round_trip():
    plan = decomp.let(
        [('base', decomp.by_name('f', prune=True, lift_bool=LiftBool.All))],
        decomp.merge(decomp.get('base'), decomp.from_artifact(ART)),
    )
    back = PLAN.validate_json(plan.model_dump_json())
    assert back == plan
    assert back.to_proto() == plan.to_proto()


def test_json_schema_covers_every_op():
    defs = PLAN.json_schema()['$defs']
    assert {
        'ByName',
        'Combine',
        'CompoundMerge',
        'FromArtifact',
        'LocalVarGet',
        'LocalVarLet',
        'Merge',
        'Prune',
    } <= set(defs)


def test_plan_repr_hides_artifact_bytes():
    plan = decomp.merge(decomp.from_artifact(ART), decomp.by_name('f', prune=True))
    r = decomp.decomp_repr(plan)
    assert r == 'merge(from_artifact(mir.fun_decomp), by_name(f, prune))'
    assert 'twine' not in r

# pyright: reportUnknownMemberType=false, reportUnknownVariableType=false
"""
Pydantic mirror of `DecomposeReqFull.Decomp`, the recursive decomposition plan.

A plan is a tree: a base decomposition (by name, or read back from an artifact)
that can then be pruned, combined, merged with another, or bound to a local
variable for reuse. Build one with the combinators at the bottom of this module
and hand it to `ImandraXClient.decompose_full`.

Each node's `op` is the name of the corresponding `oneof decomp` field in
`simple_api.proto`, so the mapping to the wire format stays mechanical --
including `op='set'` for let-binding, whose proto field is `set`.
"""

from __future__ import annotations

from typing import Annotated, Literal

import imandrax_api.bindings.simple_api_pb2 as xsimple_api_pb2
from pydantic import Field

from ..proto_utils import BaseModel
from .artmsg import Art
from .simple_api import LiftBool

# Runtime aliases to the generated proto classes (used as constructors).
_Full = xsimple_api_pb2.DecomposeReqFull
_Decomp = xsimple_api_pb2.DecomposeReqFull.Decomp


class FromArtifact(BaseModel):
    """
    Resume from a decomposition previously returned as an artifact.
    """

    op: Literal['from_artifact'] = 'from_artifact'
    art: Art

    def to_proto(self) -> _Decomp:
        return _Decomp(from_artifact=self.art.to_proto())


class ByName(BaseModel):
    """Decompose the function called `name`."""

    op: Literal['by_name'] = 'by_name'
    name: str = Field(description='name of function to decompose')
    assuming: str | None = Field(
        default=None, description='name of side condition function'
    )
    basis: list[str] = Field(default_factory=list)
    rule_specs: list[str] = Field(default_factory=list)
    prune: bool | None = Field(default=None)
    ctx_simp: bool | None = Field(default=None)
    lift_bool: LiftBool | None = Field(default=None)

    def to_proto(self) -> _Decomp:
        # `None` leaves an explicit-presence field unset. The generated stubs
        # type the optional bools as plain `bool`, so those two are assigned
        # after construction instead of passed in.
        msg = _Full.ByName(
            name=self.name,
            assuming=self.assuming,
            basis=self.basis,
            rule_specs=self.rule_specs,
            lift_bool=None if self.lift_bool is None else self.lift_bool.name,
        )
        if self.prune is not None:
            msg.prune = self.prune
        if self.ctx_simp is not None:
            msg.ctx_simp = self.ctx_simp
        return _Decomp(by_name=msg)


class Merge(BaseModel):
    """Merge two decompositions."""

    op: Literal['merge'] = 'merge'
    d1: Decomp
    d2: Decomp

    def to_proto(self) -> _Decomp:
        return _Decomp(merge=_Full.Merge(d1=self.d1.to_proto(), d2=self.d2.to_proto()))


class CompoundMerge(BaseModel):
    """Compound-merge two decompositions."""

    op: Literal['compound_merge'] = 'compound_merge'
    d1: Decomp
    d2: Decomp

    def to_proto(self) -> _Decomp:
        return _Decomp(
            compound_merge=_Full.CompoundMerge(
                d1=self.d1.to_proto(), d2=self.d2.to_proto()
            )
        )


class Prune(BaseModel):
    op: Literal['prune'] = 'prune'
    d: Decomp

    def to_proto(self) -> _Decomp:
        return _Decomp(prune=_Full.Prune(d=self.d.to_proto()))


class Combine(BaseModel):
    op: Literal['combine'] = 'combine'
    d: Decomp

    def to_proto(self) -> _Decomp:
        return _Decomp(combine=_Full.Combine(d=self.d.to_proto()))


class LocalVarGet(BaseModel):
    """Read back the decomposition bound to the local variable `name`."""

    op: Literal['get'] = 'get'
    name: str = Field(
        description='get the result stored in the variable with the given name'
    )

    def to_proto(self) -> _Decomp:
        return _Decomp(get=_Full.LocalVarGet(name=self.name))


class LocalVarBinding(BaseModel):
    name: str = Field(description='bind local name to the result of `d`')
    d: Decomp = Field(description='the operation to perform and store in the variable')

    def to_proto(self) -> _Full.LocalVarBinding:
        return _Full.LocalVarBinding(name=self.name, d=self.d.to_proto())


class LocalVarLet(BaseModel):
    """
    Bind each binding, then evaluate `and_then`.

    The bindings are evaluated simultaneously in the same environment, so one
    binding cannot refer to another from the same let; nest lets for that.
    """

    op: Literal['set'] = 'set'
    bindings: list[LocalVarBinding] = Field(
        default_factory=list,
        description='let-bindings to do simultaneously (in the same environment)',
    )
    and_then: Decomp = Field(
        description='the decomposition to do after the bindings are evaluated'
    )

    def to_proto(self) -> _Decomp:
        return _Decomp(
            set=_Full.LocalVarLet(
                bindings=[b.to_proto() for b in self.bindings],
                and_then=self.and_then.to_proto(),
            )
        )


type Decomp = Annotated[
    FromArtifact
    | ByName
    | Merge
    | CompoundMerge
    | Prune
    | Combine
    | LocalVarGet
    | LocalVarLet,
    Field(discriminator='op'),
]


# Combinators
# ====================
# Mirroring `imandrax_api.client.decomp`


def from_artifact(art: Art) -> FromArtifact:
    """Resume from a decomposition previously returned as an artifact."""
    return FromArtifact(art=art)


def by_name(
    name: str,
    assuming: str | None = None,
    basis: list[str] | None = None,
    rule_specs: list[str] | None = None,
    prune: bool | None = None,
    ctx_simp: bool | None = None,
    lift_bool: LiftBool | None = None,
) -> ByName:
    """Decompose the function called `name`."""
    return ByName(
        name=name,
        assuming=assuming,
        basis=basis or [],
        rule_specs=rule_specs or [],
        prune=prune,
        ctx_simp=ctx_simp,
        lift_bool=lift_bool,
    )


def merge(d1: Decomp, d2: Decomp) -> Merge:
    """Merge two decompositions."""
    return Merge(d1=d1, d2=d2)


def compound_merge(d1: Decomp, d2: Decomp) -> CompoundMerge:
    """Compound-merge two decompositions."""
    return CompoundMerge(d1=d1, d2=d2)


def prune(d: Decomp) -> Prune:
    """Prune the regions of `d`."""
    return Prune(d=d)


def combine(d: Decomp) -> Combine:
    """Combine the regions of `d`."""
    return Combine(d=d)


def get(name: str) -> LocalVarGet:
    """Read back the decomposition bound to the local variable `name`."""
    return LocalVarGet(name=name)


def let(bindings: list[tuple[str, Decomp]], and_then: Decomp) -> LocalVarLet:
    """
    Bind each `(name, plan)` pair, then evaluate `and_then`.

    The bindings are evaluated simultaneously in the same environment, so one
    binding cannot refer to another from the same let; nest lets for that.
    """
    return LocalVarLet(
        bindings=[LocalVarBinding(name=n, d=d) for (n, d) in bindings],
        and_then=and_then,
    )


# ====================


def decomp_repr(d: Decomp) -> str:
    """
    Compact one-line structural repr of a Decomp plan.

    Safe for logs and spans: no twine bytes leaked.
    """
    match d:
        case FromArtifact():
            return f'from_artifact({d.art.kind})'
        case ByName():
            args = [d.name]
            if d.assuming is not None:
                args.append(f'assuming={d.assuming}')
            if d.basis:
                args.append(f'basis={len(d.basis)}')
            if d.rule_specs:
                args.append(f'rule_specs={len(d.rule_specs)}')
            if d.prune:
                args.append('prune')
            if d.ctx_simp:
                args.append('ctx_simp')
            if d.lift_bool is not None:
                args.append(f'lift_bool={d.lift_bool.name}')
            return f'by_name({", ".join(args)})'
        case Merge():
            return f'merge({decomp_repr(d.d1)}, {decomp_repr(d.d2)})'
        case CompoundMerge():
            return f'compound_merge({decomp_repr(d.d1)}, {decomp_repr(d.d2)})'
        case Prune():
            return f'prune({decomp_repr(d.d)})'
        case Combine():
            return f'combine({decomp_repr(d.d)})'
        case LocalVarGet():
            return f'get({d.name})'
        case LocalVarLet():
            binds = ', '.join(f'{b.name}={decomp_repr(b.d)}' for b in d.bindings)
            return f'let([{binds}], {decomp_repr(d.and_then)})'

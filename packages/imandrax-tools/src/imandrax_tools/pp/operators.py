"""
Operator-precedence info for ImandraX (MIR) terms, used to drive parenthesisation in `term_formatter`.

Precedence table follows OCaml 5.3 (https://ocaml.org/manual/5.3/expr.html).
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import IntEnum

import imandrax_api.lib as xtype

# Types
# =====


class Notation(IntEnum):
    NONE = 0
    INFIX = 1
    PREFIX = 2


class Associativity(IntEnum):
    NONE = 0
    LEFT = 1
    RIGHT = 2


@dataclass(slots=True, frozen=True)
class OperatorInfo:
    name: str
    notation: Notation
    associativity: Associativity
    precedence: float


# Term-shape utilities
# ====================


def short_id(uid: xtype.Uid) -> str:
    """Bare (display) name of a Uid — equivalent to TS `short_id` of `name/hash`."""
    return uid.name


def has_multiple_children(t: xtype.Mir_Term) -> bool:
    v = t.view
    match v:
        case xtype.Mir_Term_view_Const(arg=c):
            return isinstance(c, xtype.Const_Const_q) and c.arg[1] != 1
        case xtype.Mir_Term_view_If():
            return True
        case xtype.Mir_Term_view_Apply(l=args):
            return len(args) > 0
        case xtype.Mir_Term_view_Var() | xtype.Mir_Term_view_Sym():
            return False
        case xtype.Mir_Term_view_Construct(args=args):
            return len(args) > 0
        case xtype.Mir_Term_view_Destruct() | xtype.Mir_Term_view_Is_a():
            return True
        case xtype.Mir_Term_view_Tuple():
            return True
        case xtype.Mir_Term_view_Field() | xtype.Mir_Term_view_Tuple_field():
            return False
        case xtype.Mir_Term_view_Record():
            return False
        case xtype.Mir_Term_view_Case() | xtype.Mir_Term_view_Sequence():
            return True
    return False


# Precedence table
# ================


def operator_info(op: str, more_than_one_arg: bool = False) -> OperatorInfo:
    # prefix-symbol
    if op.startswith(('!', '?', '~')) and len(op) > 1 and op not in ('~-', '~-.'):
        return OperatorInfo(op, Notation.PREFIX, Associativity.NONE, 20)
    # . .( .[ .{
    if op.startswith('#'):
        return OperatorInfo(op, Notation.INFIX, Associativity.LEFT, 18)
    # function/constructor/tag application, assert, lazy
    if op == 'assert' or op == 'lazy':
        return OperatorInfo(op, Notation.PREFIX, Associativity.LEFT, 17)
    # unary minus
    if op in ('-', '-.', '~-', '~-.') and not more_than_one_arg:
        return OperatorInfo(op, Notation.PREFIX, Associativity.NONE, 16)
    # **… lsl lsr asr
    if op.startswith('**') or op in ('lsl', 'lsr', 'asr'):
        return OperatorInfo(op, Notation.INFIX, Associativity.RIGHT, 15)
    # *… /… %… mod land lor lxor
    if op.startswith(('*', '/', '%')) or op in ('mod', 'land', 'lor', 'lxor'):
        return OperatorInfo(op, Notation.INFIX, Associativity.LEFT, 14)
    # +… -…
    if op.startswith(('+', '-')):
        return OperatorInfo(op, Notation.INFIX, Associativity.LEFT, 13)
    # ::
    if op == '::':
        return OperatorInfo(op, Notation.INFIX, Associativity.RIGHT, 12)
    # @… ^…
    if op.startswith(('@', '^')):
        return OperatorInfo(op, Notation.INFIX, Associativity.RIGHT, 11)

    # NOTE: this deviates from `imandrax_operators.ts`, where the
    # `implies/explies/iff` special-cases are placed *after* the generic
    # `op.startsWith("=")`/`"<"` clause below — making them dead code (e.g.
    # `==>` gets matched as a comparison operator at precedence 10, LEFT).
    # We hoist them above so the intended precedences (8.3 R / 8.2 L / 8.1 N)
    # actually take effect.
    if op in ('implies', '==>'):
        return OperatorInfo('==>', Notation.INFIX, Associativity.RIGHT, 8.3)
    if op in ('explies', '<=='):
        return OperatorInfo('<==', Notation.INFIX, Associativity.LEFT, 8.2)
    if op in ('iff', '<==>'):
        return OperatorInfo('<==>', Notation.INFIX, Associativity.NONE, 8.1)

    # =… <… >… |… &… $… !=
    if (
        op.startswith(('=', '<', '>'))
        or (op.startswith('|') and op != '||')
        or (op.startswith('&') and op not in ('&', '&&'))
        or op.startswith('$')
        or op == '!='
    ):
        return OperatorInfo(op, Notation.INFIX, Associativity.LEFT, 10)
    # & &&
    if op in ('&', '&&'):
        return OperatorInfo(op, Notation.INFIX, Associativity.RIGHT, 9)
    # or ||
    if op in ('or', '||'):
        return OperatorInfo(op, Notation.INFIX, Associativity.RIGHT, 8)
    # ,
    if op == ',':
        return OperatorInfo(op, Notation.NONE, Associativity.NONE, 7)
    # <- :=
    if op in ('<-', ':='):
        return OperatorInfo(op, Notation.INFIX, Associativity.RIGHT, 6)
    # if
    if op == 'if':
        return OperatorInfo(op, Notation.NONE, Associativity.NONE, 5)
    # ;
    if op == ';':
        return OperatorInfo(op, Notation.INFIX, Associativity.RIGHT, 4)
    # let match fun function try
    if op in ('let', 'match', 'fun', 'function', 'try'):
        return OperatorInfo(op, Notation.NONE, Associativity.NONE, 3)

    if op == 'List.append':
        return operator_info('@', more_than_one_arg)

    if op == 'subterm_selection_wildcard':
        return OperatorInfo('_', Notation.PREFIX, Associativity.LEFT, 17)

    # default: function/constructor/tag application
    return OperatorInfo(op, Notation.PREFIX, Associativity.LEFT, 17)


def default_() -> OperatorInfo:
    return OperatorInfo('', Notation.PREFIX, Associativity.LEFT, 17)


def is_default(x: OperatorInfo) -> bool:
    return (
        x.notation == Notation.PREFIX
        and x.associativity == Associativity.LEFT
        and x.precedence == 17
    )


def operator_info_of_term(t: xtype.Mir_Term) -> OperatorInfo:
    v = t.view
    match v:
        case xtype.Mir_Term_view_If():
            return operator_info('if', False)
        case xtype.Mir_Term_view_Apply(f=f, l=args):
            if isinstance(f.view, xtype.Mir_Term_view_Sym):
                return operator_info(short_id(f.view.arg.sym.id), len(args) > 0)
            return default_()
        case xtype.Mir_Term_view_Tuple():
            return operator_info(',', False)
        case xtype.Mir_Term_view_Case():
            return operator_info('match', False)
        case xtype.Mir_Term_view_Construct(c=c, args=args):
            return operator_info(short_id(c.sym.id), len(args) > 0)
        case xtype.Mir_Term_view_Const(arg=c):
            if isinstance(c, xtype.Const_Const_q) and c.arg[1] != 1:
                return operator_info('/.', True)
            return default_()
        case xtype.Mir_Term_view_Is_a():
            return OperatorInfo('is-a', Notation.INFIX, Associativity.LEFT, 17)
        case _:
            return default_()


def needs_parentheses(
    parent_oi: OperatorInfo,
    child_oi: OperatorInfo,
    is_left: bool | None = None,
    child_has_multiple_children: bool = False,
) -> bool:
    a = child_oi.name != '' and (
        parent_oi.precedence > child_oi.precedence
        or (
            (is_left is not None and child_oi.precedence == parent_oi.precedence)
            and (
                (is_left is True and parent_oi.associativity == Associativity.RIGHT)
                or (is_left is False and parent_oi.associativity == Associativity.LEFT)
            )
        )
    )
    b = parent_oi.notation != Notation.INFIX and child_oi.notation == Notation.INFIX
    c = (
        child_oi.precedence == parent_oi.precedence
        and is_default(child_oi)
        and child_has_multiple_children
    )
    return a or b or c

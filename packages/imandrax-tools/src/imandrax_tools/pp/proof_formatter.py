"""
Pretty-printer for ImandraX proof terms, as a box-drawing derivation tree.

A proof term (`Proof_Proof_term_t_poly`) is a tree: each node carries the
sequent it proves (`concl`) and a justification (`view`) that holds the child
proof terms. We render each node as `[justification] ⊢ concl`, with children
hung beneath using tree guides (`├─`, `└─`, `│`), threaded through the layout
via `pretty.prefix`.
"""

from __future__ import annotations

import imandrax_api.lib as xtype

from .goal_state import doc_of_sequent
from .operators import short_id
from .pretty import (
    Doc,
    brackets,
    group,
    hcat,
    join,
    line,
    text,
    tree,
)
from .term_formatter import term2doc
from .type_formatter import type2doc


def _tag(label: str, concl: Doc) -> Doc:
    """A node header: `[label] ⊢ concl`."""
    return hcat(text(f'[{label}] '), concl)


# Arguments
# =========


def _arg2doc(a: xtype.Proof_Arg) -> Doc:
    match a:
        case xtype.Proof_Arg_A_term(arg=t):
            return term2doc(t)
        case xtype.Proof_Arg_A_ty(arg=ty):
            return type2doc(ty)
        case xtype.Proof_Arg_A_int(arg=i):
            return text(str(i))
        case xtype.Proof_Arg_A_string(arg=s):
            return text(repr(s))
        case xtype.Proof_Arg_A_list(arg=xs):
            return brackets(join(hcat(text(','), line), [_arg2doc(x) for x in xs]))
        case xtype.Proof_Arg_A_dict(arg=kvs):
            rows = [hcat(text(f'{k}: '), _arg2doc(x)) for k, x in kvs]
            return hcat(text('{'), join(hcat(text(','), line), rows), text('}'))
        case xtype.Proof_Arg_A_seq(arg=sq):
            return doc_of_sequent(sq)
    raise ValueError(f'Unhandled proof arg: {type(a).__name__}')


# Substitutions (T_subst)
# =======================


def _subst2doc(
    t_subst: list[tuple[xtype.Proof_Var_poly, xtype.Mir_Term]],
    ty_subst: list[tuple[xtype.Uid, xtype.Mir_Type]],
) -> Doc:
    binds: list[Doc] = []
    for (uid, _), term in t_subst:
        binds.append(hcat(text(f'{short_id(uid)} := '), term2doc(term)))
    for uid, ty in ty_subst:
        binds.append(hcat(text(f"'{short_id(uid)} := "), type2doc(ty)))
    return join(hcat(text(','), line), binds)


# Main recursion
# ==============


def proof2doc(p: xtype.Proof_Proof_term_t_poly) -> Doc:
    """Render a proof term as a derivation tree."""
    concl = doc_of_sequent(p.concl)
    v = p.view
    match v:
        case xtype.Proof_View_T_assume():
            return _tag('assume', concl)

        case xtype.Proof_View_T_rule(rule=r, args=args):
            header = _tag(f'rule {r!r}', concl)
            if not args:
                return header
            return tree(header, [_arg2doc(a) for a in args])

        case xtype.Proof_View_T_subst(
            t_subst=t_subst, ty_subst=ty_subst, premise=premise
        ):
            substs = _subst2doc(t_subst, ty_subst)
            label = (
                group(hcat(text('subst '), substs))
                if (t_subst or ty_subst)
                else text('subst')
            )
            header = hcat(text('['), label, text('] '), concl)
            return tree(header, [proof2doc(premise)])

        case xtype.Proof_View_T_deduction(premises=premises):
            header = _tag('deduction', concl)
            groups: list[Doc] = []
            for name, proofs in premises:
                group_label = text(name) if name else text('•')
                groups.append(tree(group_label, [proof2doc(x) for x in proofs]))
            return tree(header, groups)

    raise ValueError(f'Unhandled proof view: {type(v).__name__}')

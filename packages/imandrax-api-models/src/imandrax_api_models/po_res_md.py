# pyright: basic
"""
Markdown rendering of PO_res types.

This module provides functions to render proof obligation results
to human-readable markdown format.
"""

from __future__ import annotations

from imandrax_api.lib import (
    Anchor,
    Anchor_Decomp,
    Anchor_Eval,
    Anchor_Named,
    Anchor_Proof_check,
    Common_Model_t_poly,
    Common_Model_ty_def_Ty_alias_unit,
    Common_Model_ty_def_Ty_finite,
    Const_Const_bool,
    Const_Const_float,
    Const_Const_q,
    Const_Const_string,
    Const_Const_z,
    Error_Error_core,
    Mir_Sequent,
    Mir_Term,
    Mir_Term_view_Apply,
    Mir_Term_view_Case,
    Mir_Term_view_Const,
    Mir_Term_view_Construct,
    Mir_Term_view_Destruct,
    Mir_Term_view_Field,
    Mir_Term_view_If,
    Mir_Term_view_Is_a,
    Mir_Term_view_Record,
    Mir_Term_view_Sequence,
    Mir_Term_view_Sym,
    Mir_Term_view_Tuple,
    Mir_Term_view_Tuple_field,
    Mir_Term_view_Var,
    Mir_Type,
    Sub_anchor,
    Tasks_PO_res_error_Error,
    Tasks_PO_res_error_Invalid_model,
    Tasks_PO_res_error_No_proof,
    Tasks_PO_res_error_Unsat,
    Tasks_PO_res_full,
    Tasks_PO_res_Shallow,
    Tasks_PO_res_success_Instance,
    Tasks_PO_res_success_Proof,
    Tasks_PO_res_success_Verified_upto,
    Upto_N_steps,
)


def anchor_name(anchor: Anchor) -> str:
    """Convert an Anchor to a human-readable name."""
    match anchor:
        case Anchor_Named(arg=cname):
            return cname.name
        case Anchor_Eval(arg=i):
            return f'Eval {i}'
        case Anchor_Proof_check(arg=inner):
            return f'Proof check of {anchor_name(inner)}'
        case Anchor_Decomp(arg=inner):
            return f'Decomp of {anchor_name(inner)}'
        case _:
            return str(anchor)


def format_upto(upto: Upto_N_steps) -> str:
    """Format an Upto value."""
    match upto:
        case Upto_N_steps(arg=n):
            return f'{n} steps'
        case _:
            return str(upto)


def format_sub_anchor(sa: Sub_anchor) -> str:
    """Format a Sub_anchor."""
    return f'{sa.fname}#{sa.anchor}'


def format_error(err: Error_Error_core) -> str:
    """Format an error to a string."""
    # The error has msg field which is a list of message items
    msg_parts = []
    for item in err.msg:
        if hasattr(item, 'arg'):
            msg_parts.append(str(item.arg))
        else:
            msg_parts.append(str(item))
    return ' '.join(msg_parts) if msg_parts else str(err.kind)


def format_term(term: Mir_Term) -> str:
    """
    Format a term to a string representation.

    This is a simplified representation - a full pretty printer would need
    to handle all term view variants.
    """
    view = term.view
    match view:
        case Mir_Term_view_Const(arg=c):
            return format_const(c)
        case Mir_Term_view_Var(arg=v):
            return format_var(v)
        case Mir_Term_view_Sym(arg=sym):
            return format_applied_symbol(sym)
        case Mir_Term_view_If(args=(cond, then_br, else_br)):
            return f'(if {format_term(cond)} then {format_term(then_br)} else {format_term(else_br)})'
        case Mir_Term_view_Apply(args=(f_term, args_list)):
            args_str = ' '.join(format_term(a) for a in args_list)
            return f'({format_term(f_term)} {args_str})'
        case Mir_Term_view_Construct(args=(cstor, args_list, _labels)):
            if not args_list:
                return format_applied_symbol(cstor)
            args_str = ', '.join(format_term(a) for a in args_list)
            return f'{format_applied_symbol(cstor)}({args_str})'
        case Mir_Term_view_Is_a(args=(cstor, t)):
            return f'(is_{format_applied_symbol(cstor)} {format_term(t)})'
        case Mir_Term_view_Tuple(arg=items):
            items_str = ', '.join(format_term(i) for i in items)
            return f'({items_str})'
        case Mir_Term_view_Field(args=(field, t)):
            return f'{format_term(t)}.{format_applied_symbol(field)}'
        case Mir_Term_view_Tuple_field(args=(idx, t)):
            return f'{format_term(t)}.{idx}'
        case Mir_Term_view_Record(args=(rows, rest)):
            rows_str = ', '.join(
                f'{format_applied_symbol(f)} = {format_term(v)}' for f, v in rows
            )
            if rest is not None:
                return f'{{ {rows_str}, ..{format_term(rest)} }}'
            return f'{{ {rows_str} }}'
        case Mir_Term_view_Case(args=(scrutinee, cases, default)):
            cases_str = ' | '.join(
                f'{format_applied_symbol(c)} -> {format_term(body)}'
                for c, body in cases
            )
            if default is not None:
                cases_str += f' | _ -> {format_term(default)}'
            return f'(match {format_term(scrutinee)} with {cases_str})'
        case Mir_Term_view_Destruct(args=(cstor, idx, t)):
            return f'(destruct_{format_applied_symbol(cstor)}_{idx} {format_term(t)})'
        case Mir_Term_view_Sequence(args=(prefix, body)):
            prefix_str = '; '.join(format_term(p) for p in prefix)
            return f'({prefix_str}; {format_term(body)})'
        case _:
            return f'<term:{type(view).__name__}>'


def format_const(const) -> str:
    """Format a constant."""
    match const:
        case Const_Const_z(arg=i):
            return str(i)
        case Const_Const_q(arg=r):
            return str(r)
        case Const_Const_float(arg=f):
            return str(f)
        case Const_Const_string(arg=s):
            return f'"{s}"'
        case Const_Const_bool(arg=b):
            return 'true' if b else 'false'
        case _:
            return f'<const:{type(const).__name__}>'


def format_var(var) -> str:
    """Format a variable."""
    uid = var.id
    # Try to get the name from the uid view
    if hasattr(uid, 'view'):
        view = uid.view
        if hasattr(view, 'name'):
            return view.name
        if hasattr(view, 'cname') and hasattr(view.cname, 'name'):
            return view.cname.name
    return f'v_{uid}'


def format_applied_symbol(sym) -> str:
    """Format an applied symbol."""
    if hasattr(sym, 'sym'):
        inner = sym.sym
        if hasattr(inner, 'name'):
            return inner.name
        if hasattr(inner, 'view'):
            view = inner.view
            if hasattr(view, 'name'):
                return view.name
            if hasattr(view, 'cname') and hasattr(view.cname, 'name'):
                return view.cname.name
    return '<sym>'


def format_sequent(seq: Mir_Sequent) -> str:
    """
    Format a sequent to a string.

    Format: H0. hyp0
            H1. hyp1
            |------
            C0. concl0
    """
    lines = []

    for i, hyp in enumerate(seq.hyps):
        lines.append(f'H{i}. {format_term(hyp)}')

    lines.append('|' + '-' * 70)

    if not seq.concls:
        lines.append('false')
    elif len(seq.concls) == 1:
        lines.append(format_term(seq.concls[0]))
    else:
        for i, concl in enumerate(seq.concls):
            lines.append(f'C{i}. {format_term(concl)}')

    return '\n'.join(lines)


def format_model(model: Common_Model_t_poly) -> str:
    """Format a model (counterexample) to a string."""
    lines = []

    if model.tys:
        lines.append('Types:')
        for ty, ty_def in model.tys:
            lines.append(f'  {format_type(ty)} = {format_ty_def(ty_def)}')

    if model.consts:
        lines.append('Constants:')
        for sym, val in model.consts:
            lines.append(f'  {format_applied_symbol(sym)} = {format_term(val)}')

    if model.funs:
        lines.append('Functions:')
        for sym, fi in model.funs:
            lines.append(f'  {format_applied_symbol(sym)}:')
            for guards, rhs in fi.fi_cases:
                guards_str = ', '.join(format_term(g) for g in guards)
                lines.append(f'    [{guards_str}] -> {format_term(rhs)}')
            lines.append(f'    else -> {format_term(fi.fi_else)}')

    if not lines:
        lines.append('(empty model)')

    return '\n'.join(lines)


def format_type(ty: Mir_Type) -> str:
    """Format a type to a string."""
    # Simplified - would need full handling of type views
    if hasattr(ty, 'view'):
        view = ty.view
        if hasattr(view, 'name'):
            return view.name
        if hasattr(view, 'sym'):
            return format_applied_symbol(view.sym)
    return '<type>'


def format_ty_def(ty_def) -> str:
    """Format a type definition from a model."""
    match ty_def:
        case Common_Model_ty_def_Ty_finite(arg=elements):
            elems_str = ', '.join(format_term(e) for e in elements)
            return f'{{ {elems_str} }}'
        case Common_Model_ty_def_Ty_alias_unit(arg=ty):
            return f'alias of {format_type(ty)}'
        case _:
            return str(ty_def)


def to_markdown(po_res: Tasks_PO_res_full, *, unicode: bool = True) -> str:
    """
    Render a Tasks_PO_res_full to markdown.

    Args:
        po_res: The proof obligation result to render
        unicode: If True, use unicode symbols (default True)

    Returns:
        A markdown string representation of the result

    """
    lines = []
    name = anchor_name(po_res.from_.anchor)
    lines.append(f'## {name}')
    lines.append('')

    res = po_res.res
    match res:
        case Tasks_PO_res_success_Proof():
            if unicode:
                lines.append('**Result:** Proved')
            else:
                lines.append('**Result:** QED')

        case Tasks_PO_res_success_Instance(arg=inst):
            lines.append('**Result:** Instance found')
            lines.append('')
            lines.append('### Model')
            lines.append('')
            lines.append('```')
            lines.append(format_model(inst.model))
            lines.append('```')

        case Tasks_PO_res_success_Verified_upto(arg=vu):
            lines.append(f'**Result:** Verified up to {format_upto(vu.upto)}')

        case Tasks_PO_res_error_No_proof(arg=np):
            lines.append('**Result:** No proof found')
            lines.append('')
            lines.append(f'**Error:** {format_error(np.err)}')
            lines.append('')

            if np.counter_model is not None:
                lines.append('### Counter-model')
                lines.append('')
                lines.append('```')
                lines.append(format_model(np.counter_model))
                lines.append('```')
                lines.append('')

            k = len(np.subgoals)
            if k > 0:
                lines.append('### Remaining subgoals')
                lines.append('')
                lines.append(f'{k} subgoal{"s" if k > 1 else ""}:')
                lines.append('')
                for sg in np.subgoals:
                    lines.append('```')
                    lines.append(format_sequent(sg))
                    lines.append('```')

        case Tasks_PO_res_error_Unsat(arg=us):
            lines.append('**Result:** Unsat (no instance exists)')
            lines.append('')
            lines.append(f'**Error:** {format_error(us.err)}')

        case Tasks_PO_res_error_Invalid_model(args=(err, model)):
            lines.append('**Result:** Invalid model')
            lines.append('')
            lines.append(f'**Error:** {format_error(err)}')
            lines.append('')
            lines.append('### Model')
            lines.append('')
            lines.append('```')
            lines.append(format_model(model))
            lines.append('```')

        case Tasks_PO_res_error_Error(arg=err):
            lines.append('**Result:** Error')
            lines.append('')
            lines.append(f'**Error:** {format_error(err)}')

        case _:
            lines.append(f'**Result:** Unknown result type: {type(res).__name__}')

    # Sub-results if any
    if po_res.sub_res:
        lines.append('')
        lines.append('### Sub-results')
        lines.append('')
        for i, sub_res_list in enumerate(po_res.sub_res):
            if sub_res_list:
                lines.append(f'#### Step {i + 1}')
                lines.append('')
                for sr in sub_res_list:
                    lines.append(f'- **{format_sub_anchor(sr.sub_anchor)}**')
                    goal_str = format_sequent(sr.goal).replace('\n', ' ').strip()
                    lines.append(f'  - Goal: `{goal_str}`')
                    n_subgoals = len(sr.sub_goals)
                    if n_subgoals > 0:
                        lines.append(
                            f'  - {n_subgoals} subgoal{"s" if n_subgoals > 1 else ""} remaining'
                        )
                    if sr.res is None:
                        lines.append('  - Status: OK')
                    else:
                        lines.append(f'  - Status: Error - {sr.res}')

    return '\n'.join(lines)


def shallow_to_markdown(po_res: Tasks_PO_res_Shallow, *, unicode: bool = True) -> str:
    """
    Render a Tasks_PO_res_Shallow to markdown.

    This version has less detail since the PO itself is a pointer.

    Args:
        po_res: The shallow proof obligation result to render
        unicode: If True, use unicode symbols (default True)

    Returns:
        A markdown string representation of the result

    """
    lines = []
    lines.append('## Proof Obligation Result')
    lines.append('')

    res = po_res.res
    match res:
        case Tasks_PO_res_success_Proof():
            if unicode:
                lines.append('**Result:** Proved')
            else:
                lines.append('**Result:** QED')

        case Tasks_PO_res_success_Instance(arg=inst):
            lines.append(f'**Result:** Instance found for {anchor_name(inst.anchor)}')
            lines.append('')
            lines.append('### Model')
            lines.append('')
            lines.append('```')
            lines.append(format_model(inst.model))
            lines.append('```')

        case Tasks_PO_res_success_Verified_upto(arg=vu):
            lines.append(
                f'**Result:** {anchor_name(vu.anchor)} verified up to {format_upto(vu.upto)}'
            )

        case Tasks_PO_res_error_No_proof(arg=np):
            lines.append('**Result:** No proof found')
            lines.append('')
            lines.append(f'**Error:** {format_error(np.err)}')
            lines.append('')

            if np.counter_model is not None:
                lines.append('### Counter-model')
                lines.append('')
                lines.append('```')
                lines.append(format_model(np.counter_model))
                lines.append('```')
                lines.append('')

            k = len(np.subgoals)
            if k > 0:
                lines.append('### Remaining subgoals')
                lines.append('')
                lines.append(f'{k} subgoal{"s" if k > 1 else ""}:')
                lines.append('')
                for sg in np.subgoals:
                    lines.append('```')
                    lines.append(format_sequent(sg))
                    lines.append('```')

        case Tasks_PO_res_error_Unsat(arg=us):
            lines.append(
                f'**Result:** {anchor_name(us.anchor)} is Unsat (no instance exists)'
            )
            lines.append('')
            lines.append(f'**Error:** {format_error(us.err)}')

        case Tasks_PO_res_error_Invalid_model(args=(err, model)):
            lines.append('**Result:** Invalid model')
            lines.append('')
            lines.append(f'**Error:** {format_error(err)}')
            lines.append('')
            lines.append('### Model')
            lines.append('')
            lines.append('```')
            lines.append(format_model(model))
            lines.append('```')

        case Tasks_PO_res_error_Error(arg=err):
            lines.append('**Result:** Error')
            lines.append('')
            lines.append(f'**Error:** {format_error(err)}')

        case _:
            lines.append(f'**Result:** Unknown result type: {type(res).__name__}')

    return '\n'.join(lines)

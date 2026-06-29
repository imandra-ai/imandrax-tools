import imandrax_api.lib as xtype

# TODO: unify variable naming style
type Term = xtype.Mir_Term_term
type Type = xtype.Mir_Type
type Region = xtype.Common_Region_t_poly[Term, Type]
type RegionMeta = xtype.Common_Region_meta[Term]
type RegionStatus = xtype.Common_Region_status[Term, Type]
type FunDecomp = xtype.Mir_Fun_decomp  # = Common_Fun_decomp_t_poly[Term, Type]
type Sequent = xtype.Common_Sequent_t_poly[xtype.Mir_Term]
type AssocList[T] = list[tuple[str, T]]
type PO_res = xtype.Tasks_PO_res_Shallow
type PO_task = xtype.Tasks_PO_task_Mir
type ProofObligation = xtype.Common_Proof_obligation_t_poly

__all__ = (
    'AssocList',
    'Term',
    'Type',
    'Region',
    'RegionMeta',
    'RegionStatus',
    'FunDecomp',
    'Sequent',
    'PO_res',
    'PO_task',
    'ProofObligation',
)


def fmt_duration(s: float) -> str:
    if s < 0:
        return '?'
    if s < 1e-6:
        return f'{s * 1e9:.0f}ns'
    if s < 1e-3:
        return f'{s * 1e6:.0f}µs'
    if s < 1:
        return f'{s * 1e3:.1f}ms'
    if s >= 1e6:
        # Implausibly large (>~11 days): the span timestamps are unreliable, so
        # fall back to scientific notation to bound the rendered width.
        return f'{s:.2e}s'
    return f'{s:.2f}s'

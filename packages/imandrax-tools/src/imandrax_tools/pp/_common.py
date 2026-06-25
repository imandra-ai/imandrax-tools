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
)

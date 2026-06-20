import imandrax_api.lib as xtype

type Term = xtype.Mir_Term_term
type Type = xtype.Mir_Type
type Region = xtype.Common_Region_t_poly[Term, Type]
type RegionMeta = xtype.Common_Region_meta[Term]
type RegionStatus = xtype.Common_Region_status[Term, Type]
type FunDecomp = xtype.Mir_Fun_decomp  # = Common_Fun_decomp_t_poly[Term, Type]

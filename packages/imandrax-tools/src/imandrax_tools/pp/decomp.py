import imandrax_api.lib as xtypes

from . import pretty as Pp
from ._common import Region, RegionMeta
from .term_formatter import term2doc

type AssocList[T] = list[tuple[str, T]]


def region_meta2doc(meta: RegionMeta) -> Pp.Doc:
    match meta:
        case xtypes.Common_Region_meta_Null():
            return Pp.text('null')
        case xtypes.Common_Region_meta_Bool(arg):
            return Pp.text(str(arg))
        case xtypes.Common_Region_meta_Int(arg):
            return Pp.text(str(arg))
        case xtypes.Common_Region_meta_String(arg):
            return Pp.text(arg)
        case xtypes.Common_Region_meta_Real((nom, den)):
            return Pp.text(f'{nom}/{den}')
        case xtypes.Common_Region_meta_Term(t):
            return term2doc(t)
        case xtypes.Common_Region_meta_List(terms):
            return Pp.list_doc([region_meta2doc(t) for t in terms])
        case xtypes.Common_Region_meta_Assoc(assoc_lst):
            assoc_lst_: AssocList[Pp.Doc] = [
                (k, region_meta2doc(v)) for (k, v) in assoc_lst
            ]
            return Pp.assoc_list(assoc_lst_)


def region_meta_assoc2doc(assoc: list[tuple[str, RegionMeta]]) -> Pp.Doc:
    assoc_lst_: list[tuple[str, Pp.Doc]] = [(k, region_meta2doc(v)) for (k, v) in assoc]
    return Pp.assoc_list(assoc_lst_)


def region2doc(region: Region) -> Pp.Doc:
    assoc_lst: AssocList[Pp.Doc] = [
        ('constraints', Pp.list_doc([term2doc(t) for t in region.constraints])),
        ('invariant', term2doc(region.invariant)),
        ('meta', region_meta_assoc2doc(region.meta)),
    ]
    return Pp.assoc_list(assoc_lst)

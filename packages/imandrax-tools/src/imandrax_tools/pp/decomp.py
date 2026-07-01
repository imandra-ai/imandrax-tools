import imandrax_api.lib as xtypes

from . import pretty as Pp
from ._common import AssocList, Region, RegionMeta
from .term_formatter import term2doc


def drop_meta_paths[T](
    assoc: AssocList[xtypes.Common_Region_meta[T]],
    ignore_path: set[tuple[str, ...]],
    cum_path: tuple[str, ...] = (),
) -> AssocList[xtypes.Common_Region_meta[T]]:
    """
    Drop entries whose dotted key-path is in `ignore_path`, recursing into `Assoc`.

    e.g. `ignore_path={('str', 'invariant')}` removes the `invariant` entry
    nested under the top-level `str` assoc. Filtering the data up front keeps
    the printers below path-agnostic. Generic over the term type since it only
    rebuilds the assoc structure.
    """
    out: AssocList[xtypes.Common_Region_meta[T]] = []
    for k, v in assoc:
        path = (*cum_path, k)
        if path in ignore_path:
            continue
        if isinstance(v, xtypes.Common_Region_meta_Assoc):
            v = xtypes.Common_Region_meta_Assoc(
                drop_meta_paths(v.arg, ignore_path, path)
            )
        out.append((k, v))
    return out


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
            assoc_lst: AssocList[RegionMeta]
            return region_meta_assoc2doc(assoc_lst)


def region_meta_assoc2doc(assoc: AssocList[RegionMeta]) -> Pp.Doc:
    assoc_lst_: list[tuple[str, Pp.Doc]] = [(k, region_meta2doc(v)) for (k, v) in assoc]
    return Pp.assoc_list(assoc_lst_)


def region2doc(region: Region) -> Pp.Doc:
    assoc_lst: AssocList[Pp.Doc] = [
        ('constraints', Pp.list_doc([term2doc(t) for t in region.constraints])),
        ('invariant', term2doc(region.invariant)),
        ('meta', region_meta_assoc2doc(region.meta)),
    ]
    return Pp.assoc_list(assoc_lst)

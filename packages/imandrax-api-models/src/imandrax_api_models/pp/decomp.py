# pyright: basic
from collections.abc import Callable
from dataclasses import fields
from typing import Any, cast

import imandrax_api.lib as xtype

from . import pretty as Pp
from ._common import AssocList, Region, RegionMeta
from .pretty import Doc, python_obj
from .term_formatter import term2doc as term2doc_, terms2doc


def term2doc(t: xtype.Mir_Term_term) -> Pp.Doc:
    return Pp.python_obj('Term', [(None, Pp.python_quote(term2doc_(t)))])


def drop_meta_paths[T](
    assoc: AssocList[xtype.Common_Region_meta[T]],
    ignore_path: set[tuple[str, ...]],
    cum_path: tuple[str, ...] = (),
) -> AssocList[xtype.Common_Region_meta[T]]:
    """
    Drop entries whose dotted key-path is in `ignore_path`, recursing into `Assoc`.

    e.g. `ignore_path={('str', 'invariant')}` removes the `invariant` entry
    nested under the top-level `str` assoc. Filtering the data up front keeps
    the printers below path-agnostic. Generic over the term type since it only
    rebuilds the assoc structure.

    No-op if the ignored path is not found at all.
    """
    out: AssocList[xtype.Common_Region_meta[T]] = []
    for k, v in assoc:
        path = (*cum_path, k)
        if path in ignore_path:
            continue
        if isinstance(v, xtype.Common_Region_meta_Assoc):
            v = xtype.Common_Region_meta_Assoc(
                drop_meta_paths(v.arg, ignore_path, path)
            )
        out.append((k, v))
    return out


def region_meta2doc(meta: RegionMeta) -> Pp.Doc:
    match meta:
        case xtype.Common_Region_meta_Null():
            return Pp.text('null')
        case xtype.Common_Region_meta_Bool(arg):
            return Pp.text(str(arg))
        case xtype.Common_Region_meta_Int(arg):
            return Pp.text(str(arg))
        case xtype.Common_Region_meta_String(arg):
            return Pp.text(arg)
        case xtype.Common_Region_meta_Real((nom, den)):
            return Pp.text(f'{nom}/{den}')
        case xtype.Common_Region_meta_Term(t):
            return term2doc(t)
        case xtype.Common_Region_meta_List(terms):
            return Pp.list_doc([region_meta2doc(t) for t in terms])
        case xtype.Common_Region_meta_Assoc(assoc_lst):
            assoc_lst: AssocList[RegionMeta]
            return region_meta_assoc2doc(assoc_lst)


def region_meta_assoc2doc(assoc: AssocList[RegionMeta]) -> Pp.Doc:
    assoc_lst_: list[tuple[str, Pp.Doc]] = [(k, region_meta2doc(v)) for (k, v) in assoc]
    return Pp.assoc_list(assoc_lst_)


def region2doc(region: Region, value2doc: Callable[[Any], Doc]) -> Pp.Doc:
    """
    A concise doc repr of the region.

    A few things are performed to make the region repr concise
    - constraint terms are shown in a `Terms` object form
    - meta: only model and model_eval are shown at a higher level
        - .str are omitted since it duplicated region's constraints and invariant
        - .id, .merge_src, .merge_tgt are omitted
    """
    rows: AssocList[Doc] = []
    for fld in fields(region):
        key = fld.name
        val = getattr(region, key)
        if key == 'constraints':
            val = cast(list[xtype.Mir_Term], val)
            rows.append(('constraints', terms2doc(val)))
        elif key == 'meta':
            val = cast(AssocList[xtype.Common_Region_meta], val)
            for meta_k, meta_v in val:
                if meta_k in ['model', 'model_eval']:
                    rows.append((meta_k, region_meta2doc(meta_v)))
        else:
            rows.append((key, value2doc(val)))
    return python_obj('Region', rows)

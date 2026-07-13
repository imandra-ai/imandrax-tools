from __future__ import annotations

from collections.abc import Mapping, Sequence

import imandrax_api.lib as xtype

from imandrax_api_models.pp.pretty import pretty
from imandrax_api_models.pp.term_formatter import term2doc

type AssocList[T] = list[tuple[str, T]]
type JSONValue = (
    str | int | float | bool | None | Mapping[str, JSONValue] | Sequence[JSONValue]
)
type JSONObject = dict[str, JSONValue]
type JSONArray = list[JSONValue]


def term_to_string(t: xtype.Mir_Term) -> str:
    return pretty(88, term2doc(t))

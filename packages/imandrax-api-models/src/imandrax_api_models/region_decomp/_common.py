from __future__ import annotations

from collections.abc import Callable, Mapping, Sequence
from dataclasses import asdict, dataclass, fields, is_dataclass
from functools import reduce
from typing import Annotated, Any, Self, TypedDict

import imandrax_api.lib as xtype
from devtools import pformat
from pydantic import BaseModel, Field, PlainSerializer, model_validator

from imandrax_api_models.pp.pretty import pretty
from imandrax_api_models.pp.term_formatter import term2doc
from imandrax_api_models.pp.xtype import to_string as xtype_to_string
from imandrax_api_models.proto_models import Art, DecomposeRes

type AssocList[T] = list[tuple[str, T]]
type JSONValue = (
    str | int | float | bool | None | Mapping[str, JSONValue] | Sequence[JSONValue]
)
type JSONObject = dict[str, JSONValue]
type JSONArray = list[JSONValue]

def term_to_string(t: xtype.Mir_Term) -> str:
    return pretty(88, term2doc(t))
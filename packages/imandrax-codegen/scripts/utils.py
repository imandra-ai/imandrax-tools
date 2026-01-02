# pyright: basic
import base64
import json
import os
import re
import subprocess
from pathlib import Path
from typing import Any, Literal, cast

import dotenv
import py_gen.ast_types as ast_types
from google.protobuf.json_format import MessageToDict
from google.protobuf.message import Message
from imandrax_api import url_dev, url_prod
from imandrax_api.lib import (
    Artifact,
    Common_Decl_t_poly_Fun,
    Common_Fun_def_t_poly,
    Mir_Decl,
    Mir_Term_term,
    Mir_Type,
    Mir_Type_var,
    Ty_view_view,
    Ty_view_view_Arrow,
    Ty_view_view_Constr,
    read_artifact_data,
)
from imandrax_api_models import Art
from imandrax_api_models.client import ImandraXClient
from IPython.core.getipython import get_ipython
from py_gen.ast_deserialize import load_from_json_string
from py_gen.unparse import unparse
from rich import print


def parse_decl_fun(mir_decl: Artifact) -> tuple[str, list[str]]:
    """Parse a Mir_Decl.Fun_def artifact.

    Returns:
        tuple[str, list[str]]: The function name and the list of argument types.
    """
    if not isinstance(mir_decl, Common_Decl_t_poly_Fun):
        raise TypeError(f'Expected Mir_Decl.Fun_def, got {type(mir_decl)}')
    fun_def: Fun_def = mir_decl.arg
    func_name: str = fun_def.f_name.name

    func_ty: Mir_Type = fun_def.f_ty.ty
    func_ty_view: Ty_view_view[None, Mir_Type_var, Mir_Type] = func_ty.view

    return func_name, unpack_arrow(func_ty_view)


def unpack_arrow(
    ty_view: Ty_view_view[None, Mir_Type_var, Mir_Type],
) -> list[str]:
    """Flatten the arrow type view to a list of types (strings)."""
    result = []

    def helper(view: Any) -> None:
        if isinstance(view, Ty_view_view_Arrow):
            # Extract args: (annotations?, left_t, right_t)
            _, left_t, right_t = view.args
            # Assumption: left_t is a Mir_Type
            left_view = left_t.view
            if isinstance(left_view, Ty_view_view_Constr):
                uid, _ = left_view.args
                result.append(uid.name)
            else:
                raise ValueError('Never: left of arrow type view should be a constr')
            # Recurse on right_t (which is a Mir_Type)
            helper(right_t.view)
        elif isinstance(view, Ty_view_view_Constr):
            uid, _ = view.args
            result.append(uid.name)
        else:
            raise ValueError(
                'Never: arrow type view should be either a constr or an arrow'
            )

    helper(ty_view)
    return result

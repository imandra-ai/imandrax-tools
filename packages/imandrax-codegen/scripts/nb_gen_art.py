# pyright: basic
# %%
import re
import subprocess
from typing import Literal, cast

from imandrax_api import Client, url_dev, url_prod
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
from rich import print

if ip := get_ipython():
    ip.run_line_magic('reload_ext', 'autoreload')
    ip.run_line_magic('autoreload', '2')
import base64
import json
import os
from pathlib import Path
from typing import Any

import dotenv
import imandrax_codegen.ast_types as ast_types
from google.protobuf.json_format import MessageToDict
from google.protobuf.message import Message
from imandrax_codegen.ast_deserialize import stmts_of_json
from imandrax_codegen.unparse import unparse

curr_dir = Path.cwd() if ip else Path(__file__).parent


dotenv.load_dotenv()
PROJECT_DIR = curr_dir / '..'
CODEGEN_EXE_PATH = PROJECT_DIR / '_build' / 'default' / 'bin' / 'parse.exe'


def proto_to_dict(proto_obj: Message) -> dict[Any, Any]:
    return MessageToDict(
        proto_obj,
        preserving_proto_field_name=True,
        always_print_fields_with_no_presence=True,
    )


# %%

c = Client(
    auth_token=os.environ['IMANDRAX_API_KEY'],
    # url=url_prod,
    url=url_dev,
)


# c = ImandraXClient(
#     auth_token=os.environ['IMANDRAX_API_KEY'],
#     url=url_prod,
# )


# %%
iml = """
type 'a identity = Identity of 'a

type 'a maybe =
  | Just of 'a
  | Nothing

type 'a validated =
  | Valid of 'a
  | Invalid of string

type 'a tagged = {
  value : 'a;
  tag : string
}

type my_ty =
  | My_ty of int identity maybe validated tagged
"""
c.eval_src(iml)
decl_res1 = c.get_decls(['my_ty'])


iml = """
type my_ty = {
  x: real;
  y: int option;
  z: int;
}
"""
c.eval_src(iml)
decl_res2 = c.get_decls(['my_ty'])

# %%
decl_res1.decls[0].artifact == decl_res2.decls[0].artifact

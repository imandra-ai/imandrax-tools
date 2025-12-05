# pyright: basic
# %%
import subprocess
from typing import cast

from imandrax_api import url_dev
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
from google.protobuf.json_format import MessageToDict
from google.protobuf.message import Message
from py_gen.ast_deserialize import load_from_json_string
from py_gen.unparse import unparse

curr_dir = Path.cwd() if ip else Path(__file__).parent


def proto_to_dict(proto_obj: Message) -> dict[Any, Any]:
    return MessageToDict(
        proto_obj,
        preserving_proto_field_name=True,
        always_print_fields_with_no_presence=True,
    )


def convert_to_standard_base64(data: str | bytes) -> str:
    """Convert bytes or URL-safe base64 string to standard base64.

    Handles two cases:
    1. If data is bytes: directly encode to standard base64
    2. If data is a URL-safe base64 string: convert to standard base64

    Pydantic serializes bytes as URL-safe base64 (using - and _ instead of + and /),
    but OCaml's Base64.decode_exn expects standard base64 encoding.

    Args:
        data: Either raw bytes or URL-safe base64 string

    Returns:
        Standard base64 string
    """
    if isinstance(data, bytes):
        # Directly encode bytes to standard base64
        return base64.b64encode(data).decode('ascii')

    # It's a string - assume it's URL-safe base64
    # Add padding if needed
    padding = (4 - len(data) % 4) % 4
    urlsafe_b64_padded = data + ('=' * padding)

    # Decode URL-safe and re-encode as standard base64
    decoded_bytes = base64.urlsafe_b64decode(urlsafe_b64_padded)
    return base64.b64encode(decoded_bytes).decode('ascii')


dotenv.load_dotenv()
PROJECT_DIR = curr_dir / '..'
CODEGEN_EXE_PATH = PROJECT_DIR / '_build' / 'default' / 'bin' / 'parse.exe'

c = ImandraXClient(
    auth_token=os.environ['IMANDRAX_API_KEY'],
    url=url_dev,
)

# %%
iml = """\
type direction = North | South | East | West

type position = { x: int; y: int; z: real }

type movement =
  | Stay of position
  | Move of position * direction

let move = fun w ->
  match w with
  | Stay p -> p
  | Move (p, d) ->
    let x, y, z = p.x, p.y, p.z in
    let x, y, z =
      match d with
      | North -> (x, y+1, z)
      | South -> (x, y-1, z)
      | East -> (x+1, y, z)
      | West -> (x-1, y, z)
    in
    { x; y; z }\
"""

name = 'move'
eval_res = c.eval_src(iml)

# %%
fun_name = 'move'
decomp_res = c.decompose(fun_name)

# %%
art = decomp_res.artifact
assert art

art_dict = art.model_dump()
art_dict['data'] = convert_to_standard_base64(art_dict['data'])

# Convert storage entries if any
for entry in art_dict.get('storage', []):
    if 'value' in entry:
        entry['value'] = convert_to_standard_base64(entry['value'])

art_json = json.dumps(art_dict)


# %%

# Pipe to the executable
result = subprocess.run(
    [
        CODEGEN_EXE_PATH,
        '-',
        '-',
        '--mode',
        'fun-decomp',
    ],
    check=False,
    input=art_json,
    text=True,
    capture_output=True,
)
ast_stmts = load_from_json_string(result.stdout)
print(unparse(ast_stmts))


# %%


# %%
def get_fun_arg_types(fun_name: str, iml: str) -> list[str] | None:
    tc_res = c.typecheck(iml)
    name_ty_map = {ty.name: ty.ty for ty in tc_res.types}
    if fun_name not in name_ty_map:
        return None
    return list(map(lambda s: s.strip(), name_ty_map[fun_name].split('->')))


get_fun_arg_types('move', iml)

# %%


# %%
gd_res = c.get_decls(names=['movement', 'move'])
decl = [decl for decl in gd_res.decls if decl.name == 'move'][0]  # noqa: RUF015
art = decl.artifact
mir_decl: Artifact = read_artifact_data(art.data, art.kind)
mir_decl = cast(Mir_Decl, mir_decl)

# %%
Fun_def = Common_Fun_def_t_poly[Mir_Term_term, Mir_Type]
fun_def: Fun_def = cast(Fun_def, mir_decl.arg)
func_name: str = fun_def.f_name.name

# Var = Common_Var_t_poly[Mir_Type]
func_ty: Mir_Type = fun_def.f_ty.ty
func_ty_view: Ty_view_view[None, Mir_Type_var, Mir_Type] = func_ty.view


# %%
# print(asdict(art).keys())  # art is not a dataclass


# %%
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


unpack_arrow(func_ty_view)

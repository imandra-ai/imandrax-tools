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
import py_gen.ast_types as ast_types
from google.protobuf.json_format import MessageToDict
from google.protobuf.message import Message
from py_gen.ast_deserialize import load_from_json_string
from py_gen.unparse import unparse

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
let v = Multiset.of_list [1; 2; 3; 2; 1]

let v = fun w -> if w = v then true else false
"""
iml = """
let v : (int, bool) Map.t =
  Map.const false
   |> Map.add 2 true

let v = fun w -> if w = v then true else false
"""
# iml = """
# let v = Set.of_list [1; 2; 3; 2; 1]

# let v = fun w -> if w = v then true else false
# """
c.eval_src(iml)
c.instance_src('v')

# %%


# iml = """
# type event =
#     | Click of { x: int; y: int }
#     | Keypress of { key: LChar.t; modifiers: LString.t list }
#     | Scroll of { delta: real }

# let v = Scroll {delta = 2.0}
# let v = fun w -> if w = v then true else false
# """
# iml = """
# let v : (int, bool) Map.t =
#   Map.const false

# let v = (fun w -> if w = v then true else false)
# """
c.eval_src(iml)
c.instance_src('v')


# %%


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


def get_fun_arg_types(fun_name: str, iml: str, c: ImandraXClient) -> list[str] | None:
    """Get the argument types of a function."""
    tc_res = c.typecheck(iml)
    name_ty_map = {ty.name: ty.ty for ty in tc_res.types}
    if fun_name not in name_ty_map:
        return None

    return list(map(lambda s: s.strip(), name_ty_map[fun_name].split('->')))


def extract_type_decl_names(ml_code: str) -> list[str]:
    """
    Extract all type definition names from OCaml code.

    Args:
        ocaml_code: String containing OCaml code

    Returns:
        List of type names defined in the code

    Examples:
        >>> code = 'type direction = North | South'
        >>> extract_ocaml_type_names(code)
        ['direction']
    """
    # Pattern matches: "type" keyword followed by type name
    # Handles both regular types and recursive types (type ... and ...)
    pattern = r'\btype\s+([a-z_][a-zA-Z0-9_]*(?:\s*,\s*[a-z_][a-zA-Z0-9_]*)*)'

    matches = re.finditer(pattern, ml_code)
    type_names = []

    for match in matches:
        # Extract the captured group (type name(s))
        names = match.group(1)
        # Split by comma in case of mutually recursive types: type t1, t2 = ...
        for name in names.split(','):
            type_names.append(name.strip())

    return type_names


def gen_ast(
    art_json: str,
    mode: Literal['fun-decomp', 'model', 'decl'],
) -> list[ast_types.stmt]:
    result = subprocess.run(
        [
            CODEGEN_EXE_PATH,
            '-',
            '-',
            '--mode',
            mode,
        ],
        check=False,
        input=art_json,
        text=True,
        capture_output=True,
    )
    if result.returncode != 0:
        raise RuntimeError(f'Failed to run generate AST: {result.stderr}')
    return load_from_json_string(result.stdout)


def serialize_artifact(art: Art) -> str:
    art_dict = art.model_dump()
    art_dict['data'] = convert_to_standard_base64(art_dict['data'])
    return json.dumps(art_dict)


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


# %%
fun_name = 'move'

eval_res = c.eval_src(iml)
decomp_res = c.decompose(fun_name)
typecheck_res = c.typecheck(iml)
# arg_types = get_fun_arg_types(fun_name, iml, c)
arg_types = extract_type_decl_names(iml)
assert arg_types

# %%
decls = c.get_decls(arg_types)
decl_arts: list[str] = [serialize_artifact(decl.artifact) for decl in decls.decls]
type_def_stmts_by_decl = [gen_ast(decl_art, mode='decl') for decl_art in decl_arts]
type_def_stmts = [stmt for stmts in type_def_stmts_by_decl for stmt in stmts]


# %%
decomp_art = decomp_res.artifact
assert decomp_art
test_def_stmts = gen_ast(serialize_artifact(decomp_art), mode='fun-decomp')

# %%
gen_code: str = unparse([
    *type_def_stmts,
    *test_def_stmts,
])

with Path('tmp.py').open('w') as f:
    f.write(gen_code)

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

# pyright: basic, reportUnusedExpression=false
# %%
import re
import subprocess
from functools import singledispatch
from typing import Literal, cast

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


c = ImandraXClient(
    auth_token=os.environ['IMANDRAX_API_KEY'],
    # url=url_dev,
    url=url_prod,
)


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


def get_fun_arg_types(fun_name: str, iml: str, c: ImandraXClient) -> list[str] | None:
    """Get the argument types of a function."""
    tc_res = c.typecheck(iml)
    name_ty_map = {ty.name: ty.ty for ty in tc_res.types}
    if fun_name not in name_ty_map:
        return None

    return list(map(lambda s: s.strip(), name_ty_map[fun_name].split('->')))


def extract_type_decl_names(ml_code: str) -> list[str]:
    """
    Extract all type definition names from OCaml code using regex.

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


@singledispatch
def gen_ast(
    art: str | Art, mode: Literal['fun-decomp', 'model', 'decl']
) -> list[ast_types.stmt]:
    raise NotImplementedError(f'Only Art and str are supported, got {type(art)}')


@gen_ast.register
def _base(
    art: str,
    mode: Literal['fun-decomp', 'model', 'decl'],
) -> list[ast_types.stmt]:
    """Use the codegen executable to generate ASTs for a given artifact."""
    result = subprocess.run(
        [
            CODEGEN_EXE_PATH,
            '-',
            '-',
            '--mode',
            mode,
        ],
        check=False,
        input=art,
        text=True,
        capture_output=True,
    )
    if result.returncode != 0:
        raise RuntimeError(f'Failed to run generate AST: {result.stderr}')
    return load_from_json_string(result.stdout)


@gen_ast.register
def _(
    art: Art,
    mode: Literal['fun-decomp', 'model', 'decl'],
) -> list[ast_types.stmt]:
    return gen_ast(serialize_artifact(art), mode)


def serialize_artifact(art: Art) -> str:
    """Serialize an artifact BaseModel to a JSON string."""
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


fun_name = 'move'

eval_res = c.eval_src(iml)
decomp_res = c.decompose(fun_name)
typecheck_res = c.typecheck(iml)
# arg_types = get_fun_arg_types(fun_name, iml, c)
arg_types = extract_type_decl_names(iml)
assert arg_types
arg_types
# %%
typecheck_res

# %%
decls = c.get_decls(['move'])


# %%
decls


# %% type declarations
decls = c.get_decls(arg_types)
type_def_stmts_by_decl = [gen_ast(decl.artifact, mode='decl') for decl in decls.decls]

type_def_stmts = [stmt for stmts in type_def_stmts_by_decl for stmt in stmts]

# %% test function definitions
assert decomp_res.artifact
test_def_stmts = gen_ast(decomp_res.artifact, mode='fun-decomp')
# %% generate code from ASTs
gen_code: str = unparse([
    *type_def_stmts,
    *test_def_stmts,
])

with Path('tmp.py').open('w') as f:
    f.write(gen_code)

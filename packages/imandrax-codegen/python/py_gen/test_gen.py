import base64
import json
import os
import re
import subprocess
from functools import singledispatch
from pathlib import Path
from typing import Literal

import dotenv
import py_gen.ast_types as ast_types
from imandrax_api import url_prod
from imandrax_api_models import Art, EvalRes
from imandrax_api_models.client import ImandraXClient
from py_gen.ast_deserialize import load_from_json_string

curr_dir = Path(__file__).parent


dotenv.load_dotenv()
PROJECT_DIR = curr_dir / '..' / '..'
CODEGEN_EXE_PATH = PROJECT_DIR / '_build' / 'default' / 'bin' / 'parse.exe'

# Utils
# ====================


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
    type_names: list[str] = []

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
def _(
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


# Main
# ====================


def gen_test_cases(iml: str, decomp_name: str) -> list[ast_types.stmt]:
    c = ImandraXClient(
        auth_token=os.environ['IMANDRAX_API_KEY'],
        url=url_prod,
    )

    # Eval IML
    eval_res: EvalRes = c.eval_src(iml)
    if eval_res.success is not True:
        error_msgs = [repr(err.msg) for err in eval_res.errors]
        raise ValueError(f'Failed to evaluate source code: {error_msgs}')

    decomp_res = c.decompose(decomp_name)
    arg_types: list[str] = extract_type_decl_names(iml)

    # Type declarations
    decls = c.get_decls(arg_types)
    type_def_stmts_by_decl = [
        gen_ast(decl.artifact, mode='decl') for decl in decls.decls
    ]
    type_def_stmts = [stmt for stmts in type_def_stmts_by_decl for stmt in stmts]

    # Test function definitions
    assert decomp_res.artifact, 'No artifact returned from decompose'
    test_def_stmts = gen_ast(decomp_res.artifact, mode='fun-decomp')

    return [
        *type_def_stmts,
        *test_def_stmts,
    ]


if __name__ == '__main__':
    import argparse

    from py_gen.unparse import unparse

    DEFAULT_IML = """\
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

    parser = argparse.ArgumentParser(description='Generate test cases for IML.')
    parser.add_argument(
        '-i',
        '--iml-path',
        help='Path of IML file to generate test cases',
    )
    parser.add_argument(
        '-f',
        '--function',
        help='Name of function to generate test cases for',
    )
    args = parser.parse_args()

    match (args.iml_path, args.function):
        case (str(iml_path), str(func)):
            iml = Path(iml_path).read_text()
            f = func
        case _:
            iml = DEFAULT_IML
            f = 'move'

    test_case_stmts = gen_test_cases(iml, f)
    print(unparse(test_case_stmts))

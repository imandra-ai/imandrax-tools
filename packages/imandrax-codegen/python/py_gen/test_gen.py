import os
import re
from pathlib import Path

import dotenv
import py_gen.ast_types as ast_types
from imandrax_api import url_dev, url_prod  # noqa: F401
from imandrax_api_models import EvalRes
from imandrax_api_models.client import ImandraXClient

from .art_parse import ast_of_art

curr_dir = Path(__file__).parent
dotenv.load_dotenv()


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


# Main
# ====================


def gen_test_cases(iml: str, decomp_name: str) -> list[ast_types.stmt]:
    c = ImandraXClient(
        auth_token=os.environ['IMANDRAX_API_KEY'],
        # url=url_dev,
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
        ast_of_art(decl.artifact, mode='decl') for decl in decls.decls
    ]
    type_def_stmts = [stmt for stmts in type_def_stmts_by_decl for stmt in stmts]

    # Test function definitions
    assert decomp_res.artifact, 'No artifact returned from decompose'
    test_def_stmts = ast_of_art(decomp_res.artifact, mode='fun-decomp')

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

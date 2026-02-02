import os
import re
from pathlib import Path
from typing import Any

from imandrax_api import Client, url_dev, url_prod  # noqa: F401
from imandrax_api_models import Art, DecomposeRes, EvalRes  # noqa: F401, RUF100
from imandrax_api_models.client import ImandraXClient
from imandrax_codegen.unparse import unparse

from .art_parse import Lang, ast_of_art, code_of_art

curr_dir = Path(__file__).parent


# TODO: Not used
def _get_fun_arg_types(  # pyright: ignore[reportUnusedFunction]
    fun_name: str,
    iml: str,
    c: ImandraXClient,
) -> list[str] | None:
    """Get the argument types of a function."""
    tc_res = c.typecheck(iml)
    name_ty_map = {ty.name: ty.ty for ty in tc_res.types}
    if fun_name not in name_ty_map:
        return None

    return list(map(lambda s: s.strip(), name_ty_map[fun_name].split('->')))


def _extract_type_decl_names(iml_code: str) -> list[str]:
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
    # Pattern matches: "type" or "and" keyword followed by optional type parameters, then type name
    # Handles both regular types and recursive types (type ... and ...)
    # Also handles parameterized types:
    #   - Single param without parens: type 'a option
    #   - Multi param with parens: type ('a, 'b) container
    #   - Wildcard param: type _ expr (GADTs)
    pattern = r'\b(?:type|and)\s+(?:(?:\([^)]+\)|\'[a-z_][a-zA-Z0-9_]*|_)\s+)?([a-z_][a-zA-Z0-9_]*(?:\s*,\s*[a-z_][a-zA-Z0-9_]*)*)'

    matches = re.finditer(pattern, iml_code)
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


def gen_test_cases(
    iml: str,
    decomp_name: str,
    lang: Lang,
    other_decomp_kwargs: dict[str, Any] | None = None,
) -> str:
    """Decomp, get decl, and generate test cases as source code."""

    other_decomp_kwargs = other_decomp_kwargs or {}

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

    decomp_res: DecomposeRes = c.decompose(decomp_name, **other_decomp_kwargs)
    decomp_art = decomp_res.artifact
    assert decomp_art, 'No artifact returned from decompose'

    arg_types: list[str] = _extract_type_decl_names(iml)

    decls = c.get_decls(arg_types)

    match lang:
        case 'typescript':
            type_def_srcs = [
                code_of_art(decl.artifact, mode='decl', lang=lang)
                for decl in decls.decls
            ]
            test_def_src = code_of_art(decomp_art, mode='fun-decomp', lang=lang)
            return '\n'.join([
                *type_def_srcs,
                test_def_src,
            ])
        case 'python':
            # TODO(#20):
            # Python still needs two-stage generation otherwise
            # we get two `from __future__ import annotations`
            type_defs_stmts = [
                ast_of_art(decl.artifact, mode='decl') for decl in decls.decls
            ]
            type_def_stmts = [stmt for stmts in type_defs_stmts for stmt in stmts]
            test_def_stmts = ast_of_art(decomp_art, mode='fun-decomp')
            code = unparse([
                *type_def_stmts,
                *test_def_stmts,
            ])
            return code

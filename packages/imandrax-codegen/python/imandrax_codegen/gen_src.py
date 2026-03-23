import os
from pathlib import Path
from typing import Any, Literal, assert_never

from imandrax_api import url_dev, url_prod
from imandrax_api_models import (  # noqa: F401, RUF100
    Art,
    DecomposeRes,
    EvalRes,
    GetDeclsRes,
    InstanceRes,
    VerifyRes,
)
from imandrax_api_models.client import ImandraXClient
from imandrax_codegen.unparse import unparse
from iml_query.processing import extract_type_decl_names

from .art_parse import Lang, Mode, ast_of_art, code_of_art

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


class GenSourceCodeError(ValueError):
    pass


def gen_source_code(
    model_res: DecomposeRes | VerifyRes | InstanceRes,
    lang: Lang,
    decls_res: GetDeclsRes,
) -> tuple[str, str] | GenSourceCodeError:
    mode: Mode
    art: Art

    match model_res:
        case DecomposeRes():
            mode = 'fun-decomp'
            if model_res.artifact is None:
                return GenSourceCodeError('No artifact in decompose response')
            art = model_res.artifact
        case VerifyRes():
            mode = 'model'
            if (
                model_res.refuted is None
                or (model_res.refuted.model is None)
                or (model_res.refuted.model.artifact is None)
            ):
                return GenSourceCodeError(
                    'No refuted model artifact in verify response'
                )
            art = model_res.refuted.model.artifact
        case InstanceRes():
            mode = 'model'
            if (
                model_res.sat is None
                or (model_res.sat.model is None)
                or (model_res.sat.model.artifact is None)
            ):
                return GenSourceCodeError(
                    'No satisfiable model artifact in instance response'
                )
            art = model_res.sat.model.artifact

    match lang:
        case 'typescript':
            type_def_srcs = [
                code_of_art(decl.artifact, mode='decl', lang=lang)
                for decl in decls_res.decls
            ]
            src_body = code_of_art(art, mode=mode, lang=lang)
            return (
                '\n'.join(type_def_srcs),
                src_body,
            )
        case 'python':
            # TODO(#20):
            # Python still needs two-stage generation otherwise
            # we get two `from __future__ import annotations`
            type_defs_stmts = [
                ast_of_art(decl.artifact, mode='decl') for decl in decls_res.decls
            ]
            type_def_stmts = [stmt for stmts in type_defs_stmts for stmt in stmts]
            body_stmts = ast_of_art(art, mode=mode)
            type_def_src = (
                unparse(type_def_stmts, include_future_import=True)
                if type_def_stmts
                else ''
            )
            src_body = unparse(body_stmts, include_future_import=False)
            return (type_def_src, src_body)


# Main
# ====================


def gen_test_cases(
    iml: str,
    decomp_name: str,
    lang: Lang,
    other_decomp_kwargs: dict[str, Any] | None = None,
    imandrax_api_key: str | None = None,
    imandrax_env: str | None = None,
) -> tuple[str, str]:
    """Decomp, get decl, and generate test cases as source code.

    Return:
        Tuple of (type declarations, test case definition)
    """

    other_decomp_kwargs = other_decomp_kwargs or {}

    env = imandrax_env or os.getenv('IMANDRAX_ENV', 'prod')
    url = url_dev if env == 'dev' else url_prod

    c = ImandraXClient(
        auth_token=imandrax_api_key or os.environ['IMANDRAX_API_KEY'],
        url=url,
    )

    # Eval IML
    eval_res: EvalRes = c.eval_src(iml)
    if eval_res.success is not True:
        error_msgs = [repr(err.msg) for err in eval_res.errors]
        raise ValueError(f'Failed to evaluate source code: {error_msgs}')

    #  Decomp
    decomp_res: DecomposeRes = c.decompose(decomp_name, **other_decomp_kwargs)
    decomp_art = decomp_res.artifact
    assert decomp_art, 'No artifact returned from decompose'

    # Get type declarations
    arg_types: list[str] = extract_type_decl_names(iml)
    decls: GetDeclsRes = c.get_decls(arg_types)

    src_res = gen_source_code(decomp_res, lang, decls)
    if isinstance(src_res, GenSourceCodeError):
        raise src_res
    else:
        return src_res


def gen_counter_example(
    iml: str,
    vg_src: str,
    vg_type: Literal['verify', 'instance'],
    lang: Lang,
    vg_hint: str | None = None,
    imandrax_api_key: str | None = None,
    imandrax_env: str | None = None,
) -> tuple[str, str]:
    """Decomp, get decl, and generate test cases as source code.

    Return:
        Tuple of (type declarations, test case definition)
    """

    env = imandrax_env or os.getenv('IMANDRAX_ENV', 'prod')
    url = url_dev if env == 'dev' else url_prod

    c = ImandraXClient(
        auth_token=imandrax_api_key or os.environ['IMANDRAX_API_KEY'],
        url=url,
    )

    # Eval IML
    eval_res: EvalRes = c.eval_src(iml)
    if eval_res.success is not True:
        error_msgs = [repr(err.msg) for err in eval_res.errors]
        raise ValueError(f'Failed to evaluate source code: {error_msgs}')

    #  VG
    match vg_type:
        case 'verify':
            model_res = c.verify_src(vg_src, vg_hint)
        case 'instance':
            model_res = c.instance_src(vg_src, vg_hint)
        case _:
            assert_never(vg_type)

    # Get type declarations
    arg_types: list[str] = extract_type_decl_names(iml)
    decls: GetDeclsRes = c.get_decls(arg_types)

    src_res = gen_source_code(model_res, lang, decls)
    if isinstance(src_res, GenSourceCodeError):
        raise src_res
    else:
        return src_res


if __name__ == '__main__':
    import dotenv

    dotenv.load_dotenv()
    iml = """
    let f x = x + 1
    """

    instance_src = 'fun x -> f x = 2'

    res = '\n'.join(
        gen_counter_example(
            iml=iml, vg_src=instance_src, vg_type='instance', lang='python'
        )
    )
    print(res)

#!/usr/bin/env python
import sys
from enum import StrEnum
from pathlib import Path
from typing import Annotated

import typer
from imandrax_codegen.art_parse import code_of_art
from imandrax_codegen.gen_tests import Lang, gen_test_cases

app = typer.Typer()


def _parse_lang(lang: str) -> Lang | Exception:
    if lang.lower() in ('typescript', 'ts'):
        return 'typescript'
    elif lang.lower() in ('python', 'py'):
        return 'python'
    return typer.BadParameter(
        f'Unsupported language: {lang}. Only TypeScript and Python are supported.'
    )


class Mode(StrEnum):
    model = 'model'
    decl = 'decl'
    fun_decomp = 'fun-decomp'


def read_input(path: str) -> str:
    if path == '-':
        return sys.stdin.read()
    return Path(path).read_text()


def write_output(output: str | None, content: str) -> None:
    if output:
        Path(output).write_text(content)
    else:
        typer.echo(content, nl=not content.endswith('\n'))


@app.callback(invoke_without_command=True)
def main(
    ctx: typer.Context,
    lang: Annotated[
        str,
        typer.Option('-l', '--lang', help='Target language'),
    ],
    input_path: Annotated[
        str,
        typer.Option('-i', '--input', help='Input artifact file (use "-" for stdin)'),
    ] = '-',
    output: Annotated[
        str | None,
        typer.Option('-o', '--output', help='Output file path (defaults to stdout)'),
    ] = None,
    mode: Annotated[
        Mode,
        typer.Option('-m', '--mode', help='Parse mode'),
    ] = Mode.model,
) -> None:
    """Generate source code from ImandraX artifact."""
    if ctx.invoked_subcommand is not None:
        return

    content = read_input(input_path)
    target_lang = _parse_lang(lang)
    if isinstance(target_lang, Exception):
        raise target_lang
    result = code_of_art(content, mode.value, target_lang)
    write_output(output, result)


def gen_test_command(
    iml_path: Annotated[
        str,
        typer.Argument(
            help='Path of IML file to generate test cases (use "-" for stdin)'
        ),
    ],
    function: Annotated[
        str,
        typer.Option(
            '-f', '--function', help='Name of function to generate test cases for'
        ),
    ],
    lang: Annotated[
        str,
        typer.Option('-l', '--lang', help='Target language'),
    ],
    output: Annotated[
        str | None,
        typer.Option('-o', '--output', help='Output file path (defaults to stdout)'),
    ] = None,
) -> None:
    """Typer command for generating test cases from IML."""
    iml = read_input(iml_path)
    target_lang = _parse_lang(lang)
    if isinstance(target_lang, Exception):
        raise target_lang

    result = gen_test_cases(iml, function, lang=target_lang)
    write_output(output, result)


app.command()(gen_test_command)

if __name__ == '__main__':
    app()

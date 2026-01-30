#!/usr/bin/env python
import sys
from enum import StrEnum
from pathlib import Path
from typing import Annotated

import typer
from imandrax_codegen.art_parse import codegen
from imandrax_codegen.test_gen import gen_test_cases_source

app = typer.Typer()


class Lang(StrEnum):
    python = 'python'
    typescript = 'typescript'
    py = 'py'
    ts = 'ts'


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
    lang: Annotated[
        Lang,
        typer.Option('-l', '--lang', help='Target language'),
    ] = Lang.python,
) -> None:
    """Generate source code from ImandraX artifact."""
    if ctx.invoked_subcommand is not None:
        return

    content = read_input(input_path)
    target_lang = 'typescript' if lang in (Lang.typescript, Lang.ts) else 'python'
    result = codegen(content, mode.value, target_lang)
    write_output(output, result)


def gen_test(
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
    output: Annotated[
        str | None,
        typer.Option('-o', '--output', help='Output file path (defaults to stdout)'),
    ] = None,
    lang: Annotated[
        Lang,
        typer.Option('-l', '--lang', help='Target language'),
    ] = Lang.python,
) -> None:
    """Generate test cases for IML."""
    iml = read_input(iml_path)
    target_lang = 'typescript' if lang in (Lang.typescript, Lang.ts) else 'python'
    result = gen_test_cases_source(iml, function, lang=target_lang)
    write_output(output, result)


app.command()(gen_test)

if __name__ == '__main__':
    app()

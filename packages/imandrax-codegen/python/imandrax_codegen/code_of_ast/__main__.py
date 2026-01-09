#!/usr/bin/env python
"""CLI tool to convert OCaml AST JSON to Python source code."""

import sys
from pathlib import Path
from typing import Annotated

import typer
from imandrax_codegen.ast_deserialize import stmts_of_json
from imandrax_codegen.unparse import unparse

app = typer.Typer()


@app.command(name='code-of-ocaml-ast')
def code_of_ocaml_ast(
    input_file: Annotated[
        str,
        typer.Argument(help="Input JSON file (from OCaml yojson), or '-' for stdin"),
    ],
    output: Annotated[
        str | None,
        typer.Option(
            '-o',
            '--output',
            help='Output Python file (writes to stdout if not provided)',
        ),
    ] = None,
    include_real_to_float_alias: Annotated[
        bool,
        typer.Option(
            '--include-real-to-float-alias', help='Include real to float alias'
        ),
    ] = False,
) -> None:
    """Convert OCaml AST JSON to Python source code."""
    # Read and deserialize
    if input_file == '-':
        json_str = sys.stdin.read()
    else:
        with Path(input_file).open() as f:
            json_str = f.read()

    if not json_str:
        typer.echo('imandrax_codegen error: Input is empty', err=True)
        raise typer.Exit(code=1)

    stmts = stmts_of_json(json_str)

    # Generate Python code
    python_code = unparse(
        stmts,
        alias_real_to_float=include_real_to_float_alias,
    )

    # Write output
    if output:
        with Path(output).open('w') as f:
            f.write(python_code)
            f.write('\n')
    else:
        typer.echo(python_code)


if __name__ == '__main__':
    app()

#!/usr/bin/env python
import sys
from pathlib import Path

import typer
from imandrax_codegen.test_gen import gen_test_cases
from imandrax_codegen.unparse import unparse

app = typer.Typer()


def gen_test_command(
    iml_path: str = typer.Argument(
        help='Path of IML file to generate test cases (use "-" to read from stdin)',
    ),
    function: str = typer.Option(
        ...,
        '-f',
        '--function',
        help='Name of function to generate test cases for',
    ),
    output: str | None = typer.Option(
        None,
        '-o',
        '--output',
        help='Output file path (defaults to stdout)',
    ),
) -> None:
    """Generate test cases for IML."""
    # Read input from stdin or file
    if iml_path == '-':
        iml = sys.stdin.read()
    else:
        iml = Path(iml_path).read_text()

    test_case_stmts = gen_test_cases(iml, function)
    result = unparse(test_case_stmts)

    # Write output to file or stdout
    if output:
        Path(output).write_text(result)
    else:
        typer.echo(result)


app.command()(gen_test_command)

if __name__ == '__main__':
    app()

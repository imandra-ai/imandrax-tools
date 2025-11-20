from inline_snapshot import snapshot

from imandrax_api_models.context_utils import format_code_snippet_with_error


def test_single_line_error():
    code = """\
def calculate(a, b):
    total = a + b
    result = x + total
    return result"""

    res = format_code_snippet_with_error(
        code, (3, 13), (3, 14), "undefined variable 'x'"
    )
    assert res == snapshot(
        """\
Error: undefined variable 'x'

  1 | def calculate(a, b):
  2 |     total = a + b
* 3 |     result = x + total
    |              ^
  4 |     return result\
"""
    )


def test_multi_line_error():
    code = """\
def calculate(a, b):
    if a > 0:
        result = a + b
        return result
    else:
        return 0"""
    res = format_code_snippet_with_error(
        code,
        (2, 4),
        (4, 17),
        'unreachable code detected',
    )
    assert res == snapshot(
        """\
Error: unreachable code detected

  1 | def calculate(a, b):
* 2 |     if a > 0:
    |     ^~~~~~~~~~
* 3 |         result = a + b
    | ~~~~~~~~~~~~~~~~~~~~~~
* 4 |         return result
    | ~~~~~~~~~~~~~~~~~
  5 |     else:
  6 |         return 0\
"""
    )


def test_error_at_first_line():
    """Test error at the very first line of code."""
    code = """\
import unknown_module
def foo():
    pass"""
    res = format_code_snippet_with_error(code, (1, 7), (1, 21), 'module not found')
    assert res == snapshot("""\
Error: module not found

* 1 | import unknown_module
    |        ^^^^^^^^^^^^^^
  2 | def foo():
  3 |     pass\
""")


def test_error_at_last_line():
    """Test error at the very last line of code."""
    code = """\
def foo():
    x = 1
    return undefined"""
    res = format_code_snippet_with_error(code, (3, 11), (3, 20), 'undefined variable')
    assert res == snapshot("""\
Error: undefined variable

  1 | def foo():
  2 |     x = 1
* 3 |     return undefined
    |            ^^^^^^^^^\
""")


def test_error_at_column_zero():
    """Test error starting at column 0."""
    code = """\
def calculate():
    return 42
invalid syntax here"""
    res = format_code_snippet_with_error(code, (3, 0), (3, 7), 'invalid syntax')
    assert res == snapshot("""\
Error: invalid syntax

  1 | def calculate():
  2 |     return 42
* 3 | invalid syntax here
    | ^^^^^^^\
""")


def test_error_with_zero_context():
    """Test with context_line=0 (no surrounding lines)."""
    code = """\
line1
line2
line3 with error
line4
line5"""
    res = format_code_snippet_with_error(
        code, (3, 6), (3, 10), 'syntax error', context_line=0
    )
    assert res == snapshot("""\
Error: syntax error

* 3 | line3 with error
    |       ^^^^\
""")


def test_error_with_large_context():
    """Test with large context_line value exceeding file bounds."""
    code = """\
def foo():
    return x"""
    res = format_code_snippet_with_error(
        code, (2, 11), (2, 12), 'undefined variable', context_line=10
    )
    assert res == snapshot("""\
Error: undefined variable

  1 | def foo():
* 2 |     return x
    |            ^\
""")


def test_multi_line_error_with_empty_line():
    """Test multi-line error that spans across an empty line."""
    code = """\
def broken():
    start

    end"""
    res = format_code_snippet_with_error(code, (2, 4), (4, 7), 'incomplete block')
    assert res == snapshot("""\
Error: incomplete block

  1 | def broken():
* 2 |     start
    |     ^~~~~~
* 3 | \n\
    | \n\
* 4 |     end
    | ~~~~~~~\
""")

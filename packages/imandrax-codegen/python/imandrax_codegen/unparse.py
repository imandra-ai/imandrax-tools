"""Convert custom AST to Python stdlib AST and unparse to source code."""

import ast as stdlib_ast
import subprocess
from typing import Any, Final, cast

from ruff.__main__ import find_ruff_bin

from . import ast_types as custom_ast

ruff_bin = find_ruff_bin()


OPTION_LIB_SRC: Final[str] = """\
T = TypeVar('T')


@dataclass
class Some(Generic[T]):
    value: T

option: TypeAlias = Some[T] | None
"""


def format_code(code: str) -> str:
    """Format Python code using ruff."""

    # Sort imports
    code = fix_ruff_check(code, ['I001'])

    try:
        result = subprocess.run(
            [ruff_bin, 'format', '-'],
            input=code,
            capture_output=True,
            text=True,
            check=True,
        )
        return result.stdout
    except (subprocess.CalledProcessError, FileNotFoundError):
        # If ruff fails or is not found, return original code
        return code


def fix_ruff_check(code: str, rules: list[str]) -> str:
    """Lint and fix code with ruff for the given rules."""

    if not rules:
        return code

    ruff_bin = find_ruff_bin()

    try:
        result = subprocess.run(
            [ruff_bin, 'check', '--select', ','.join(rules), '--fix', '-'],
            check=False,
            input=code,
            capture_output=True,
            text=True,
        )
        # ruff check outputs fixed code to stdout
        return result.stdout if result.stdout else code
    except (subprocess.CalledProcessError, FileNotFoundError):
        # If ruff fails or is not found, return original code
        return code


def remove_unused_import(code: str) -> str:
    """Remove unused imports from code."""
    return fix_ruff_check(code, ['F401'])


def to_stdlib(node: Any) -> Any:
    """Recursively convert custom AST node to stdlib AST node."""
    if node is None:
        return None

    # Handle primitive types
    if isinstance(node, (str, int, float, bool, bytes)):
        return node

    # Handle lists
    if isinstance(node, list):
        node_list = cast(list[Any], node)
        result: list[Any] = []
        for item in node_list:
            result.append(to_stdlib(item))
        return result

    # Handle custom AST nodes - get the corresponding stdlib class by name
    class_name = node.__class__.__name__
    stdlib_class = getattr(stdlib_ast, class_name, None)

    if stdlib_class is None:
        raise ValueError(f'No stdlib AST class found for: {class_name}')

    # Get all fields from the custom node and recursively convert them
    kwargs: dict[str, Any] = {}
    for field_name, field_value in node.__dict__.items():
        kwargs[field_name] = to_stdlib(field_value)

    return stdlib_class(**kwargs)


def unparse(
    nodes: list[custom_ast.stmt],
    alias_real_to_float: bool = False,
    # TODO: add a config field?
    # - [x] whether to alias `real` to `float` or not
    #   - [ ] alternatively: use Decimal instead of float
    # - the python version to use: 3.12+ or not
    #   - this determines the type definition syntax
    # # 3.12+
    # class Pair[A, B]:
    #     first: A
    #     second: B
    # # ----
    # # 3.11-
    # A = TypeVar('A')
    # B = TypeVar('B')
    # class Pair(Generic[A, B]):
    #     first: A
    #     second: B
    # TODO: use ruff upgrade and --target-version parameter
) -> str:
    """Convert custom AST to Python source code using stdlib ast.unparse."""
    stdlib_stmts: list[stdlib_ast.stmt] = to_stdlib(nodes)

    def mk_ast(src: str) -> list[stdlib_ast.stmt]:
        return stdlib_ast.parse(src).body

    def gen_code(stmts: list[stdlib_ast.stmt]) -> str:
        """Generate Python source code from a list of AST statements."""
        module = stdlib_ast.Module(body=stmts, type_ignores=[])
        stdlib_ast.fix_missing_locations(module)
        return stdlib_ast.unparse(module)

    future_annotations_import = stdlib_ast.ImportFrom(
        module='__future__',
        names=[stdlib_ast.alias(name='annotations', asname=None)],
        level=0,
    )
    dataclass_import = stdlib_ast.ImportFrom(
        module='dataclasses',
        names=[stdlib_ast.alias(name='dataclass', asname=None)],
        level=0,
    )
    typing_import = stdlib_ast.ImportFrom(
        module='typing',
        names=[
            stdlib_ast.alias(name='TypeVar', asname=None),
            stdlib_ast.alias(name='Generic', asname=None),
            stdlib_ast.alias(name='TypeAlias', asname=None),
        ],
        level=0,
    )
    option_lib_import = mk_ast('from imandrax_option_lib import option, Some')
    option_lib_definition: list[stdlib_ast.stmt] = mk_ast(OPTION_LIB_SRC)
    alias_real: list[stdlib_ast.stmt] = mk_ast('real = float')

    body = [
        future_annotations_import,
        dataclass_import,
        typing_import,
        *option_lib_import,
        *(alias_real if alias_real_to_float else []),
        *stdlib_stmts,
    ]

    code = gen_code(body)

    code = remove_unused_import(code)

    # After removing unused imports, if "from imandrax_option_lib" is still present,
    # it means that the option_lib definition is needed
    if 'from imandrax_option_lib' in code:
        body = [
            future_annotations_import,
            dataclass_import,
            typing_import,
            *option_lib_definition,
            *(alias_real if alias_real_to_float else []),
            *stdlib_stmts,
        ]
        code = gen_code(body)

    return format_code(code)

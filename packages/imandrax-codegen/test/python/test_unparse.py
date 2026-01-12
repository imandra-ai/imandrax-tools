from dataclasses import dataclass
from pathlib import Path
from typing import Any

import yaml
from imandrax_codegen.test_gen import gen_test_cases
from imandrax_codegen.unparse import unparse
from inline_snapshot import snapshot

curr_dir = Path(__file__).parent
DATA_DIR = curr_dir.parent / 'data' / 'fun_decomp'


@dataclass
class TestGenInput:
    __test__ = False  # This is not a test
    name: str
    iml: str
    function_name: str
    other_decomp_kwargs: dict[str, Any]


def read_test_input(file_path: Path):
    with file_path.open('r') as f:
        o = yaml.safe_load(f)

    return TestGenInput(
        name=o['name'],
        iml=o['iml'],
        function_name=o['decomp_kwargs']['name'],
        other_decomp_kwargs={
            k: v for k, v in o['decomp_kwargs'].items() if k != 'name'
        },
    )


def test_composite_record():
    file_path = DATA_DIR / 'composite_record.yaml'
    input_data = read_test_input(file_path)
    stmts = gen_test_cases(
        iml=input_data.iml,
        decomp_name=input_data.function_name,
        other_decomp_kwargs=input_data.other_decomp_kwargs,
    )
    code = unparse(stmts)
    assert code == snapshot('''\
from __future__ import annotations

from dataclasses import dataclass


@dataclass
class point:
    x: int
    y: int


def test_1():
    """test_1

    - invariant: "positive"
    - constraints:
        - not (p.x + p.y = 0)
        - p.x + p.y >= 1
    """
    result: str = distance_category(p=point(0, 1))
    expected: str = 'positive'
    assert result == expected


def test_2():
    """test_2

    - invariant: "origin"
    - constraints:
        - p.x + p.y = 0
    """
    result: str = distance_category(p=point(-38, 38))
    expected: str = 'origin'
    assert result == expected


def test_3():
    """test_3

    - invariant: "negative"
    - constraints:
        - p.x + p.y <= (-1)
    """
    result: str = distance_category(p=point(0, -1))
    expected: str = 'negative'
    assert result == expected
''')


def test_nested_conditions():
    file_path = DATA_DIR / 'nested_conditions.yaml'
    input_data = read_test_input(file_path)
    stmts = gen_test_cases(
        iml=input_data.iml,
        decomp_name=input_data.function_name,
        other_decomp_kwargs=input_data.other_decomp_kwargs,
    )
    code = unparse(stmts)
    assert code == snapshot('''\
from __future__ import annotations


def test_1():
    """test_1

    - invariant: ~- x + y
    - constraints:
        - x <= 0
        - y <= 0
    """
    result: int = nested_check(x=0, y=0)
    expected: int = 0
    assert result == expected


def test_2():
    """test_2

    - invariant: y - x
    - constraints:
        - y >= 1
        - x <= 0
    """
    result: int = nested_check(y=1, x=0)
    expected: int = 1
    assert result == expected


def test_3():
    """test_3

    - invariant: x - y
    - constraints:
        - x >= 1
        - y <= 0
    """
    result: int = nested_check(x=1, y=0)
    expected: int = 1
    assert result == expected


def test_4():
    """test_4

    - invariant: x + y
    - constraints:
        - x >= 1
        - y >= 1
    """
    result: int = nested_check(x=1, y=1)
    expected: int = 2
    assert result == expected
''')


def test_list_operations():
    file_path = DATA_DIR / 'list_operations.yaml'
    input_data = read_test_input(file_path)
    stmts = gen_test_cases(
        iml=input_data.iml,
        decomp_name=input_data.function_name,
        other_decomp_kwargs=input_data.other_decomp_kwargs,
    )
    code = unparse(stmts)
    assert code == snapshot('''\
from __future__ import annotations


def test_1():
    """test_1

    - invariant: 0
    - constraints:
        - not (xs <> [])
    """
    result: int = list_check(xs=[])
    expected: int = 0
    assert result == expected


def test_2():
    """test_2

    - invariant: List.hd xs
    - constraints:
        - xs <> []
        - not ((List.tl xs) <> [])
    """
    result: int = list_check(xs=[0])
    expected: int = 0
    assert result == expected


def test_3():
    """test_3

    - invariant: List.hd xs + List.hd (List.tl xs)
    - constraints:
        - (List.tl xs) <> []
        - xs <> []
    """
    result: int = list_check(xs=[1, 0])
    expected: int = 1
    assert result == expected
''')


def test_complex_variant_record():
    file_path = DATA_DIR / 'complex_variant_record.yaml'
    input_data = read_test_input(file_path)
    stmts = gen_test_cases(
        iml=input_data.iml,
        decomp_name=input_data.function_name,
        other_decomp_kwargs=input_data.other_decomp_kwargs,
    )
    code = unparse(stmts)
    assert code == snapshot('''\
from __future__ import annotations

from dataclasses import dataclass


@dataclass
class user:
    id: int
    active: status


@dataclass
class Active:
    pass


@dataclass
class Inactive:
    pass


status = Active | Inactive


def test_1():
    """test_1

    - invariant: (-1)
    - constraints:
        - not (u.active = Active)
    """
    result: int = process_user(u=user(0, Inactive()))
    expected: int = -1
    assert result == expected


def test_2():
    """test_2

    - invariant: 0
    - constraints:
        - u.active = Active
        - u.id <= 0
    """
    result: int = process_user(u=user(0, Active()))
    expected: int = 0
    assert result == expected


def test_3():
    """test_3

    - invariant: u.id
    - constraints:
        - u.active = Active
        - u.id >= 1
    """
    result: int = process_user(u=user(1, Active()))
    expected: int = 1
    assert result == expected
''')


def test_composite_tuple():
    file_path = DATA_DIR / 'composite_tuple.yaml'
    input_data = read_test_input(file_path)
    stmts = gen_test_cases(
        iml=input_data.iml,
        decomp_name=input_data.function_name,
        other_decomp_kwargs=input_data.other_decomp_kwargs,
    )
    code = unparse(stmts)
    assert code == snapshot('''\
from __future__ import annotations


def test_1():
    """test_1

    - invariant: _x_1_25.1 - _x_1_25.0
    - constraints:
        - _x_1_25.0 <= _x_1_25.1
        - not (_x_1_25.0 = _x_1_25.1)
    """
    result: int = tuple_compare(_x_1_25=(0, 1))
    expected: int = 1
    assert result == expected


def test_2():
    """test_2

    - invariant: 0
    - constraints:
        - _x_1_25.0 = _x_1_25.1
        - _x_1_25.0 <= _x_1_25.1
    """
    result: int = tuple_compare(_x_1_25=(0, 0))
    expected: int = 0
    assert result == expected


def test_3():
    """test_3

    - invariant: _x_1_25.0 - _x_1_25.1
    - constraints:
        - not (_x_1_25.0 <= _x_1_25.1)
    """
    result: int = tuple_compare(_x_1_25=(0, -1))
    expected: int = 1
    assert result == expected
''')


def test_with_basis():
    file_path = DATA_DIR / 'with_basis.yaml'
    input_data = read_test_input(file_path)
    stmts = gen_test_cases(
        iml=input_data.iml,
        decomp_name=input_data.function_name,
        other_decomp_kwargs=input_data.other_decomp_kwargs,
    )
    code = unparse(stmts)
    assert code == snapshot('''\
from __future__ import annotations


def test_1():
    """test_1

    - invariant: helper (~- x)
    - constraints:
        - x <= 0
    """
    result: int = compute(x=0)
    expected: int = 0
    assert result == expected


def test_2():
    """test_2

    - invariant: helper x
    - constraints:
        - x >= 1
    """
    result: int = compute(x=1)
    expected: int = 1
    assert result == expected
''')


def test_primitive_real():
    file_path = DATA_DIR / 'primitive_real.yaml'
    input_data = read_test_input(file_path)
    stmts = gen_test_cases(
        iml=input_data.iml,
        decomp_name=input_data.function_name,
        other_decomp_kwargs=input_data.other_decomp_kwargs,
    )
    code = unparse(stmts)
    assert code == snapshot('''\
from __future__ import annotations


def test_1():
    """test_1

    - invariant: true
    - constraints:
        - true
    """
    result: bool = classify_temp(temp=0)
    expected: bool = True
    assert result == expected
''')


def test_multiple_parameters():
    file_path = DATA_DIR / 'multiple_parameters.yaml'
    input_data = read_test_input(file_path)
    stmts = gen_test_cases(
        iml=input_data.iml,
        decomp_name=input_data.function_name,
        other_decomp_kwargs=input_data.other_decomp_kwargs,
    )
    code = unparse(stmts)
    assert code == snapshot('''\
from __future__ import annotations


def test_1():
    """test_1

    - invariant: 0
    - constraints:
        - a <= b
        - not (a = b)
        - not (b = c)
    """
    result: int = calculate(b=1, c=2, a=0)
    expected: int = 0
    assert result == expected


def test_2():
    """test_2

    - invariant: b * a
    - constraints:
        - b = c
        - a <= b
        - not (a = b)
    """
    result: int = calculate(a=0, c=1, b=1)
    expected: int = 0
    assert result == expected


def test_3():
    """test_3

    - invariant: a * c
    - constraints:
        - a = b
        - a <= b
    """
    result: int = calculate(b=0, a=0, c=0)
    expected: int = 0
    assert result == expected


def test_4():
    """test_4

    - invariant: 0
    - constraints:
        - not (a <= b)
        - b <= c
        - not (a = b)
        - not (b = c)
    """
    result: int = calculate(b=0, a=1, c=1)
    expected: int = 0
    assert result == expected


def test_5():
    """test_5

    - invariant: b * a
    - constraints:
        - not (a <= b)
        - b = c
        - b <= c
        - not (a = b)
    """
    result: int = calculate(a=0, c=-1, b=-1)
    expected: int = 0
    assert result == expected


def test_6():
    """test_6

    - invariant: a + b + c
    - constraints:
        - not (a <= b)
        - not (b <= c)
    """
    result: int = calculate(a=1, c=-1, b=0)
    expected: int = 0
    assert result == expected
''')


def test_variant_simple():
    file_path = DATA_DIR / 'variant_simple.yaml'
    input_data = read_test_input(file_path)
    stmts = gen_test_cases(
        iml=input_data.iml,
        decomp_name=input_data.function_name,
        other_decomp_kwargs=input_data.other_decomp_kwargs,
    )
    code = unparse(stmts)
    assert code == snapshot('''\
from __future__ import annotations

from dataclasses import dataclass


@dataclass
class Red:
    pass


@dataclass
class Green:
    pass


@dataclass
class Blue:
    pass


color = Red | Green | Blue


def test_1():
    """test_1

    - invariant: 1
    - constraints:
        - not (c = Blue)
        - not (c = Green)
    """
    result: int = color_value(c=Red())
    expected: int = 1
    assert result == expected


def test_2():
    """test_2

    - invariant: 2
    - constraints:
        - c = Green
        - not (c = Blue)
    """
    result: int = color_value(c=Green())
    expected: int = 2
    assert result == expected


def test_3():
    """test_3

    - invariant: 3
    - constraints:
        - c = Blue
    """
    result: int = color_value(c=Blue())
    expected: int = 3
    assert result == expected
''')


def test_option_type():
    file_path = DATA_DIR / 'option_type.yaml'
    input_data = read_test_input(file_path)
    stmts = gen_test_cases(
        iml=input_data.iml,
        decomp_name=input_data.function_name,
        other_decomp_kwargs=input_data.other_decomp_kwargs,
    )
    code = unparse(stmts)
    assert code == snapshot('''\
from __future__ import annotations

from dataclasses import dataclass
from typing import Generic, TypeAlias, TypeVar

T = TypeVar('T')


@dataclass
class Some(Generic[T]):
    value: T


option: TypeAlias = Some[T] | None


def test_1():
    """test_1

    - invariant: ~- Option.get opt
    - constraints:
        - not Is_a(None, opt)
        - Option.get opt <= 0
    """
    result: int = option_value(opt=Some(0))
    expected: int = 0
    assert result == expected


def test_2():
    """test_2

    - invariant: Option.get opt
    - constraints:
        - not Is_a(None, opt)
        - Option.get opt >= 1
    """
    result: int = option_value(opt=Some(1))
    expected: int = 1
    assert result == expected


def test_3():
    """test_3

    - invariant: 0
    - constraints:
        - Is_a(None, opt)
    """
    result: int = option_value(opt=None)
    expected: int = 0
    assert result == expected
''')


def test_basic():
    file_path = DATA_DIR / 'basic.yaml'
    input_data = read_test_input(file_path)
    stmts = gen_test_cases(
        iml=input_data.iml,
        decomp_name=input_data.function_name,
        other_decomp_kwargs=input_data.other_decomp_kwargs,
    )
    code = unparse(stmts)
    assert code == snapshot('''\
from __future__ import annotations


def test_1():
    """test_1

    - invariant: x + 2
    - constraints:
        - x >= 1
    """
    result: int = f(x=1)
    expected: int = 3
    assert result == expected


def test_2():
    """test_2

    - invariant: 1 + x
    - constraints:
        - x <= 0
    """
    result: int = f(x=0)
    expected: int = 1
    assert result == expected
''')


def test_primitive_bool():
    file_path = DATA_DIR / 'primitive_bool.yaml'
    input_data = read_test_input(file_path)
    stmts = gen_test_cases(
        iml=input_data.iml,
        decomp_name=input_data.function_name,
        other_decomp_kwargs=input_data.other_decomp_kwargs,
    )
    code = unparse(stmts)
    assert code == snapshot('''\
from __future__ import annotations


def test_1():
    """test_1

    - invariant: 0
    - constraints:
        - not a
        - not b
    """
    result: int = bool_logic(a=False, b=False)
    expected: int = 0
    assert result == expected


def test_2():
    """test_2

    - invariant: 2
    - constraints:
        - b
        - not a
    """
    result: int = bool_logic(a=False, b=True)
    expected: int = 2
    assert result == expected


def test_3():
    """test_3

    - invariant: 2
    - constraints:
        - a
        - not b
    """
    result: int = bool_logic(a=True, b=False)
    expected: int = 2
    assert result == expected


def test_4():
    """test_4

    - invariant: 1
    - constraints:
        - a
        - b
    """
    result: int = bool_logic(a=True, b=True)
    expected: int = 1
    assert result == expected
''')


def test_with_guards():
    file_path = DATA_DIR / 'with_guards.yaml'
    input_data = read_test_input(file_path)
    stmts = gen_test_cases(
        iml=input_data.iml,
        decomp_name=input_data.function_name,
        other_decomp_kwargs=input_data.other_decomp_kwargs,
    )
    code = unparse(stmts)
    assert code == snapshot('''\
from __future__ import annotations


def test_1():
    """test_1

    - invariant: 0
    - constraints:
        - x <= 0
    """
    result: int = classify(x=0, y=0)
    expected: int = 0
    assert result == expected


def test_2():
    """test_2

    - invariant: 0
    - constraints:
        - x >= 1
        - y <= 0
    """
    result: int = classify(x=1, y=0)
    expected: int = 0
    assert result == expected


def test_3():
    """test_3

    - invariant: 3
    - constraints:
        - x <= y
        - y <= x
        - x >= 1
        - y >= 1
    """
    result: int = classify(y=1, x=1)
    expected: int = 3
    assert result == expected


def test_4():
    """test_4

    - invariant: 2
    - constraints:
        - not (y <= x)
        - x <= y
        - x >= 1
        - y >= 1
    """
    result: int = classify(y=2, x=1)
    expected: int = 2
    assert result == expected


def test_5():
    """test_5

    - invariant: 1
    - constraints:
        - not (x <= y)
        - x >= 1
        - y >= 1
    """
    result: int = classify(y=1, x=2)
    expected: int = 1
    assert result == expected
''')


def test_variant_with_data():
    file_path = DATA_DIR / 'variant_with_data.yaml'
    input_data = read_test_input(file_path)
    stmts = gen_test_cases(
        iml=input_data.iml,
        decomp_name=input_data.function_name,
        other_decomp_kwargs=input_data.other_decomp_kwargs,
    )
    code = unparse(stmts)
    assert code == snapshot('''\
from __future__ import annotations

from dataclasses import dataclass


@dataclass
class Circle:
    arg0: int


@dataclass
class Rectangle:
    arg0: int
    arg1: int


shape = Circle | Rectangle


def test_1():
    """test_1

    - invariant: Destruct(Rectangle, 0, s) * Destruct(Rectangle, 1, s)
    - constraints:
        - not Is_a(Circle, s)
    """
    result: int = area(s=Rectangle(0, 1))
    expected: int = 0
    assert result == expected


def test_2():
    """test_2

    - invariant: Destruct(Circle, 0, s) * Destruct(Circle, 0, s)
    - constraints:
        - Is_a(Circle, s)
    """
    result: int = area(s=Circle(0))
    expected: int = 0
    assert result == expected
''')


def test_primitive_int():
    file_path = DATA_DIR / 'primitive_int.yaml'
    input_data = read_test_input(file_path)
    stmts = gen_test_cases(
        iml=input_data.iml,
        decomp_name=input_data.function_name,
        other_decomp_kwargs=input_data.other_decomp_kwargs,
    )
    code = unparse(stmts)
    assert code == snapshot('''\
from __future__ import annotations


def test_1():
    """test_1

    - invariant: 1
    - constraints:
        - not (x = 0)
        - x >= 1
    """
    result: int = classify_number(x=1)
    expected: int = 1
    assert result == expected


def test_2():
    """test_2

    - invariant: 0
    - constraints:
        - x = 0
    """
    result: int = classify_number(x=0)
    expected: int = 0
    assert result == expected


def test_3():
    """test_3

    - invariant: (-1)
    - constraints:
        - x <= (-1)
    """
    result: int = classify_number(x=-1)
    expected: int = -1
    assert result == expected
''')


if __name__ == '__main__':
    TEMPLATE: str = """\
def test_{file_name}():
    file_path = DATA_DIR / '{file_name}.yaml'
    input_data = read_test_input(file_path)
    stmts = gen_test_cases(
        iml=input_data.iml,
        decomp_name=input_data.function_name,
        other_decomp_kwargs=input_data.other_decomp_kwargs,
    )
    code = unparse(stmts)
    assert code == snapshot()
"""

    def codegen():
        files: list[str] = []
        for file in DATA_DIR.iterdir():
            if file.suffix == '.yaml':
                files.append(file.stem)

        for file_name in files:
            print(TEMPLATE.format(file_name=file_name))

    codegen()

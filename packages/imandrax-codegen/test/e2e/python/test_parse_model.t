Setup: Define helper function
  $ fence() { printf '```python\n'; cat; printf '```'; }
  $ run_test() { (
  >    cd $DUNE_SOURCEROOT/packages/imandrax-codegen && \
  >    py-gen-parse "test/data/art/model/$1" - --mode model \
  >    | uv run python/imandrax_codegen/code_of_ast - \
  >    | fence
  > ); }

inline_record
  $ run_test composite/inline_record.yaml
  ```python
  from __future__ import annotations
  
  w: event = Scroll(2.0)
  
  ```

map_default_value_only
  $ run_test composite/map_default_value_only.yaml
  ```python
  from __future__ import annotations
  
  from typing import TypeVar
  
  a = TypeVar('a')
  w: defaultdict[a, bool] = defaultdict(lambda: False)
  
  ```

map_int_bool_0
  $ run_test composite/map_int_bool_0.yaml
  ```python
  from __future__ import annotations
  
  w: defaultdict[int, bool] = defaultdict(lambda: False)
  
  ```

map_int_bool_1
  $ run_test composite/map_int_bool_1.yaml
  ```python
  from __future__ import annotations
  
  w: defaultdict[int, bool] = defaultdict(lambda: False, {2: True})
  
  ```

map_int_bool_2
  $ run_test composite/map_int_bool_2.yaml
  ```python
  from __future__ import annotations
  
  w: defaultdict[int, bool] = defaultdict(lambda: False, {2: True, 3: False})
  
  ```

multiset_empty
  $ run_test composite/multiset_empty.yaml
  ```python
  from __future__ import annotations
  
  from typing import TypeVar
  
  a = TypeVar('a')
  w: defaultdict[a, int] = defaultdict(lambda: 0)
  
  ```

multiset_nonempty
  $ run_test composite/multiset_nonempty.yaml
  ```python
  from __future__ import annotations
  
  w: defaultdict[int, int] = defaultdict(lambda: 0, {1: 2, 3: 1, 2: 2})
  
  ```

set_empty
  $ run_test composite/set_empty.yaml
  ```python
  from __future__ import annotations
  
  from typing import TypeVar
  
  a = TypeVar('a')
  w: defaultdict[a, bool] = defaultdict(lambda: False)
  
  ```

set_nonempty
  $ run_test composite/set_nonempty.yaml
  ```python
  from __future__ import annotations
  
  w: defaultdict[int, bool] = defaultdict(lambda: False, {1: True, 3: True, 2: True})
  
  ```

variant_and_record
  $ run_test composite/variant_and_record.yaml
  ```python
  from __future__ import annotations
  
  w: movement = Move(position(x=1, y=2, z=3.0), North())
  
  ```

bool list
  $ run_test primitive/bool_list.yaml
  ```python
  from __future__ import annotations
  
  w: list[bool] = [True, False]
  
  ```

empty list
  $ run_test primitive/empty_list.yaml
  ```python
  from __future__ import annotations
  
  from typing import TypeVar
  
  a = TypeVar('a')
  w: list[a] = []
  
  ```

int option none
  $ run_test primitive/int_option_none.yaml
  ```python
  from __future__ import annotations
  
  from dataclasses import dataclass
  from typing import Generic, TypeAlias, TypeVar
  
  T = TypeVar('T')
  
  
  @dataclass
  class Some(Generic[T]):
      value: T
  
  
  option: TypeAlias = Some[T] | None
  a = TypeVar('a')
  w: option[a] = None
  
  ```

int option
  $ run_test primitive/int_option.yaml
  ```python
  from __future__ import annotations
  
  from dataclasses import dataclass
  from typing import Generic, TypeAlias, TypeVar
  
  T = TypeVar('T')
  
  
  @dataclass
  class Some(Generic[T]):
      value: T
  
  
  option: TypeAlias = Some[T] | None
  w: option[int] = Some(2)
  
  ```

int
  $ run_test primitive/int.yaml
  ```python
  from __future__ import annotations
  
  w: int = 2
  
  ```

LChar
  $ run_test primitive/LChar.yaml
  ```python
  from __future__ import annotations
  
  w: str = '\x00'
  
  ```

LString
  $ run_test primitive/LString.yaml
  ```python
  from __future__ import annotations
  
  w: list[str] = ['h', 'i']
  
  ```

real
  $ run_test primitive/real.yaml
  ```python
  from __future__ import annotations
  
  w: real = 3.14
  
  ```

record
  $ run_test primitive/record.yaml
  ```python
  from __future__ import annotations
  
  w: user = user(id=1, active=True)
  
  ```

single element int list
  $ run_test primitive/single_element_int_list.yaml
  ```python
  from __future__ import annotations
  
  w: list[int] = [1]
  
  ```

tuple of bool and int
  $ run_test primitive/tuple_of_bool_and_int.yaml
  ```python
  from __future__ import annotations
  
  w: tuple[bool, int] = (True, 2)
  
  ```

variant1
  $ run_test primitive/variant1.yaml
  ```python
  from __future__ import annotations
  
  w: status = Active()
  
  ```

variant2
  $ run_test primitive/variant2.yaml
  ```python
  from __future__ import annotations
  
  w: status = Waitlist(1)
  
  ```

variant3
  $ run_test primitive/variant3.yaml
  ```python
  from __future__ import annotations
  
  w: status = Waitlist(2, True)
  
  ```


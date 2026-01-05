Setup: Define helper function
  $ fence() { printf '```python\n'; cat; printf '```'; }
  $ run_test() { (
  >    cd $DUNE_SOURCEROOT/packages/imandrax-codegen && \
  >    yq ".get_decls_res.decls[0].artifact" "test/data/decl/$1" -o json \
  >    | py-gen-parse - - --mode decl \
  >    | uv run py-gen - \
  >    | fence
  > ); }

function
  $ run_test function.yaml
  ```python
  Error parsing input: WIP: Fun
  Error: Input is empty
  ```

GADT_monomorphic
  $ run_test GADT_monomorphic.yaml
  ```python
  Unexpected error: Yojson__Safe.Util.Type_error("Can't get member 'kind' of non-object type null", 870828711)
  Error: Input is empty
  ```

nested_generics
  $ run_test nested_generics.yaml
  ```python
  from __future__ import annotations
  from dataclasses import dataclass
  from typing import TypeVar, Generic, TypeAlias
  
  T = TypeVar('T')
  
  
  @dataclass
  class Some(Generic[T]):
      value: T
  
  
  option: TypeAlias = Some[T] | None
  
  
  @dataclass
  class My_ty:
      arg0: tagged[validated][maybe][identity][int]
  
  
  my_ty = My_ty
  
  ```

real_and_option
  $ run_test real_and_option.yaml
  ```python
  from __future__ import annotations
  from dataclasses import dataclass
  from typing import TypeVar, Generic, TypeAlias
  
  T = TypeVar('T')
  
  
  @dataclass
  class Some(Generic[T]):
      value: T
  
  
  option: TypeAlias = Some[T] | None
  
  
  @dataclass
  class my_ty:
      x: real
      y: option[int]
      z: int
  
  ```

record
  $ run_test record.yaml
  ```python
  from __future__ import annotations
  from dataclasses import dataclass
  from typing import TypeVar, Generic, TypeAlias
  
  T = TypeVar('T')
  
  
  @dataclass
  class Some(Generic[T]):
      value: T
  
  
  option: TypeAlias = Some[T] | None
  
  
  @dataclass
  class point:
      x: int
      y: int
  
  ```

tuple_two_int
  $ run_test tuple_two_int.yaml
  ```python
  Unexpected error: Yojson__Safe.Util.Type_error("Can't get member 'kind' of non-object type null", 870828711)
  Error: Input is empty
  ```

variant_poly_two_var
  $ run_test variant_poly_two_var.yaml
  ```python
  from __future__ import annotations
  from dataclasses import dataclass
  from typing import TypeVar, Generic, TypeAlias
  
  T = TypeVar('T')
  
  
  @dataclass
  class Some(Generic[T]):
      value: T
  
  
  option: TypeAlias = Some[T] | None
  a = TypeVar('a')
  b = TypeVar('b')
  
  
  @dataclass
  class Empty:
      pass
  
  
  @dataclass
  class Single(Generic[a]):
      arg0: a
  
  
  @dataclass
  class Pair(Generic[a, b]):
      arg0: a
      arg1: b
  
  
  @dataclass
  class Labeled(Generic[a, b]):
      key: a
      value: b
  
  
  @dataclass
  class Multi(Generic[a, b]):
      arg0: list[a]
      arg1: list[b]
  
  
  container = Empty | Single[a] | Pair[a, b] | Labeled[a, b] | Multi[a, b]
  
  ```

variant_poly
  $ run_test variant_poly.yaml
  ```python
  from __future__ import annotations
  from dataclasses import dataclass
  from typing import TypeVar, Generic, TypeAlias
  
  T = TypeVar('T')
  
  
  @dataclass
  class Some(Generic[T]):
      value: T
  
  
  option: TypeAlias = Some[T] | None
  a = TypeVar('a')
  
  
  @dataclass
  class Point:
      pass
  
  
  @dataclass
  class Circle(Generic[a]):
      arg0: a
  
  
  @dataclass
  class Rectangle(Generic[a]):
      arg0: a
      arg1: a
  
  
  @dataclass
  class Triangle(Generic[a]):
      a: a
      b: a
      c: a
  
  
  shape_poly = Point | Circle[a] | Rectangle[a] | Triangle[a]
  
  ```

variant_recursive
  $ run_test variant_recursive.yaml
  ```python
  from __future__ import annotations
  from dataclasses import dataclass
  from typing import TypeVar, Generic, TypeAlias
  
  T = TypeVar('T')
  
  
  @dataclass
  class Some(Generic[T]):
      value: T
  
  
  option: TypeAlias = Some[T] | None
  
  
  @dataclass
  class Leaf:
      arg0: int
  
  
  @dataclass
  class Node:
      arg0: tree
      arg1: tree
  
  
  tree = Leaf | Node
  
  ```

variant_simple
  $ run_test variant_simple.yaml
  ```python
  from __future__ import annotations
  from dataclasses import dataclass
  from typing import TypeVar, Generic, TypeAlias
  
  T = TypeVar('T')
  
  
  @dataclass
  class Some(Generic[T]):
      value: T
  
  
  option: TypeAlias = Some[T] | None
  
  
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
  
  ```

variant_two
  $ run_test variant_two.yaml
  ```python
  from __future__ import annotations
  from dataclasses import dataclass
  from typing import TypeVar, Generic, TypeAlias
  
  T = TypeVar('T')
  
  
  @dataclass
  class Some(Generic[T]):
      value: T
  
  
  option: TypeAlias = Some[T] | None
  
  
  @dataclass
  class Circle:
      arg0: int
  
  
  @dataclass
  class Polygon:
      arg0: rect
  
  
  shape = Circle | Polygon
  
  ```

variant_with_composite_payload
  $ run_test variant_with_composite_payload.yaml
  ```python
  from __future__ import annotations
  from dataclasses import dataclass
  from typing import TypeVar, Generic, TypeAlias
  
  T = TypeVar('T')
  
  
  @dataclass
  class Some(Generic[T]):
      value: T
  
  
  option: TypeAlias = Some[T] | None
  
  
  @dataclass
  class Circle:
      arg0: option[int]
  
  
  shape2 = Circle
  
  ```

variant_with_payload
  $ run_test variant_with_payload.yaml
  ```python
  from __future__ import annotations
  from dataclasses import dataclass
  from typing import TypeVar, Generic, TypeAlias
  
  T = TypeVar('T')
  
  
  @dataclass
  class Some(Generic[T]):
      value: T
  
  
  option: TypeAlias = Some[T] | None
  
  
  @dataclass
  class Point:
      pass
  
  
  @dataclass
  class Circle:
      arg0: int
  
  
  @dataclass
  class Rectangle:
      arg0: int
      arg1: int
  
  
  @dataclass
  class Triangle:
      a: int
      b: int
      c: int
  
  
  shape = Point | Circle | Rectangle | Triangle
  
  ```

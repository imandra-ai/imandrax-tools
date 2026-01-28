Type Declaration

## tuple_two_int

```iml
type two_int = int * int
```

```ts

```


## record

```iml
type point = { x: int; y: int }
```

```ts

```


## real_and_option

```iml
type my_ty = {
  x: real;
  y: int option;
  z: int;
}
```

```ts

```


## variant_with_composite_payload

```iml
type shape2 =
| Circle of int option
```

```ts

```


## GADT_monomorphic

```iml
type _ expr =
  | Int: int -> int expr
  | Add: int expr * int expr -> int expr
```

```ts

```


## record_with_composite_type

```iml
type shape = {
  circle : int option
}
```

```ts

```


## variant_with_payload

```iml
type shape =
| Point
| Circle of int
| Rectangle of int * int
| Triangle of {a: int; b: int; c: int}
```

```ts

```


## nested_generics

```iml
type 'a identity = Identity of 'a

type 'a maybe =
  | Just of 'a
  | Nothing

type 'a validated =
  | Valid of 'a
  | Invalid of string

type 'a tagged = {
  value : 'a;
  tag : string
}

type my_ty =
  | My_ty of int identity maybe validated tagged
```

```ts

```


## variant_two

```iml
type rect =
| Rectangle of int * int
| Square of int

type shape =
| Circle of int
| Polygon of rect
```

```ts

```


## variant_simple

```iml
type color = Red | Green | Blue

let color_value = fun c ->
  match c with
  | Red -> 1
  | Green -> 2
  | Blue -> 3
```

```ts

```


## variant_poly

```iml
type 'a shape_poly =
| Point
| Circle of 'a
| Rectangle of 'a * 'a
| Triangle of {a: 'a; b: 'a; c: 'a}
```

```ts

```


## variant_recursive

```iml
type tree =
  | Leaf of int
  | Node of tree * tree ;;
```

```ts

```

## variant_poly_two_var

```iml
type ('a, 'b) container =
  | Empty
  | Single of 'a
  | Pair of 'a * 'b
  | Labeled of { key: 'a; value: 'b }
  | Multi of 'a list * 'b list
```

```ts

```

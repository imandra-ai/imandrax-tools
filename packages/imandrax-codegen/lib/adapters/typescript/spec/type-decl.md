Type Declaration

## Preliminaries
Option lib
```ts
interface Some<T> {
  value: T;
}

type Option<T> = Some<T> | null;
```

Note: we model variant types as discriminated unions, with tag and payload fields
to avoid property name collisions.

## tuple_two_int

```iml
type two_int = int * int
```

```ts
type TwoInt = [number, number];
```


## record

```iml
type point = { x: int; y: int }
```

```ts
type Point = {
  x: number;
  y: number;
};
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
type MyTy = {
  x: number;
  y: Option<number>;
  z: number;
}
```

Or (lossy),
```ts
type MyTy = {
  x: number;
  y: number | null;
  z: number;
};
```


## variant_with_composite_payload

```iml
type shape2 =
| Circle of int option
```

```ts
type Shape2 = { tag: "Circle"; payload: Option<number> };
```


## record_with_composite_type

```iml
type shape = {
  circle : int option
}
```

```ts
type Shape = {
  circle: Option<number>;
};
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
type Shape =
  | { tag: "Point"; payload: null }
  | { tag: "Circle"; payload: number }
  | { tag: "Rectangle"; payload: [ number, number ] }
  | { tag: "Triangle"; payload: { a: number; b: number; c: number } };
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
type Identity<A> = { tag: "Identity"; payload: A };

type Maybe<A> =
  | { tag: "Just"; payload: A }
  | { tag: "Nothing", payload: null };

type Validated<A> =
  | { tag: "Valid"; payload: A }
  | { tag: "Invalid"; payload: string };

type Tagged<A> = {
  value: A;
  tag: string;
};

type MyTy = {
  tag: "MyTy";
  payload: Tagged<Validated<Maybe<Identity<number>>>>;
};
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
type Rect =
  | { tag: "Rectangle"; value: [number, number] }
  | { tag: "Square"; value: number };

type Shape =
  | { tag: "Circle"; value: number }
  | { tag: "Polygon"; value: Rect };
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
type Color = "Red" | "Green" | "Blue";
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
type ShapePoly<A> =
  | { tag: "Point" }
  | { tag: "Circle"; payload: A }
  | { tag: "Rectangle"; payload: [A, A] }
  | { tag: "Triangle"; payload: { a: A; b: A; c: A } };
```


## variant_recursive

```iml
type tree =
  | Leaf of int
  | Node of tree * tree ;;
```

```ts
type Tree =
  | { tag: "Leaf"; payload: number }
  | { tag: "Node"; payload: [ Tree, Tree ] };
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
type Container<A, B> =
  | { tag: "Empty" }
  | { tag: "Single"; payload: A }
  | { tag: "Pair"; payload: [A, B] }
  | { tag: "Labeled"; payload: { key: A; value: B } }
  | { tag: "Multi"; payload: [A[], B[]] };
```



## GADT_monomorphic

```iml
type _ expr =
  | Int: int -> int expr
  | Add: int expr * int expr -> int expr
```

```ts
type Expr<T> =
  | { tag: "Int"; payload: number }
  | { tag: "Add"; payload: [Expr<number>, Expr<number>] };
```

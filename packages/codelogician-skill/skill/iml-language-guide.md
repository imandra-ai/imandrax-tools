---
name: iml-language-guide
description: IML language guide. Covers the syntax and built-in annotations, and how ImandraX works with IML. Includes examples, tips and pitfalls.
---

# IML Language Guide

Rougly speaking: IML has OCaml syntax, but:
- it has its own prelude instead of OCaml standard library (see below and [all-prelude-module-signatures.md](./reference/all-prelude-module-signatures.md))
- it's pure (no exceptions and other side effects)
- it has a few special syntaxes and built-in annotations to trigger different ImandraX tasks (verification, region decomposition, etc.))
    - syntax: `verify`, `instance`, `lemma`, `theorem`, `axiom`, `test`, `eval`. They all have the same grammar rule as `let`!
    - built-in-annotations: 
        - `[@@measure <measure-function>]`: annotation for termination measure (when default measure is not sufficient)
        - `[@@decomp top <decomp-args> ()]`: attached to a function definition to trigger region decomposition
        - `[@@by <tactic>]`, `[@@rw]`, `[@@fc]`, ...: for tactic-based proofs, installing rewrite rules, installing forward chaining rules, etc.

How ImandraX works with IML on a high level:
- ImandraX typechecks IML code.
- ImandraX spawns tasks (proof-obligations (including termination proving), region decomposition, eval, etc.) from IML and processes them.
- ImandraX returns the results of the tasks to the user.

## IML Prelude vs OCaml Standard Library

OCaml standard library modules are unavailable in IML. Instead, IML has its own prelude with different signatures.

- Example: `List.nth` in OCaml has signature `'a list -> int -> 'a`, while in IML it's `int -> 'a list -> 'a option`. They have different parameter order and return type.
- Available modules in IML prelude include: `Int`, `LChar`, `LString`, `List`, `Map`, `Multiset`, `Option`, `Real`, `Result`, `Set`, and `String`. See [all-prelude-module-signatures.md](./reference/all-prelude-module-signatures.md) for details.

## Numerical Representation and Precision

IML defaults to arbitrary-precision arithmetic: a float-like literal such as
`3.14159` is a `real`, represented exactly as `314159/100000`, not a floating-point
approximation. (Machine floats do exist, in a separate module, but `real` is the
default and the right choice for most modeling.) The two default numeric types,
`real` and `int`, each carry their own set of operators.

Notes:
- `3.14159` has type `real`, not `float`
- Real arithmetic: `( +. )`, `( -. )`, `( *. )`, `( /. )` are operators with signatures `real -> real -> real`.
- Integer arithmetic: `( + )`, `( - )`, `( * )`, `( / )` are operators with signatures `int -> int -> int`.
- Gotcha: However, `( = ) : 'a -> 'a -> bool` is polymorphic and can be used for both real and integer equality. `( =. )` does not exist.
- Tip: Default to `real` when modeling. The solver handles it more efficiently due to continuous geometry. Use `int` when the quantity is inherently discrete (counters, indices, sequence numbers) or when divisibility/modular properties are part of the spec itself. 
- Conversion functions:
    - `Real.of_int : int -> real`
    - `Real.to_int : real -> int`

## String, Logic-mode character, and Logic-mode string
- Logic-mode character `LChar.t` is 8-bit character.
- Logic-mode string `LString.t` is a type alias for `LChar.t list`.
  - `{l|...|l}` creates a `LString.t` literal

## Error Handling 

IML is a pure language with no exceptions:
- The OCaml's `failwith` function isn not available in IML.
- Instead, either:
  - Transform partial functions into total functions
  - Use monadic error handling with `Result` or `Option` modules

## Type System Constraints

Unlike OCaml, IML restricts function representation in composite types:

- Functions **cannot** be part of algebraic data types, records, or tuples
- For state transition modeling, define:
  - A dedicated event type (with parameterized constructors if needed)
  - A step function that applies events to states

## Examples

Each example demonstrates key concepts in IML through:
1. Explanatory comments before functions that highlight important concepts
2. REPL evaluation results showing type signatures and values. Comments marked with
`(* val ... *)` show the REPL's response, helping you understand type inference and
evaluation.
These comments are only for educational purposes. They teach you about the thinking
process of writing IML code but they might not desired to exist when writing IML code.

The examples use plain first-order recursive style (explicit `let rec` + `match`
instead of `List.fold_left`/`List.map` with lambdas). This is idiomatic IML, and it
also keeps the code easy for ImandraX to reason about should you later want to
verify properties of it.

### Example 1
```iml
(* A float-like literal is interpreted as type `real`: an arbitrary-precision real number. *)
let pi = 3.14159
(* val pi : real = 314159/100000 *)
(* Arithmetic for `real` uses dotted operators: `+.`, `-.`, `*.`, `/.` *)
let circle_area (d : real) : real =
  let r = d /. 2.0 in
  pi *. r *. r
(* val circle_area : real -> real = <fun> *)
```

### Example 2
```iml
(* Note: `int` is the type for integers (arbitrary precision). *)
let rec count_negatives (row : int list) : int =
  match row with
  | [] -> 0
  | x :: rest -> (if x < 0 then 1 else 0) + count_negatives rest
(* val count_negatives : int list -> int = <fun> *)
let x = count_negatives [-1; -2; -3; 4; 5]
(* val x : int = 3 *)
(* Note:
   - `List.nth : int -> 'a list -> 'a option` — argument order differs from OCaml,
     and it returns an `option` instead of raising an exception.
   - Handle the option with `match` ... *)
let count_negatives_in_row (m : int list list) (i : int) : int option =
  match List.nth i m with
  | None -> None
  | Some row -> Some (count_negatives row)
(* val count_negatives_in_row : int list list -> int -> int option = <fun> *)
(* ... or with the binding operators `let*` / `let+` from the `Option` module: *)
let count_negatives_in_row' (m : int list list) (i : int) : int option =
  let open Option in
  let+ row = List.nth i m in
  count_negatives row
(* val count_negatives_in_row' : int list list -> int -> int option = <fun> *)
```

### Example 3
```iml
(* Note:
   - integer division and modulo in IML: `/` and `mod : int -> int -> int`.
*)
let is_divisor (n : int) (d : int) : bool =
  n mod d = 0
(* val is_divisor : int -> int -> bool = <fun> *)
(* Operations unavailable in IML (like square root) can be stubbed with
   `[@@opaque]`: a typed placeholder that lets the code compile.
   But note that it hinders verification so should be avoided unless necessary. *)
let sqrt : real -> real = () [@@opaque]
(* val sqrt : real -> real = <fun> *)
(* Conversion between `int` and `real`: `Real.of_int` and `Real.to_int`. *)
let rec sum_divisors_upto (n : int) (d : int) : int =
  if d < 2 then 0
  else if is_divisor n d then
    let q = n / d in
    (if d = q then d else d + q) + sum_divisors_upto n (d - 1)
  else
    sum_divisors_upto n (d - 1)
(* val sum_divisors_upto : int -> int -> int = <fun> *)
let sum_of_proper_divisors (num : int) : int =
  let limit = Real.to_int (sqrt (Real.of_int num)) in
  1 + sum_divisors_upto num limit
(* val sum_of_proper_divisors : int -> int = <fun> *)
```

### Example 4
```iml
(* Another example with conversion between `real` and `int`. *)
let sum_of_first_n_numbers (n : int) : real =
  (Real.of_int n *. (Real.of_int n +. 1.0)) /. 2.0
(* val sum_of_first_n_numbers : int -> real = <fun> *)
let sum_of_squares_of_first_n_numbers (n : int) : real =
  (Real.of_int n *. (Real.of_int n +. 1.0) *. (2.0 *. Real.of_int n +. 1.0)) /. 6.0
(* val sum_of_squares_of_first_n_numbers : int -> real = <fun> *)
let sum_matrix_element_absolute_difference (n : int) : int =
  let sum_first_n = sum_of_first_n_numbers n in
  let sum_squares_n = sum_of_squares_of_first_n_numbers n in
  (sum_first_n +. sum_squares_n) |> Real.to_int
(* val sum_matrix_element_absolute_difference : int -> int = <fun> *)
```

### Example 5
```iml
(* Note:
   - `min : 'a -> 'a -> 'a` and `abs : int -> int` are available at top level.
   - There is no `max_int` in IML (ints are arbitrary-precision), so instead of a
     magic sentinel for "no result", return an `int option` and use `None`.
*)
(* Smallest absolute difference between `a` and every element of `rest`.
   `None` when `rest` is empty (no element to compare against). *)
let rec min_abs_diff_with (a : int) (rest : int list) : int option =
  match rest with
  | [] -> None
  | b :: more ->
    let d = abs (a - b) in
    (match min_abs_diff_with a more with
     | None -> Some d
     | Some m -> Some (min d m))
(* val min_abs_diff_with : int -> int list -> int option = <fun> *)
(* Smallest absolute difference over all pairs drawn from the list.
   `None` when there are fewer than two elements (no pair exists). *)
let rec min_abs_diff_pair (arr : int list) : int option =
  match arr with
  | [] | [_] -> None
  | a :: rest ->
    (match min_abs_diff_with a rest, min_abs_diff_pair rest with
     | None, r | r, None -> r
     | Some x, Some y -> Some (min x y))
(* val min_abs_diff_pair : int list -> int option = <fun> *)
```

### Example 6
```iml
(* Termination is on the shrinking list, not the index `i` (which could be
   negative), so guide ImandraX with an explicit `[@@measure ...]`. *)
(* Replace the element at index i (0-based), leaving the list unchanged if
   the index is out of range: *)
let rec set_nth (i : int) (v : 'a) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | x :: rest -> if i = 0 then v :: rest else x :: set_nth (i - 1) v rest
[@@measure Ordinal.of_int (List.length l)]
(* val set_nth : int -> 'a -> 'a list -> 'a list = <fun> *)
(* Swap the elements at indices i and j; matching a pair of `List.nth`
   results handles the out-of-range cases in one shot. *)
let swap_elements (l : 'a list) (i : int) (j : int) : 'a list =
  match List.nth i l, List.nth j l with
  | Some xi, Some xj -> set_nth i xj (set_nth j xi l)
  | _ -> l
(* val swap_elements : 'a list -> int -> int -> 'a list = <fun> *)
```

### Example 7
```iml
(* Character and String:
   - IML supports logic-mode character `LChar.t` and logic-mode string `LString.t`.
   - `{l|...|l}` is used to create a logic-mode string, `LString.t`, aka `LChar.t list`.
   - To create a logic-mode character, `LChar.t`, you can use list operations on
     `LString.t`, e.g. `List.hd {l|...|l}`.
*)
let char_0 : LChar.t = List.hd {l|0|l}
(* val char_0 : LChar.t = '0' *)
let rec count_zeros (s : LString.t) : int =
  match s with
  | [] -> 0
  | ch :: rest -> (if ch = char_0 then 1 else 0) + count_zeros rest
(* val count_zeros : LString.t -> int = <fun> *)
let count_bits (s : LString.t) : int * int =
  let zeros = count_zeros s in
  (zeros, List.length s - zeros)
(* val count_bits : LString.t -> int * int = <fun> *)
let can_be_made_uniform_with_one_flip (s : LString.t) : bool =
  let zeros, ones = count_bits s in
  zeros = 1 || ones = 1
(* val can_be_made_uniform_with_one_flip : LString.t -> bool = <fun> *)
```


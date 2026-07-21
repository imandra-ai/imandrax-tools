---
name: termination-proving
description: Termination proving using [@@measure ...] and the Ordinal module. Every `let rec` definition triggers a termination proving task. Read this when the default termination proving heuristics fail.
tag:
  - termination-proving
---

# Basic

Since IML serves as both a programming language and a logic, function termination must be provable:

- *Every* `let rec` definition triggers a termination proving task. All recursive functions need to be *admitted* before they can be used. The admission includes proving termination.
- For common recursion patterns, termination is proven automatically. When no custom measure is given, ImandraX's heuristics will select the first argument that is tested in every branch, i.e., automatic measure synthesis will try a measure with a single argument.
- When default measure is not sufficient, provide explicit termination measures using `[@@measure ...]` or `[@@adm ...]`.

**Ordinals in Termination Proofs:**
Termination measures in IML must return values of type `Ordinal.t`. This type represents ordinals up to ε₀ in Cantor Normal Form, providing a well-founded ordering essential for proving termination. When defining measures, you can use functions like `Ordinal.of_int` to convert integers to ordinals, or construct more complex ordinals for nested recursion.

**Example with explicit measure:**
```iml
let left_pad_measure n xs =
  Ordinal.of_int (n - List.length xs)

let rec left_pad c n xs =
  if List.length xs >= n then
    xs
  else
    left_pad c n (c :: xs)
[@@measure left_pad_measure n xs]
```

The measure shows that `n - List.length xs` decreases with each recursive call and remains non-negative, proving termination. The `Ordinal.of_int` function converts this integer difference to an ordinal value that Imandra can use to establish a well-founded ordering.

Note that `[@@measure ...]` can only be used for top-level functions, i.e., functions that are not defined within other functions.


# More examples and practical tips

# How to Prove Termination

This guide shows you how to prove that your recursive functions terminate. Use this when ImandraX rejects a function due to termination concerns.

## Quick Decision Tree

```
Is your function rejected for termination?
├─ Does it use structural recursion on datatypes?
│   └─ Should work automatically. Check recursion structure.
├─ Does it have simple decreasing integer arguments?
│   └─ Should work automatically. Verify the decrease.
├─ Do arguments decrease lexicographically?
│   └─ Use [@@adm arg1, arg2, ...]
└─ Need custom measure?
    └─ Use [@@measure fn arg1 arg2 ...]
```

## Approach 1: Rely on Automatic Detection

ImandraX handles these automatically:

**Structural recursion on lists:**
```iml
let rec sum_list = function
  | [] -> 0
  | x :: xs -> x + sum_list xs
(* Works automatically *)
```

**Simple decreasing integers:**
```iml
let rec countdown x =
  if x <= 0 then 0
  else x + countdown (x - 1)
(* Works automatically *)
```

NOTE: When no custom measure is given, ImandraX's heuristics will select the first argument that is tested in every branch, i.e., automatic measure synthesis will try a measure with a single argument.

## Approach 2: Lexicographic Ordering with @@adm

Use when multiple arguments decrease in order:

```iml
let rec ackermann m n =
  if m <= 0 then n + 1
  else if n <= 0 then ackermann (m - 1) 1
  else ackermann (m - 1) (ackermann m (n - 1))
[@@adm m, n]
```

This tells ImandraX: "m decreases first; when m stays the same, n decreases."

**When to use:**
- Multiple recursive calls with different decreasing patterns
- Nested recursion (like Ackermann)
- Arguments that decrease in a specific priority order

## Approach 3: Custom Measure with @@measure

Use when you need to compute a decreasing quantity:

**Step 1:** Define a measure function returning `Ordinal.t`:
```iml
let left_pad_measure n xs =
  Ordinal.of_int (n - List.length xs)
```

**Step 2:** Attach the measure to your function:
```iml
let rec left_pad c n xs =
  if List.length xs >= n then xs
  else left_pad c n (c :: xs)
[@@measure left_pad_measure n xs]
```

**Key points:**
- Measure function must return `Ordinal.t`
- Use `Ordinal.of_int` to convert integers
- Include only the arguments that affect the measure


# Idioms

Recurring measure/termination patterns from expert proof corpora:

**`max 0 e` guard.** Ints can be negative; ordinals can't. Whenever the
decreasing quantity could dip below zero on bad inputs, guard it — this is the
standard idiom, used on nearly every int-recursion:

```iml
let rec real_pow (g : real) (n : int) : real =
  if n <= 0 then 1.0 else Real.(g * real_pow g (n - 1))
[@@measure Ordinal.of_int (max 0 n)]
```

**Upward recursion.** Measure by distance to the bound:
`[@@measure Ordinal.of_int (List.length l + 1 - k)]` for a function recursing
on `k + 1` up to `List.length l`.

**Lexicographic measures.** `Ordinal.pair` (see [ordinal.md](./reference/ordinal.md)):
`[@@measure Ordinal.pair (Ordinal.of_int (List.length p)) (Ordinal.of_int (List.length cands))]`.

**Lemma-assisted admission (`[@@by]` on the definition).** A `[@@by]` attached
to a `let rec` is the tactic for its termination proof obligation. When the
decrease isn't syntactic, prove supporting lemmas *first* and `[%use]` them in
the admission script:

```iml
lemma len_remove_lt x p = List.mem x p ==> List.length (remove x p) < List.length p
[@@by auto]

let rec perms_aux (candidates : int list) (p : int list) : int list list = ...
[@@measure Ordinal.pair (Ordinal.of_int (List.length p))
                        (Ordinal.of_int (List.length candidates))]
[@@by [%use len_remove_lt (List.hd candidates) p] @> simplify () @>>| auto]
```

This is how functions with data-dependent decrease (e.g. recursing on `n / d`)
get admitted: the expert corpus admits `prime_factors` with a six-`[%use]`
termination script.

**Defensive unreachable guards.** When the decrease only holds under invariants
the admission check can't see, add an early-return branch that makes termination
trivial, and *refute that branch later* in the correctness proof:

```iml
let rec descent_loop (m : int) (q : sq4) : sq4 =
  let mp = next_m m q in
  if mp <= 0 || mp >= m then q   (* unreachable under correct hypotheses *)
  else descent_loop mp (descent_q m q)
[@@measure Ordinal.of_int (max 0 m)]
```

The correctness theorem then proves `mp` is always in range, so the guard branch
never fires on valid inputs. This cleanly separates "it terminates" from "it's
correct". (Similarly: guard inputs, e.g. gcd returning `0` on negatives.)

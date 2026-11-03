---
name: region-decomp-advanced-features
description: Composing region decompositions inside the decomp attribute - the types m / ret / refiner, the operators <<, <|<, ~|, |>>, the refiners prune / enumerate / enumerate_all, let-bindings, and rule specs
---
# Composing Region Decompositions

[region-decomp-intro.md](../region-decomp-intro.md) covers `top ()` and its
flags. A single `top` only ever decomposes one function in one go. The
`[@@decomp ...]` attribute actually accepts a small expression language over
the `Decomp` module, which is more expressive: you can decompose a caller and
its callees separately and merge them, collapse regions by output, enumerate
the values of a classifier inside every region, and name intermediate results.

## Types in a decomp expression

Every expression inside `[@@decomp ...]` has one of four types from the
`Decomp` module:

| Type | Meaning | How you get one |
|---|---|---|
| `m` | a decomposition *waiting for a function name* | `top ()`, `top ~basis:... ()` |
| `ret` | a finished decomposition (`(t, string) result`) | apply an `m` to a name: `top () [%id f]` |
| `refiner` | a transformation `t -> ret` | `prune`, `combine`, `enumerate e`, `enumerate_all ~style [...]` |
| `t` | the raw decomposition inside a `ret` | you never write one directly |

The attribute as a whole must have type `m`; the decorated function's name is
applied to it implicitly. So in

```iml
let d = g [@@decomp top ~basis:[[%id f]] () << top () [%id f]]
```

the left of `<<` is an `m` (it will be applied to `g`) and the right is a
`ret` (already applied to `f`). This is the rule to keep in mind: **the
decorated function is implicit on the left; every other function is written
out with `[%id ...]` on the right.**

Signatures of the pieces (from the prelude):

```iml
val top : ?assuming:identifier -> ?basis:identifier list
       -> ?rule_specs:identifier list -> ?prune:bool -> ?ctx_simp:bool
       -> ?lift_bool:lift_bool -> unit -> m

val ( |>> ) : m -> refiner -> m         (* apply a refiner *)
val ( << )  : m -> ret -> m             (* merge *)
val ( <|< ) : m -> ret -> m             (* compound merge *)
val ( ~| )  : m -> m                    (* combine; same as [|>> combine] *)

val prune   : refiner
val combine : refiner
val enumerate     : ret -> refiner
val enumerate_all : style:style -> ret list -> refiner   (* Additive | Multiplicative *)
```

Plain `let x = ... in ...` is also allowed inside the attribute, so an `m` or
`ret` can be named and reused. It is ordinary IML syntax, not an operator.

## Running example

Every example below decomposes `g`, which calls `f`. `pos` is used where an
operation needs a second function with `g`'s parameters. Region listings are
ImandraX output written as `constraints -> invariant`.

```iml
let f (x : int) : int = if x > 12 then 2 else if x > 4 then 1 else if x < -4 then 1 else 0
let g (x : int) : int = if x > 8 then 1 else f (x + 1)
let pos (x : int) : bool = x > -3
```

The two starting points:

```iml
let d_full  = g [@@decomp top ()]
let d_basis = g [@@decomp top ~basis:[[%id f]] ()]
```

```text
top () on g                          top ~basis:[[%id f]] () on g
  x >= 9              -> 1             x >= 9  -> 1
  x <= -6             -> 1             x <= 8  -> f (x + 1)
  x >= 4 && x <= 8    -> 1
  x >= 12 && x <= 8   -> 2   (empty)
  x >= -5 && x <= 3   -> 0

top () on f
  x >= 13             -> 2
  x <= -5             -> 1
  x >= 5 && x <= 12   -> 1
  x >= -4 && x <= 4   -> 0
```

With `~basis`, `f (x + 1)` stays opaque and `g` splits only on its own `if`.
Without it `f` is inlined and its branches split the input further. The region
`x >= 12 && x <= 8` is unsatisfiable but kept, because pruning is off by
default.

## Merge: `d << e`

`e` decomposes a function that appears in `d`'s regions, in a constraint or an
invariant. Typically that is a function listed in `d`'s `~basis`. Every region
of `d` that mentions the function is replaced by one piece per region of `e`.
Each piece

- keeps the original region's constraints,
- adds the constraints of that region of `e`, instantiated at the argument the
  function is called with,
- replaces the call by that region's invariant.

Regions that do not mention the function are left unchanged.

```iml
let d_merge = g [@@decomp top ~basis:[[%id f]] () << top () [%id f]]
```

```text
x >= 9              -> 1     (untouched: no f here)
x <= -6             -> 1     (from f's x <= -5, at x + 1)
x >= 4 && x <= 8    -> 1     (from f's 5 <= x <= 12, at x + 1, cut by x <= 8)
x >= 12 && x <= 8   -> 2     (from f's x >= 13; empty, kept)
x >= -5 && x <= 3   -> 0     (from f's -4 <= x <= 4)
```

`f`'s constraint `x >= 5` becomes `x + 1 >= 5`, i.e. `x >= 4`. The result is
the same partition as `top ()` without a basis. That is the point: decompose a
caller and each callee separately, keeping each decomposition small, then
compose them.

When the function appears in a constraint, e.g. `if f x = 1 then 10 else 20`,
the region `f x = 1` is cut by `f`'s regions and only pieces where `f`'s value
makes the constraint true survive.

Several basis functions are merged by chaining:

```iml
top ~basis:[[%id f]; [%id h]] () << top () [%id f] << top () [%id h]
```

Merge is idempotent and, for distinct callees, order-independent. Merging a
decomposition of a function that does not appear in `d` is a no-op.

## Combine: `~| d`

Joins all regions with the same invariant into one region whose constraint is
the disjunction of theirs.

```iml
let d_combine = g [@@decomp ~| (top ())]
```

```text
x >= 9 || x <= -6 || (x <= 8 && x >= 4)   -> 1
x >= 12 && x <= 8                          -> 2
x >= -5 && x <= 3                          -> 0
```

Use it when you care about distinct outputs rather than distinct paths: one
region per behaviour.

## Compound merge: `d <|< e`

A merge that also joins the pieces coming from the *same* region of `d` when
they end up with equal invariants. It is equivalent to combining `e` before
merging it: `d <|< top () [%id f]` gives the same partition as
`d << (~| (top ())) [%id f]`. Pieces from different regions of `d` stay
separate.

```iml
let d_cmerge = g [@@decomp top ~basis:[[%id f]] () <|< top () [%id f]]
```

```text
x >= 9                         -> 1    (its own region of d, not joined)
x <= -6 || (x <= 8 && x >= 4)  -> 1    (both pieces of d's x <= 8 that give 1)
x >= 12 && x <= 8              -> 2
x >= -5 && x <= 3              -> 0
```

From finest to coarsest: `d << e` has 5 regions, `d <|< e` has 4, and
`~| (d << e)` has 3. Compound merge keeps the caller's own path distinctions
while collapsing callee paths that lead to the same value.

## Prune: `d |>> prune`

Removes regions whose constraints no input satisfies, and attaches a witness
input (`model`) to each remaining region.

```iml
let d_prune = g [@@decomp top () |>> prune]
```

```text
x >= 9              -> 1
x <= -6             -> 1
x >= 4 && x <= 8    -> 1
x >= -5 && x <= 3   -> 0
```

`top ~prune:true ()` gives the same result when `top` is the whole expression.
In a composed expression the two differ: `~prune:true` prunes the `top` result
*before* the other operators run, so regions emptied by a later merge or
`~assuming` are kept and the witnesses of the pre-merge regions are no longer
valid for the merged ones. Put `|>> prune` **last** when you want a clean,
witnessed result:

```iml
let d = g [@@decomp (top ~basis:[[%id f]] () << top () [%id f]) |>> prune]
```

Refiners chain freely: `top () |>> prune |>> combine`, `~| (top () |>> prune)`,
`(top ... () |>> prune) << e` are all accepted.

## Restrict the domain: `top ~assuming:...`

Not an operator, but it composes with all of the above. Only inputs satisfying
the assumption are decomposed. The assumption must have exactly the same
parameters as the decomposed function.

```iml
let d_assume = g [@@decomp top ~assuming:[%id pos] ()]
```

```text
x >= 9              -> 1
x >= -2 && x <= -6  -> 1   (empty, kept)
x >= 4 && x <= 8    -> 1
x >= 12 && x <= 8   -> 2   (empty, kept)
x >= -2 && x <= 3   -> 0   (was -5..3)
```

The assumption carries through a merge:
`top ~assuming:[%id pos] ~basis:[[%id f]] () << top () [%id f]` covers exactly
the assumed inputs. Add `|>> prune` at the end to drop the regions the
assumption made empty.

## Enumerate: `d |>> enumerate e`

`e` decomposes a function with **exactly the same parameters** as the
decomposed function. It does not need to appear in `d` at all. For each region
of `d`, enumerate computes every concrete value that function can return
inside the region and adds one child region per value, meaning "this region,
and the function returns this value". The parent regions are kept; each child
carries metadata `base = <parent region id>` in the artifact.

```iml
let d_enum = g [@@decomp top () |>> enumerate (top () [%id f])]
```

```text
x >= 9              -> 1
  ├ f x = 1 && x >= 9            -> 1
  └ f x = 2 && x >= 9            -> 1
x <= -6             -> 1
  └ f x = 1 && x <= -6           -> 1
x >= 4 && x <= 8    -> 1
  ├ f x = 0 && x >= 4 && x <= 8  -> 1    (x = 4)
  └ f x = 1 && x >= 4 && x <= 8  -> 1    (5..8)
x >= 12 && x <= 8   -> 2                 (empty: no children)
x >= -5 && x <= 3   -> 0
  ├ f x = 0 && ...               -> 0
  └ f x = 1 && ...               -> 0    (x = -5)
```

Note that `f` is applied to `x`, the decomposed function's own parameter, not
to the `x + 1` that `g` passes to it. That is why the parameters must match: a
function with a different arity fails with
`Invalid_argument("List.for_all2")`.

Enumerate splits by **value**, not by `e`'s regions. Regions of `e` that return
the same value share one child, and a non-constant invariant such as `x + 1`
yields one child per value it takes inside the region. The enumerated function
must therefore take finitely many values in every region; otherwise
enumeration does not terminate. Boolean predicates and variant-returning
classifiers always qualify: enumerating by `pos` splits each region into
`pos x` and `not (pos x)`.

Typical use: enumerate by a classifier to get test cases that cover every
output class within every path.

Do not confuse this with merge, even though both take a second decomposition
on the right. Merge *substitutes* a callee at its call site and replaces the
regions; enumerate *tags* every region with the values of an unrelated
function of the same inputs and keeps the regions.

## Enumerate by several functions: `enumerate_all`

```iml
let d_add = g [@@decomp top () |>> enumerate_all ~style:Additive       [top () [%id f]; top () [%id pos]]]
let d_mul = g [@@decomp top () |>> enumerate_all ~style:Multiplicative [top () [%id f]; top () [%id pos]]]
```

- **`Additive`**: the union. A region's children are the children from
  enumerating by each function separately, so children of one region overlap.
- **`Multiplicative`**: the product. Each child pins all functions' values at
  once, keeping only feasible combinations, so children of one region are
  disjoint.

```text
Additive                                   Multiplicative
x >= -5 && x <= 3 -> 0                     x >= -5 && x <= 3 -> 0
  ├ f x = 0                                  ├ f x = 0 && pos x
  ├ f x = 1                                  ├ f x = 1 && not (pos x)
  ├ pos x                                    └ f x = 0 && not (pos x)
  └ not (pos x)
```

`f x = 1 && pos x` is infeasible on this region, so `Multiplicative` drops it.
`enumerate e` is `enumerate_all [e]`; with one function the two styles agree.
List order does not matter.

## Naming sub-expressions with `let`

```iml
let d = g [@@decomp let m = top ~basis:[[%id f]] () in ~| (m << top () [%id f])]
```

The server accepts this. The `codelogician eval check-decomp` command, which
parses the attribute client-side, currently rejects `let` with
`DecompParsingError: cannot parse let_expression`; run such files with
`codelogician eval check --with-decomps` instead.

## Witnesses after composition

Only `prune` computes witnesses. A `model` on a region that came out of a
merge or enumerate step without a trailing `|>> prune` was computed for a
different region and may not satisfy the constraints shown. Read witnesses
from the artifact, and finish composed expressions with `|>> prune` when you
intend to use them.

---

# Rule specs: `top ~rule_specs:[...]`

This is a different mechanism from the composition operators above. It does
not change *which* regions are produced; it changes how the terms inside the
constraints and invariants are simplified while the decomposition is computed.

A rule spec is an axiom declared with the `rule_spec` keyword. It is admitted
without proof, so it should state something you have proved elsewhere or are
willing to assume. The same attributes as for lemmas apply:

- `[@@rw]` makes it a rewrite rule, used to simplify terms inside constraints
  and invariants.
- `[@@fc]` makes it a forward-chaining rule, adding derived facts to a region's
  context so that feasibility checks and further simplification see them.

```iml
let h (xs : int list) (ys : int list) : int =
  if List.length (xs @ ys) > 3 then 1 else 0

rule_spec len_app (xs : int list) (ys : int list) =
  List.length (xs @ ys) = List.length xs + List.length ys
[@@rw]

let d_plain = h [@@decomp top ()]
let d_rs    = h [@@decomp top ~rule_specs:[[%id len_app]] ()]
```

```text
top () on h                        top ~rule_specs:[[%id len_app]] () on h
  length (xs @ ys) <= 3  -> 0        length xs + length ys <= 3  -> 0
  length (xs @ ys) >= 4  -> 1        length xs + length ys >= 4  -> 1
```

Same two regions; the rule has rewritten the opaque `length (xs @ ys)` into a
sum the solver can reason about. That matters when a later `~assuming`,
`prune` or merge needs to decide feasibility of a constraint, or when you want
the generated test-case constraints in a simpler form.

Use rule specs when the function under decomposition calls recursive library
functions that stay opaque in regions (`List.length`, `List.mem`, folds) and
you know an algebraic identity that exposes the structure you care about.

---
name: induction-control
description: Controlling induction in ImandraX - custom induction schemes via skeleton recursive functions and induction ~id, the anchor trick, pinned-variable induction, _aux-then-specialize, generalizing before inducting, and step lemmas that take induction hypotheses as premises. Read this when the default induction gives the wrong cases or an unusable IH.
---

# Induction Control

ImandraX derives induction schemes from recursive function definitions. When the
default choice (heuristic, or the recursion of a function in your goal) gives the
wrong cases or an unusable induction hypothesis, you don't fight it — you *supply*
the scheme. These techniques appear in essentially every hard expert proof.

## 1. Pick the scheme with `induction ~id:[%id f] ()`

`induction ~id:[%id f] ()` inducts along the recursion structure of function `f`
(similarly `induct ~on_fun:[%id f] ()`). Point it at whichever recursive function
*shapes the goal* — including imported ones (`~id:[%id Binomial.choose]`) and
recognizer predicates:

```iml
(* geometric_sequence recurses singleton / cons-with-ratio-check, giving
   exactly the two cases the sum theorem needs *)
theorem geo_sum_mult seq r = geometric_sequence seq r ==> ...
[@@by induction ~id:[%id geometric_sequence] () @>| [ <base>; <step> ]]
```

Use `~vars:["v"]` to pin the induction variable for structural induction, and
`[@@adm x, y]` on definitions to control which arguments drive admission (and
hence the derived scheme).

## 2. Throwaway skeleton functions: define the scheme you want

If no existing function has the right recursion, define a function whose *only
purpose* is its recursion structure — the body value is irrelevant:

```iml
(* Plain natural-number induction on an int parameter *)
let rec nat_ind n = if n <= 0 then true else nat_ind (n - 1)
[@@measure Ordinal.of_int (max 0 n)]

lemma choose_1 n = n >= 0 ==> choose n 1 = n
[@@by induction ~id:[%id nat_ind] () @>>| auto]
```

Richer schemes bake in exactly the IH you need:

```iml
(* IH on (tl s, xs), (s, tl xs), AND (tl s, tl xs) simultaneously *)
let rec fun_induct (s : real list) (xs : real list) =
  if s = [] || xs = [] then 0
  else fun_induct (List.tl s) xs + fun_induct s (List.tl xs)
       + fun_induct (List.tl s) (List.tl xs)
[@@adm s, xs]

(* "ys shrinks by remove h" — structural induction on xs would never give this *)
let rec ppe_ind xs ys n =
  match xs with [] -> true | h :: t -> ppe_ind t (remove h ys) n
```

A common special case: to prove a loop `descent_loop` correct, define a
`descent_ind` mirroring its exact recursion and drive the invariant proof with
`induction ~id:[%id descent_ind] () @>>| close_tac`.

## 3. The anchor trick: attaching a scheme to the goal

For `~id` to apply, the scheme function must relate to the goal. Two standard
ways to force it, both followed by deriving a *clean* lemma:

```iml
(* (a) vacuous hypothesis anchor *)
lemma subseq_tail_weak_h s xs =
  fun_induct s xs = fun_induct s xs && subseqp s xs ==> ...
[@@by induction ~id:[%id fun_induct] () @>| [...]]

lemma subseq_tail_weak s xs = subseqp s xs ==> ...
[@@by [%use subseq_tail_weak_h s xs] @> auto]

(* (b) conjoin into the conclusion, strip with an "always true" lemma *)
lemma pa_scheme_always c p = pa_scheme c p = true [@@by auto]
lemma ballot_gen_h ... = ... ==> (P ... && pa_scheme candidates p) [@@by ...]
```

The anchor also solves a **polymorphism** wrinkle: a scheme over `'a list list`
may need a dummy `(x : 'a)` argument so the type variable is anchored
(`nmc_ind x l k = nmc_ind x l k  (* anchor for polymorphism workaround *)`).

## 4. Pinned-variable induction and `_aux`-then-specialize

When your target fixes an argument that the recursion varies (e.g. you care about
`small_divisor_from 2 n` but the function recurses by incrementing that `2`):

- **Generalize** (`_aux` pattern): prove the lemma for a variable cursor `k` with
  range hypotheses, then specialize:
  ```iml
  lemma sd_divides_aux n k = 2 <= k && k <= max 2 n ==> ...
  lemma sd_divides n = ... [@@by [%use sd_divides_aux n 2] @> auto]
  ```
- **Pin**: keep the argument a *variable* constrained by hypothesis `j = 2`, so
  induction can still follow the recursion on `j`:
  ```iml
  lemma sd_minimal_key n k j =
    n > 1 && 2 <= k && j = 2 && k < small_divisor_from j n ==> ...
  ```
  Stating it directly with the constant `2` would make the IH unusable.

Argument *order* also matters — ImandraX's heuristics prefer certain positions,
and experts annotate reorderings: `(* s first so induction picks it *)
lemma foo_h s u = ...` then restate as `foo u s` via `[%use foo_h ...]`.

## 5. Strengthen (generalize) before inducting

If an "obvious" induction gets stuck at a point where the IH doesn't apply, the
statement is too specific. Standard strengthenings:

- Add a variable: prove `pow_ge_linear_scaled` (two variables) because the
  one-variable `pow_ge_linear` isn't inductive enough.
- Generalize a fixed endpoint: prove `P m n x` for `0 <= n <= m` instead of
  `P m m x`; specialize back with a one-line corollary.
- Generalize an accumulator: prove facts about `favp_from a b c e` for arbitrary
  accumulator values, not just the `0`-seeded wrapper.
- Generalize the collection: prove over any `candidates` with
  `sublistp candidates p`, then instantiate `candidates = p`.

Telltale sign you need this: an induction subgoal with *no applicable hypothesis*.

## 6. Step lemmas with IHs as explicit premises

Don't fight a hard inductive step in place. Extract it as a **non-inductive**
lemma whose hypotheses are exactly the IH instances, over fresh variables:

```iml
(* takes both IH equalities as premises; needs no induction itself *)
lemma choose_fact_interior_from_IH n k ihl ihr =
  0 < k && k < n
  && ihl = ...   (* IH at (n-1, k-1) *)
  && ihr = ...   (* IH at (n-1, k)   *)
  ==> choose n k * fact k * fact (n - k) = fact n
[@@by ... @> nonlin ()]
```

Then the inductive proof's step branch is just
`[%use choose_fact_interior_from_IH ...] @> auto`, instantiated at the
destructor terms the induction leaves you (`n - 1`, `List.hd xs`, `List.tl xs`).
Only the main lemma inducts; every helper is plain algebra.

## 7. Structuring the induction proof itself

```iml
[@@by induction () @>| [ <base tactic> ; <step tactic> ]]   (* per-subgoal *)
[@@by induction ~id:[%id f] () @>>| auto]                   (* same tactic on all *)
```

- `@>|` is positional — it depends on subgoal order. Keep each branch to a short
  chain; if a branch grows past a few steps, extract a step lemma (§6).
- Inside branches the surgical kit applies: `[%cases cond]` to split
  interior/boundary, `[%expand f args]` to unfold exactly one call (a bare
  `expand` may also hit the IH occurrence — prefer the applied form),
  `[%replace x]` to commit an equation, `lift_ifs @>>| auto` to split residual
  conditionals.
- A reusable closer can be named:
  `let close_sub = intros @> [%subgoal ...] @>| [...] in induction ... @>>| close_sub`.

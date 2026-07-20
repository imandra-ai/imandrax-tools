---
name: verification-guide
description: Verification guide for ImandraX, including tactic usage. Read this when working with any non-trivial proof-obligation tasks spawned by `let rec` (termination), `instance`, `verify`, `lemma` and `theorem`.
---

# Verification

## Commands

Commands that will trigger verification:

- `verify <func>`: takes a function representing a goal and attempts to prove it.
  If the proof attempt fails, ImandraX will try to synthesize a concrete
  counterexample illustrating the failure. When verifying a formula that doesn't depend on
  function parameters, `verify (<expr>)` is a shorthand for `verify (fun () -> <expr>)`.
  Verification will be bound by unrolling limits, specified by `[@@upto <n>]`

- `instance <func>`: takes a function representing a goal and attempts to
  synthesize an instance (i.e., a concrete value) that satisfies it. It is
  useful for answering the question "What is an example value that satisfies
  this particular property?". It's the reciprocal of `verify`. When `verify` refutes a property, the counterexample is what `instance` would find for the negated property.
    - `verify P` returns PROVEN if P holds for all inputs
    - `verify P` returns a COUNTEREXAMPLE if P fails somewhere
    - `instance P` finds an input where P is true

- `theorem <name> <vars> = <body>`: takes a name, variables and a function of
  the variables representing a goal to be proved. If ImandraX proves the goal,
  the named theorem is installed and may be used in subsequent proofs. Theorems
  can be tagged with attributes instructing ImandraX how the theorem should be
  (automatically) applied to prove other theorems in the future.

- `lemma <name> <vars> = <body>`: synonym of `theorem`, but idiomatically often used
   for "smaller" subsidiary results as one is working up to a larger `theorem`.

- `axiom <name> <vars> = <body>`: declares an axiom, effectively the same as
   `theorem` but forcing ImandraX to *assume* the truth of the conjecture, rather
   than verifying it. This is of course dangerous and should be used with
   extreme care.


## Tactics

Based on the `by` annotation (`[@@by auto]`), ImandraX allows users to structure proofs through a combination of smaller tactics and more sophisticated tacticals, enabling modular, reusable proof strategies. 

A tactic takes a goal and either:
- returns `Error _` to signal failure
- returns `Ok None` to signal that it did nothing (goal stays the same)
- returns `Ok (Some (subgoals, proof))` with new subgoals and the proof that these imply the input goal.

There's a wide range of tactics ranging from highly automated (`auto`, `unroll`, etc.) to surgical (`[%expand ...]` and `exact`).


### Inductive Waterfall `auto`

The auto tactic, `auto` as in `[@@by auto]`, is ImandraX's flagship automated inductive waterfall proof strategy, which combines simplification
(including automatic subgoaling, conditional rewriting and forward-chaining
using previously proved lemmas, decision procedures for datatypes and
arithmetic, etc.), and may decide to do induction. This is the most common *closer* for a `theorem` in Imandra.
  - Simplification is in many ways the most important part of the waterfall, and the step that most often causes a clause to evaporate or the goal to be refuted
  - Rewrite rules (`[@@rw]`) control simplification and, well-chosen, can make whole classes of goals close automatically — but see the calibration note below: in practice most lemmas are fed to proofs explicitly via `[%use]`, and only a *small, deliberately shaped* set of lemmas is installed as rules.
  - In expert practice `auto` almost never carries a hard proof alone: the dominant proof shape is a `[@@by]` script of explicitly instantiated lemmas (`intros @> [%use lemma args] @> ... @> auto`) with `auto` doing the final propositional/arithmetic glue. See [proof-method.md](./proof-method.md).

#### `auto` with more fine-grained control: `induct`

`induct` (not to be confused with `induction` tactic) is `auto` with some parameters exposed for controlling it. For example,
  - `~on_fun:<func_name>` - perform functional induction using an induction
    scheme derived from `<func_name>`. This is the default behavior.
  - `~on_vars:<args>` - perform structural induction on the arguments
    `<args>` if given, else on a heuristically chosen collection of variables.
    The types of the induction variables must be algebraic datatypes / variant
    types. An additive approach (linear in the total number of constructors) is
    taken for combining the schemes of individual variables into a composite
    induction scheme.
  - `~max_induct:<n>` - set the maximum induction depth to `<n>`.

It can be used to disable some features like generalization and destructor elimination etc.

#### Rule Classes
Theorems may be installed as rules, which instructs ImandraX to apply them in
certain ways during subsequent proof attempts. The development of an appropriate
collection of rules can be used to "teach" ImandraX how to reason in a new
domain.

- `[@@rw]` or `[@@rewrite]`: install theorem as a rewrite rule
- `[@@permutative]`: restrict rewrite rule as permutative
- `[@@fc]`: install theorem as a forward chaining rule
- `[@@elim]` or `[@@elimination]`: install theorem as an elimination rule
- `[@@gen]` or `[@@generalization]`: install theorem as a generalization rule

**Calibration — which lemmas to install, and which to `[%use]`.** In expert
proof corpora, most lemmas get *no* rule class: they are instantiated explicitly
with `[%use lemma args]` where needed. Installing everything as `[@@rw]` is a
known antipattern — rules whose guards the simplifier cannot relieve simply never
fire (the annotations become decoration), and badly shaped rules (e.g. bare
associativity/commutativity) destabilize `auto` across the whole file.

Lemma shapes that make **good `[@@rw]` rules**:
- oriented, unconditional (or trivially-guarded) *equations*, rewriting a more
  complex form to a simpler one;
- typically structural/homomorphism facts inductions need silently:
  `len_append`, `prod_list_append`, `mem_append = (mem x y || mem x z)`,
  `all_p_append`, membership characterizations, small evaluation facts.
- A conditional equation whose guard is itself a hard proof obligation will not
  fire under the plain simplifier — keep it un-installed and `[%use]` it.

Lemma shapes that make **good `[@@fc]` rules**:
- implications whose conclusion is an *inequality/sign/membership fact* —
  useless as rewrites but valuable as extra hypotheses for the arithmetic
  procedures: `a_count_nonneg`, `sum_psd`, `len_cons_ge_1`, monotone membership
  propagation. Optionally mark the matching subterm with `[@trigger]`:
  ```iml
  theorem odds_len_1 x =
    x <> [] && List.tl x <> []
    ==> (List.length (odds x) [@trigger]) < List.length x
  [@@by induct ~on_fun:[%id odds] ()] [@@fc]
  ```
- `[@@fc]` fires globally: after adding one, re-check the whole file.

Everything else — arithmetic facts, mod/congruence lemmas, bridge lemmas, crux
identities — is `[%use]`d at the point of need. See
[proof-method.md](./proof-method.md) for the `[%use]`-chain workflow.
Related discipline: once a function has characteristic lemmas, add
`[@@disable f, ...]` on downstream proofs so the waterfall reasons through the
lemmas instead of unfolding the definition (see the tactics reference,
"Additional proof forms and attributes").

# Other Common Tactics

### `intros`

`intros` takes a goal with implications and conjunctions `H |- (A && B) ==> C` and returns the new goal `H, A, B |- C`. This is typically the first tactic in a chain when the goal has premises that need to be introduced as hypotheses.

```iml
lemma foo x y = x > 0 && y > 0 ==> x + y > 0 [@@by intros @> auto]
```

### `simp` / `simplify`

Apply simplification to the goal. There are several variants:

- `simp` - Quick and simple simplifier, always returns 1 goal
- `simplify ()` - Full waterfall simplifier with all active rules
- `[%simp rule1, rule2]` - Simplify using specific rewrite rules
- `[%simp_only rule1, rule2]` - Simplify using ONLY the given rules (strict mode)

```iml
lemma rev_rev x = List.rev (List.rev x) = x [@@by auto] [@@rw]

(* Using specific rules *)
lemma foo x y = List.length (List.rev (x @ y)) = List.length x + List.length y
  [@@by [%simp rev_len, len_append]]
```

### `unroll`

`unroll n` performs bounded model checking via SMT, unrolling recursive definitions up to `n` steps. This is useful for goals that can be discharged by finite exploration.

```iml
(* Unroll with default SMT solver *)
verify (fun x -> x < 10 ==> f x >= 0) [@@by unroll 100]

(* Unroll with specific solver *)
[@@by unroll ~smt:"z3" 50]
```

### Arithmetic Decision Procedures

- `arith` - Decision procedure for linear (real and integer) arithmetic
- `nonlin ()` - SMT solver with non-linear arithmetic enabled

```iml
lemma linear_example x y = x + y >= x [@@by intros @> arith]

lemma quadratic_example x = x * x >= 0 [@@by nonlin ()]
```

### `cases`

`[%cases t1, t2, ..., tk]` performs case analysis on boolean conditions, generating k+1 subgoals: one for each case (with the case as hypothesis) and one for when all cases are false.

```iml
lemma abs_nonneg x = abs x >= 0
  [@@by [%cases x >= 0] @>| [auto; auto]]
```

### `expand`

`[%expand "f"]` unfolds the definition of function `f`. Use `[%expand (f x y)]` to expand a specific application.

```iml
let square x = x * x

lemma square_pos x = x <> 0 ==> square x > 0
  [@@by intros @> [%expand "square"] @> nonlin ()]
```

### `use`

`[%use lemma_name args]` instantiates a previously proven theorem and adds it as a hypothesis to the current goal.

```iml
lemma pow_pos_helper b n = b > 0 ==> pow b n > 0 [@@by auto]

lemma foo x = x > 0 ==> pow x 5 + 1 > 1
  [@@by intros @> [%use pow_pos_helper x 5] @> auto]
```


### Composing Tactics
To compose tactics, use the following operators:

- `@>`: `t @> t'` expects `t` to produce exactly one sub-goal, and uses `t'` to prove this subgoal.
- `<|>`: `t1 <|> t2` is the `or tactical` which applies `t1` and uses its result if
    it succeeds, and otherwise applies `t2` if `t1` had failed
- `@>>|`: `t1 @>>| t2` applies `t1` and then applies `t2` to all `t1`-generated subgoals.
- `@>|`: `t @>| [t1; t2; ...; tn]` expects `t` to produce `n` subgoals, and , and uses `t_i` to solve the `i`-th goal.

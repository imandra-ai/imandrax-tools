---
name: tactic-reference
description: Reference for proof tactics
---

# Tactic combinators
- `@>`: `t @> t'` expects `t` to produce exactly one sub-goal, and uses `t'` to prove this subgoal.
  - Note: if left tactic has multiple subgoals (e.g. `induction` / `lift_ifs` / `[%cases]`), this will error with `TacticEvalErr: @>: expected first tactic to return exactly one goal`.
- `<|>`: `t1 <|> t2` is the `or tactical` which applies `t1` and uses its result if it succeeds, and otherwise applies `t2` if `t1` had failed
  - Note: this can be unreliable around `auto` -- `auto` often succeeds-with-subgoals instead of erroring, so the fallback never runs.
- `@>>|`: `t1 @>>| t2` applies `t1` and then applies `t2` to all `t1`-generated subgoals.
- `@>|`: `t @>| [t1; t2; ...; tn]` expects `t` to produce `n` subgoals, and , and uses `t_i` to solve the `i`-th goal.
  - Note: it depends on goal order
- monadic map `( let+ )` and monadic bind `( let* )` exists as well


# Tactics

1. `quote_term`
- Signature: `'a -> 'a term`
- Doc: `quote_term (1 + 2)` is a quotation that will not evaluate to anything but
the term "1 + 2". It can then be provided as-is to a tactic.

2. `combine_proofs`
- Signature: `proof list -> proof -> proof`
- Doc: Given a proof `p` of `box A1, ..., box An |- box B` and `hyps`, proofs of
`|- box A_i` respectively, return a proof of `|- box B`

3. `skip`
- Signature: `t`
- Doc: `skip` is a no-op from a reasoning perspective (goal will be unchanged).
This can be useful when needing to put a place-holder tactic in, e.g., a
list of tactics for subgoals when using `@>|`, e.g.,
`... @|> [...; skip; ...]`.

See the variant `skip_msg` which is like `skip` but also prints a message.

Note the notation `[%skip "skip message"]` can be used, with `[%skip]`
emitting no message.

4. `skip_msg`
- Signature: `string -> t`
- Doc: `skip_msg "msg"` is like `skip` (a reasoning no-op) but also prints the
string "msg". This can be useful for tactic debugging.

Note the notation `[%skip "skip message"]` can be used, with `[%skip]`
emitting no message.

5. `ground_eval`
- Signature: `?max_cache:int -> unit -> t`

6. `qs`
- Signature: `t`
- Doc: A very fast, incomplete SMT solver for simple QF_UFDT/LIA/LRA goals.

7. `cc`
- Signature: `t`

8. `simp`
- Signature: `t`
- Doc: Quick and simple simplifier. This always returns 1 goal.

9. `simp_all`
- Signature: `t`
- Doc: Quick and simple simplifier on hypotheses and conclusion. This always
returns 1 goal.

10. `ctx_simplify`
- Signature: `t`
- Doc: Contextual simplifcation

11. `expr_simplify`
- Signature: `t`
- Doc: Moral equivalent of Z3's Expr.simplify applied to each hypothesis and
conclusion in the goal.

12. `or_left`
- Signature: `t`
- Doc: Takes `G ?- a || b`, returns `G ?- a`

13. `or_right`
- Signature: `t`
- Doc: Takes `G ?- a || b`, returns `G ?- b`

14. `split_and`
- Signature: `t`
- Doc: Takes `G ?- a && b`, returns `G ?- a` and `G ?- b`

15. `exact`
- Signature: `t`
- Doc: Takes `G, t ?- t, ...`, succeeds with no further goal. Otherwise fails.

16. `trivial`
- Signature: `t`
- Doc: Succeeds if a goal is trivially true due to having `false` in the
hypotheses, `true` in the conclusion, or the exact same term in the
hypotheses and conclusion (subsuming the `exact` tactic).

17. `intros`
- Signature: `t`
- Doc: `intros` takes a goal `H |- (A && B) ==> C` and returns the new goal
`H, A, B |- C`.

18. `unintros`
- Signature: `int list -> t`
- Doc: `unintros [1;3;4]` takes a goal `H0; H1; ...; H7 |- C` and turns it into
`H0; H2; H5; H6; H7 |- (H1 && H3 && H4) ==> C`. In that sense it's the
inverse of `intros`.

19. `swap`
- Signature: `int -> t`
- Doc: `swap i` takes a goal `H0; ...; Hk |- C0; ...; Ck` and `swaps` a literal
(either a hypothesis or a conclusion term) over the sequent line (|-),
negating it in the process.

If `i>=0`, then hypothesis `i` (`Hi`) is negated and made a conclusion. If
`i<0`, then conclusion `abs(i+1)` (C(abs(i+1))) is negated and made a
hypothesis.

20. `drop`
- Signature: `int -> t`
- Doc: `drop i` drops a hypothesis or conclusion, using the same literal
addressing scheme as `swap`.

21. `expand`
- Signature: `?index:int -> ?all:bool -> string -> t`
- Doc: `expand "foo"` expands the first occurrence of `foo`.
`expand ~index:3 "foo"` expands the index 3 (i.e., fourth) occurrence of
`foo`. `expand ~all:true "foo"` expands all current instances of `foo`.
Instances introduced through this expansion process will not be
recursively expanded.

Note the notation `[%expand "foo"]` can also be used, with either function
names or terms which will be automatically quoted (see `expand_term`).

22. `expand_term`
- Signature: `?index:int -> ?all:bool -> 'a term -> t`
- Doc: `expand_term (quote_term (foo x))` expands the first occurrence of `foo
x`. `expand_term ~index:3 (quote_term (foo x))` expands the index 3 (i.e.,
fourth) occurrence of `foo x`.
`expand_term ~all:true (quote_term (foo x))` expands all current instances
of `foo`. Instances introduced through this expansion process will not be
recursively expanded.

Note the notation `[%expand (foo x)]` handles term quoting automatically.

23. `replace`
- Signature: `'a term -> t`
- Doc: `replace (quote_term x)` uses an equality hypothesis `x=t` to replace `x`
by `t`.

24. `normalize`
- Signature: `?rules:identifier list -> ?strict:bool -> ?basis:identifier list -> ?inline:bool -> 'a term -> t`
- Doc: `normalize (quote_term t)` normalizes a given term under hypotheses of the
current goal, replacing the term with its normalized version if it appears
in the goal, and adding the hypotheses `t = normalized_t` if the target
term does not appear already in the goal.

Normalization applies all active rewrite rules, forward chaining rules,
expands all enabled non-recursive function definitions and includes the
speculative expansion and simplification of recursive functions in order
to take advantage of inductive hypotheses, in the same manner as the
waterfall simplifier.

The `rules` and `strict` parameters behave the same as with the `simplify`
tactic, restricting which rewrite rules and function definitions may be
applied and/or expanded. The `basis` parameter acts as a restriction on
which function definitions can be expanded (basis members are not
expanded, unless they are explicitly given via `rules` with `strict:true`.

Note the notation `[%normalize t]` which normalizes term `t` with default
arguments given to `normalize`.

25. `generalize`
- Signature: `'a term -> string -> t`
- Doc: `generalize (quote_term t) gen_name` generalizes term `t` by replacing it
with a fresh Skolem of the appropriate type throughout the goal. There are
some restrictions on the name `gen_name` used for the new general term: no
terms can already appear in the goal with the same name, and the term
should follow the basic syntactic rules of IML/OCaml identifiers (begin
with a lowercase alphabetical character or `_`, etc.).

26. `cases`
- Signature: `bool term list -> t`
- Doc: `cases [(quote_term t1);...;(quote_term tk)]` case-splits on the given
cases, returning k+1 subgoals consisting of one for each case (with the
case added as a hypothesis), and one additional subgoal for the 'negative'
case in which all t1,...,tk are false. The terms must be boolean-valued.

Note the notation `[%cases t1, t2, ..., tk]` will handle term quoting
automatically.

27. `subgoal`
- Signature: `bool term -> t`
- Doc: `subgoal (quote_term t)` assumes a term and adds the correctness of this
assumption as a subgoal. This effectively applies the `cut` rule of
sequent calculus to produce two subgoals: one in which the term has been
assumed, and another in which the term must be proved under the current
goal context. The term must be boolean-valued.

Note the notation `[%subgoal t]` will handle term quoting automatically.

28. `lift_ifs`
- Signature: `t`
- Doc: Lift if-then-else expressions to the top-level and split accordingly into
subgoals. This does a limited amount of feasibility checking to eliminate
obviously true subgoals (infeasible `if` branches under the current
context) while lifting.

29. `flatten`
- Signature: `t`
- Doc: Disjunctive flattening.

30. `smolsmt`
- Signature: `t`
- Doc: A small and simple SMT solver for QF_UF goals.

31. `unroll`
- Signature: `?smt:string -> ?query_timeout:int -> ?no_asm_query_timeout:int -> int -> t`
- Doc: `unroll ~smt:"z3" 42` does 42 rounds of unrolling with SMT solver named
"z3". Pass `?smt:None` to use the best available SMT solver.

32. `arith`
- Signature: `t`
- Doc: A decision procedure for linear (real and integer) arithmetic.

33. `nonlin`
- Signature: `?smt:string -> unit -> t`
- Doc: An SMT solver with non-linear arithmetic enabled. Pass `None` to use the
best available SMT solver.

34. `auto`
- Signature: `t`
- Doc: Auto tactic, Imandra's flagship automated inductive waterfall proof
procedure, taking into account all known active (rw,fc,elim,gen) rules,
incorporating conditional rewriting with back-chaining, speculative
expansion of recursive functions and symbolic execution, forward-chaining
for simplification contexts, subgoaling, congruence closure, tautology
detection, destructor elimination, generalization and automated induction.

35. `induction`
- Signature: `?id:identifier -> ?vars:string list -> unit -> t`
- Doc: Induction tactic: Synthesize an induction scheme for the goal and apply
it. This does not invoke the induction waterfall (as in `auto`).

36. `simplify`
- Signature: `?smt:string -> ?backchain_limit:int -> ?rules:identifier list -> ?strict:bool -> ?basis:identifier list -> unit -> t`
- Doc: Full simplifier (the waterfall simplifier). Pass `?smt:None` to use the
best available SMT solver. The parameter `rules` is an optional list of
IDs of rewrite rules. If given, only these rewrite rules will be used
during simplification. The parameter `~strict` when `true` and given in
conjunction with `rules` further restricts the simplifier to only use
definitional rules (function definitions, both recursive and
non-recursive) and rewrite rules which are explicitly present in the
`rules` list. The parameter `~backchain_limit` controls the recursive
depth of back-chaining on hypotheses that is allowed during the relieving
of rewrite rule guards (hypotheses).

Note the syntax `[%simplify foo, bar, baz]` which expands to `simplify
~rules:[[%id foo]; [%id bar]; [%id baz]] ()`.

Note that `[%simp ...]` is an alias for `[%simplify ...]`.

Note the syntax `[%simplify_only ...]` and its alias `[%simp_only ...]`
which are like `[%simp ...]` with `~strict:true`.

37. `induct`
- Signature: `?smt:string -> ?on_fun:identifier -> ?on_vars:string list -> ?otf:bool -> ?max_induct:int -> ?do_not:string list -> unit -> t`
- Doc: Induction using the waterfall. Use like this:
`induct ~smt:"z3" ~on_vars:["x"; "y"] ()` for structural induction, or
`induct ~on_fun:[%id f] ()` for function induction with function `f`

38. `use`
- Signature: `bool term -> t`
- Doc: Use an existing theorem to add an additional hypothesis to the current
goal. For example `use (quote_term (add_is_commutative x y))` takes a goal
`A ?- B` and returns a goal `(x+y)=(y+x), A ?- B`.

Note the notation `[%use foo x y z]` handles term quoting automatically.

39. `enumerate`
- Signature: `string list -> t`
- Doc: A tactic that enumerates all values of finite-type variables.

40. `par_solve`
- Signature: `(t term * goal) list -> proof list res`
- Doc: Primitive to solve subgoals in parallel



# Additional proof forms and attributes

Forms used pervasively in expert proofs that are not in the tactic list above.

## `[%have name := fact by (tactic)]` — structured forward step

Proves `fact` on the spot with the given sub-tactic and adds it to the goal as a
named hypothesis. Lets a long proof read like a paper proof: a sequence of named,
locally-discharged facts.

```iml
[@@by intros
   @> [%have p_div_m := m mod p = 0 by ([%use prime_div_sq p m] @> auto)]
   @> [%have p_ge_2  := p >= 2      by auto]
   @> nonlin ()]
```

The `by (...)` sub-proof can use any tactic expression (`[%use]`, `[%simp_only]`,
`arith`, `nonlin ()`, ...). Also useful for micro-rewrites the simplifier won't
perform: `[%have rew := 2*m*m = 2*(m*m) by (nonlin ())]`.

## `esimp` — evaluation-oriented simplification

Simplifies by evaluation/computation; useful to expose `let`-bound tuple
components (e.g. a `let (u, v) = bezout_sub a b in ...` in the goal) and to
compute ground subterms mid-proof. Commonly chained: `esimp @> simplify ()`.

## `[%t ...]` — quote_term shorthand

`generalize [%t (a / b)] "q"` ≡ `generalize (quote_term (a / b)) "q"`.

## Tactics are first-class values

Bind and reuse them:
```iml
let crush = Tactic.unroll 10   (* file-level house closer *)

theorem descent_loop_correct p m q = ...
[@@by
  let close_sub = intros @> [%subgoal ...] @>| [ ...; crush ] in
  induction ~id:[%id descent_ind] () @>>| close_sub]
```

Plain `let` inside a `[@@by]` block can also name terms for instantiations:
`let (u, v) = bezout_sub a b in [%use mod_mul_preserves_zero u a k] @> ...`.

## `[@@disable f, g, ...]` — freeze definitions for one proof

Attribute on a lemma/theorem: the listed function definitions are not unfolded
during that proof, so the prover must reason through previously proven lemmas
about them. Essential once a function has its characteristic-lemma API, and to
stop `auto` diverging into a case-explosion or nested-recursion unfold:

```iml
theorem descent_loop_correct p m q = ...
[@@by induction ~id:[%id descent_ind] () @>>| close_sub]
[@@disable c_mod, descent_q, centered, sum2]
```

Works on prelude functions too (e.g. `[@@disable List.mem]`). Counterpart:
`[%simp_only f, g]` *unfolds only* the listed definitions at a chosen point.

## `[@trigger]` — forward-chaining trigger annotation

Marks the subterm of a `[@@fc]` rule that should pattern-match hypotheses to
fire the rule:

```iml
theorem odds_len_1 x =
  x <> [] && List.tl x <> []
  ==> (List.length (odds x) [@trigger]) < List.length x
[@@by induct ~on_fun:[%id odds] ()] [@@fc]
```

## `[@@by ...]` on a `let rec` — proving the termination obligation

A `[@@by]` attached to a recursive *definition* is the tactic for its
termination proof obligation (usually next to `[@@measure ...]`). Nontrivial
termination goals can `[%use]` previously proven lemmas:

```iml
let rec prime_factors (n:int) : int list = ...
[@@measure Ordinal.of_int (max 0 n)]
[@@by [%use sd_le_n n] @> [%use div_smaller n (small_divisor_from 2 n)]
   @> simplify () @> arith]
```

See [[termination-proving.md]] for more details on measures, `[@@adm]`, and related idioms.

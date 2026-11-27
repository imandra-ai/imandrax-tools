---
name: arithmetic-playbook
description: Making arithmetic goals go through in ImandraX - the generalize-then-nonlin recipe, isolating nonlinear steps into micro-lemmas, the div_mod normal form for divisibility, a reusable mod-theorems vocabulary, int/real bridges, and inequality-combination lemmas. Read this whenever a goal involves multiplication of variables, division, mod, or mixed int/real reasoning.
---

# Arithmetic Playbook

Linear arithmetic (`arith`, and inside `auto`) is decided automatically. Anything
nonlinear — products of variables, division, `mod`, powers — needs to be *staged*
for the solver. Experts never ask the prover to do nonlinear reasoning about
defined functions directly; they reduce every such step to one of the recipes
below.

## 1. The core recipe: generalize opaque terms, then `nonlin ()`

`nonlin ()` is an SMT call with nonlinear arithmetic. It works on *polynomials
over atoms*, so first replace every non-arithmetic subterm (recursive calls,
factorials, gcds, quotients, powers) with a fresh variable:

```iml
[@@by intros
   @> generalize (quote_term (Combinations.fact ((p - 1) / 2))) "f"
   @> generalize (quote_term (upper_prod p ((p - 1) / 2))) "u"
   @> nonlin ()]
```

Shorthand: `generalize [%t (a / b)] "q"`. After generalization the goal is pure
polynomial algebra in `f`, `u`, ... and `nonlin ()` can decide it. If you skip
this step, `nonlin`/`auto` will try to reason about the function bodies and
usually wander.

## 2. Isolate nonlinear steps into micro-lemmas over fresh variables

The alternative (often cleaner) staging: state the needed algebraic fact as a
standalone lemma over plain `int`/`real` variables, prove it by `nonlin ()`, and
inject it where needed with `[%use]`:

```iml
lemma cs_algebra (a b c : real) =
  Real.(a >= 0.0) && Real.(c > 0.0) && Real.(a - (b * b) / c >= 0.0)
  ==> Real.(b * b <= a * c)
[@@by nonlin ()]

(* later, at the call site: *)
[@@by ... @> [%use cs_algebra (norm_sq u) (dot u v) (norm_sq v)] @> auto]
```

Name the crux identity of each induction step this way (`algebra_key`,
`..._telescopes`, `..._step`); the expert corpus contains 40+ such micro-lemmas.
Two-step closers like `[@@by simplify () @> nonlin ()]` are the norm when the
definition must be unfolded first (e.g. the Euler four-square identity is one
such line).

## 3. Inequalities don't combine themselves

The prover will not add `A >= B` and `C >= D` into `A + C >= B + D`, chain
strict/non-strict bounds, or "unsquare" for you. Write the combination lemma once
and instantiate it explicitly:

```iml
lemma ge_add_mono (a b c d : real) =
  Real.(a >= b) && Real.(c >= d) ==> Real.(a + c >= b + d) [@@by auto]

lemma le_of_sq_le_nonneg_rhs (a b : real) =    (* "unsquare" *)
  Real.(b >= 0.0) && Real.(a * a <= b * b) ==> Real.(a <= b)
[@@by [%cases Real.(a <= 0.0)] @>| [...; ...]]
```

Same for substitution under an operator — congruence lemmas make one rewrite
step `[%use]`-able:

```iml
lemma mod_congr_arg p x y = x = y ==> x mod p = y mod p [@@by auto]
```

## 4. Divisibility normal form: `div_mod` + explicit quotient

To prove `d | E` (i.e. `E mod d = 0`), don't search for a divisibility rule set.
Convert everything to equations and exhibit the quotient yourself:

1. For each *known* divisibility, `[%use div_mod x d]` to get
   `x = d * (x / d) + x mod d` (with `x mod d = 0` in context).
2. For the *goal*, instantiate `mod_mul_left_zero <quotient-expression> d` where
   the quotient expression is E/d written out explicitly (it can span lines).
3. Close with `nonlin ()` (generalizing `x / d` terms to fresh variables first
   if needed, per §1).

This pattern discharges essentially every "d divides <big product>" obligation in
the expert number-theory proofs.

## 5. The mod vocabulary

Build (or import) a small congruence API once and route all mod reasoning through
it — these ~12 facts are the working vocabulary of every divisibility/parity
proof (see `extended-prelude` for a ready-made library):

```
div_mod          : d <> 0 ==> a = d * (a / d) + a mod d
mod_range        : d > 0  ==> 0 <= a mod d && a mod d < d
mod_add          : k <> 0 ==> (a + b) mod k = ((a mod k) + (b mod k)) mod k
mod_mul          : k <> 0 ==> (a * b) mod k = ((a mod k) * (b mod k)) mod k
mod_mod / mod_idem, mod_small, zero_mod, mod_self_zero,
mod_mul_left_zero / mod_mul_right_zero  : (k * a) mod k = 0 forms
mod_zero_trans, mod_one_self
```

Implementation note: builtin `mod` is hard to induct on directly. The expert
library proves everything about a hand-rolled Euclidean `emod` (visible
recursion), then transfers each fact to `mod` mechanically via a bridge lemma
(`mod_eq_emod`) instantiated once per distinct mod-subterm in the goal. If you
need a mod fact that isn't in the API, that's the pattern to extend it with.

Parity/sign idioms: work in nonneg residues (state "≡ −1 (mod p)" as
`x mod p = p - 1`); track `(-1)^k` as the arithmetic expression
`(1 - 2 * (k mod 2))` so it survives induction without case explosion; get the
two-case split of a parity argument from `mod_range` + `[%expand]` of the
relevant definition at `2`.

## 6. Division and roots: keep them out (recap)

Covered fully in `statement-engineering.md`: prove the multiplicative form and
derive the `/` form via `mult_div_equiv` (`b <> 0 && a*b = c ==> a = c/b`,
`[@@by simplify () @> nonlin ()]`); encode sqrt/n-th-root relationally with
witness parameters, and prove the small "sqrt algebra" lemma family
(monotonicity, product) from squares.

## 7. int/real boundary management

Cross the `Real.of_int` boundary through explicit bridge lemmas, not in the
middle of a big goal:

```iml
lemma proj_le_z n = (Real.of_int n <=. 0.0) = (n <= 0) [@@by auto]

lemma int_ineq_to_real a b =        (* lift an integer bound to a probability *)
  2 * a < b && b > 0 && a >= 0
  ==> Real.(of_int a / of_int b <. 0.5)
[@@by nonlin ()]
```

Prove the substantive fact on the `int` side (often by evaluation or induction),
then lift once at the end.

## 8. Choosing the closer

- `arith` — linear int/real goals; also the closer for one-line ordering lemmas.
- `nonlin ()` — polynomial goals after §1/§2 staging.
- `auto` — anything needing rewriting/induction glue around the arithmetic.
- `unroll N` — when the goal is decided by bounded computation (concrete or
  small-finite goals; also useful as a finisher when `auto` over-inducts).
- Plain evaluation: goals that are just big computations
  (`2 * falling_fact 365 23 < int_pow 365 23`) close by `[@@by auto]` /
  `ground_eval` — arrange statements to exploit this (see
  `statement-engineering.md`).

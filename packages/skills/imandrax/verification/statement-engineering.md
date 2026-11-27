---
name: statement-engineering
description: How to state theorems and choose encodings so they are provable in ImandraX. Witness functions instead of existentials, division-free forms, executable predicates, totalization, and more. Read this BEFORE formalizing any non-trivial property — a hard proof is usually won or lost at the statement.
---

# Statement Engineering

Most failed proof attempts are not tactic failures — they are statement failures.
Expert IML proofs (e.g. the Imandra "100 theorems" corpus) spend their effort
*before* proving: choosing definitions and phrasings that make the theorem fall
inside what ImandraX's induction and decision procedures can handle. This document
is the playbook. Notably, in ~17k lines of expert proofs there is **not a single
existential quantifier, division-form main statement, or partial application of
an unbounded math concept** — everything is re-encoded first.

## 1. Existentials become witness functions

Never state "there exists an x such that P x". Define a total function that
*computes* the witness, then prove P of it:

```iml
(* NOT: exists p. is_prime p && p > n *)
let euclid (n:int) : int = small_divisor_from 2 (prod_upto n + 1)

theorem infinitude_of_primes n =
  let bigger_prime = euclid (abs n) in
  is_prime bigger_prime && bigger_prime > n
[@@by [%use euclid_is_prime (abs n)] @> [%use euclid_gt_n (abs n)] @> auto]
```

This works for any existence claim: Bézout coefficients (`bezout_sub : int -> int
-> int * int` mirroring gcd's recursion), a prime factorization (`prime_factors n`),
a modular inverse (`inv_mod a p`). State the theorem as `let w = witness x in P w`.

When the witness is found by bounded *search*, pair the search function with
exactly two lemmas:

- **soundness**: `result >= 0 ==> in_range result && condition result`
- **completeness**: `condition y0 && in_range y0 ==> result >= 0`
  (any concrete satisfying value forces the search to succeed)

```iml
let rec seed_y p x ymax =
  if ymax < 0 then -1
  else if good p x ymax then ymax
  else seed_y p x (ymax - 1)
[@@measure Ordinal.of_int (max 0 (ymax + 1))]
```

A "least counterexample" function (search `[i..k]` for the first failure) is the
same pattern and gives you minimal-counterexample arguments without quantifiers.

## 2. Eliminate division, roots, and limits from the statement

- **Multiplicative form first, division form as corollary.** Prove
  `2 * arithmetic_sum a d n = (n + 1) * (2*a + n*d)`, not the `/2` form. Then
  derive the division form with one bridge lemma:

  ```iml
  lemma mult_div_equiv (a b c : real) =
    b <> 0.0 && Real.(a * b) = c ==> a = Real.(c / b)
  [@@by simplify () @> nonlin ()]
  ```

- **Roots are relations, not functions.** There is no workable `sqrt`. Encode it
  as a constraint on an extra theorem parameter:

  ```iml
  let is_sqrt (x y : real) = Real.(x >= 0.0) && Real.(y >= 0.0) && Real.(x * x) = y

  theorem cauchy_schwarz u v eu ev =
    same_length u v && is_sqrt eu (norm_sq u) && is_sqrt ev (norm_sq v)
    ==> Real.(abs_real (dot u v) <= eu * ev)
  ```

  Same move for n-th roots (AM-GM is stated with a hypothesis
  `real_pow g (len xs) = prod xs` instead of `g = root n (prod xs)`), and for
  trig (angle equality in the isosceles theorem is a polynomial cos²-equality plus
  a sign condition — no `cos`).

- **Limits become explicit bounds.** "The harmonic series diverges" is stated as
  `n >= exp2 (2*m) ==> harmonic_sum n >. Real.of_int m`.

- **Probabilities become integer counting.** The birthday problem is
  `2 * falling_fact 365 23 < int_pow 365 23` (provable by evaluation!), with the
  real-valued `< 0.5` form derived by an int→real bridge lemma afterwards.

## 3. Choose the definition whose recursion IS the induction you need

The recursion structure of your definitions determines which inductions come for
free. Encode concepts as executable recursive functions shaped like the proof:

- Primality = fixpoint of a search function:
  `let is_prime p = p >= 2 && small_divisor_from 2 p = p`
- Divisibility = `let divides d n = d <> 0 && n mod d = 0`
- Binomial coefficients = the Pascal recursion itself (so Pascal's identity is
  the definition, not a lemma to fight for)
- Multiset equality = recursive `permutation` via `remove_one`
- Polynomial division = synthetic division whose recursion matches the factor
  theorem's induction

If a proof needs to sum a different way than your definition recurses, add a
second function plus a decomposition lemma
(`harmonic_sum n = harmonic_sum m +. harmonic_extend m n`) rather than fighting
the original.

## 4. Totalize with harmless defaults

Give partial operations a benign value on bad inputs so theorems hold
*unconditionally* and induction bases are trivial: `inv k = 0.0` for `k <= 0`;
`real_pow g n = 1` for `n <= 0`; gcd returns `0` on negative inputs. Prefer this
over hypotheses that you'll have to relieve in every downstream lemma. Relatedly,
avoid stating lemmas over underspecified destructors: `List.hd`/`List.tl` on a
possibly-empty list take unspecified junk values, which can make a true-looking
statement refutable — prefer pattern-matching forms
(`match p with x :: rest -> ...`).

## 5. Universal properties of collections: recursive `all_*` predicates

Don't quantify over members; write a recursive boolean and its support lemmas:

```iml
let rec all_nonneg (xs : int list) : bool =
  match xs with [] -> true | x :: rest -> x >= 0 && all_nonneg rest

lemma all_nonneg_append xs ys =
  all_nonneg (xs @ ys) = (all_nonneg xs && all_nonneg ys) [@@by auto] [@@rw]

lemma all_nonneg_mem x xs =
  all_nonneg xs && List.mem x xs ==> x >= 0 [@@by auto]
```

The standard kit per predicate: an `_append` homomorphism (usually `[@@rw]`), a
`_mem` extraction lemma, and hd/tl access lemmas as needed.

## 6. Checker functions ("badguy" pattern) for membership and bounded ∀

To prove `P x ==> List.mem x (generated_set n)` — or any bounded universal — write
a recursive boolean **checker** whose recursion produces the right IH, then prove
two mechanical lemmas: (1) the checker equals / is implied by the target
predicate, (2) the checker holds under your hypotheses.

```iml
let rec raim_check (p lo hi : int) : bool =
  if lo > hi then true
  else List.mem (inv_mod lo p) (range 1 (p - 2)) && raim_check p (lo + 1) hi

lemma raim_check_eq p lo hi =
  raim_check p lo hi = all_inv_in p (range lo hi) (range 1 (p - 2)) [@@by auto]
lemma raim_check_true p lo hi = <hyps> ==> raim_check p lo hi [@@by induction () ...]
```

This turns a quantified pairing/closure argument into two easy inductions.

## 7. Counting, cardinality, and bijections

- Cardinality is `List.length` of an explicitly constructed enumeration; "sets"
  are duplicate-free lists (a `dlistp` predicate).
- To prove `|A| = f n`: build a second construction whose length trivially
  satisfies the recurrence, then show mutual inclusion, and close with a
  remove-based pigeonhole lemma (`dlist_sub_len`: distinct + subset ⇒ length ≤).
- Injectivity is never assumed — define the inverse function and prove
  round-trip lemmas; injectivity is then `[%use roundtrip d1] @> [%use roundtrip d2] @> auto`.
- Bijections/countability: state `inj`/`surj` as higher-order *definitions*
  taking the concrete `encode`/`decode` functions as arguments (no set theory).

## 8. Two definitions: one for meaning, one for proof

Keep a transparent "specification" definition (obviously correct) and a
proof-friendly one (recursion mirrors the target recurrence), connected by a
lemma. E.g. derangements as `filter is_derangement (all_perms n)` *and* as a
direct recursive construction mirroring the subfactorial recurrence; the counting
theorem is proved on the second and transferred. Never contort one definition to
serve both roles.

## 9. Make easy theorems easy: prefer polynomial-shaped statements

If a statement can be phrased as a polynomial identity/implication over a fixed
number of `int`/`real` variables — no recursion, no division — plain `[@@by auto]`
or `nonlin ()` often closes it with **zero lemmas**. The Pythagorean theorem is
two `[@@by auto]` theorems. This is why fixed-dimension records
(`type vec = {x: real; y: real}` for R²) beat list encodings when dimension is
fixed: they keep goals quantifier-free and polynomial.

## 10. Misc encoding idioms

- **Signs/parity arithmetically, not by cases**: track `(-1)^k` as
  `(1 - 2 * (k mod 2))` so one formula survives induction; specialize to the
  if-then-else form afterwards. Avoid negative residues: state "≡ −1 (mod p)" as
  `x mod p = p - 1`.
- **Avoid conjunction-heavy goals**: prove near-identical component facts as
  separate lemmas ("four lemmas to avoid conjunction splitting") instead of one
  4-way conjunction that blows up together.
- **Name every function you'll reason about.** No inline lambdas in anything a
  proof will touch — if a higher-order lemma needs a concrete instance, define a
  named closure (`let cell_fn m x i j = cell m i j x`) and connect it with a
  bridge lemma.

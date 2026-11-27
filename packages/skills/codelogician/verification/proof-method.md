---
name: proof-method
description: The end-to-end method for proving a non-trivial theorem in ImandraX - the session loop, the [%use] forward-instantiation workhorse, lemma-ladder architecture, calibration of effort, and the antipatterns to avoid. Read this when starting any theorem that plain auto does not close.
---

# The Proof Method

How experts actually drive ImandraX, distilled from the Imandra "100 theorems"
corpus and from postmortems of agent proof sessions. The headline: **expert proofs
are `[@@by <tactic script>]` programs assembled from explicitly instantiated
lemmas** — automation closes gaps, it does not find proofs.

## The session loop

1. **Encode** the statement following `statement-engineering.md`. This step
   decides whether the proof is possible.
2. **`eval`-test the definitions.** Before proving anything, run `eval` on
   concrete inputs with expected values in comments:
   ```iml
   eval prime_factors 100    (* expect [2;2;5;5] *)
   eval sum_naturals 100     (* = 5050 *)
   ```
   This catches encoding bugs that would otherwise surface as unprovable (or
   worse, refuted) theorems hours later. Experts do this in most files.
3. **Try the cheap closer on the whole statement**: `[@@by auto]`. Polynomial
   statements over fixed variables often just close. For finite/concrete goals,
   try `[@@by unroll 50]` (bounded checking is a legitimate proof method — the
   Königsberg bridges theorem is proved this way).
4. **If it doesn't close: STOP.** Do not permute tactics (see antipatterns).
   Understand *why*: dump/inspect the induction scheme or the stuck subgoals,
   and write the mathematical plan **in prose comments** — which case needs which
   fact, what the IH gives you, what algebraic identity bridges them.
5. **Build the lemma ladder bottom-up** (next section), re-checking the file
   after each lemma. Keep proofs small; comment out sketches not yet proved.
6. **Assemble.** The final theorem should be a *thin* layer: `intros` + 2–7
   `[%use]` of your top-level lemmas + one closer. If the final proof is a
   25-line nested tactic script, extract the hard cases into standalone lemmas.

## The workhorse: `[%use]` forward-instantiation chains

The single most common expert proof shape (60%+ of all non-trivial proofs):

```iml
theorem p_even_from_eq p q = (p*p = 2*(q*q)) ==> p mod 2 = 0
[@@by [%use mod_mul_right_zero (q*q) 2]
   @> [%use square_even_iff_even p]
   @> auto]
```

You compute the instantiations by hand: each `[%use lemma arg1 arg2]` adds that
instance as a hypothesis, and the closer (`auto` / `arith` / `nonlin ()` /
`unroll N`) only has to do propositional and (non)linear glue. Notes:

- Instantiation arguments can be arbitrarily large terms. After an induction,
  goals are phrased over `xs`, so instantiate at the destructors:
  `[%use step_lemma (List.hd xs) (list_prod_mod (List.tl xs) n)]`.
- Put a comment on each `[%use]` line stating the mathematical fact it
  contributes. This is how expert scripts stay debuggable.
- You can name terms with plain OCaml `let` inside the `[@@by]` block:
  `let (u, v) = bezout_sub a b in [%use ...]`.
- Do **not** expect installed `[@@rw]` rules to replace this. Conditional rules
  with non-trivial guards frequently do not fire inside `auto` (it may divert to
  induction first). Explicit `[%use]` is the default; rules are a bonus
  (see `verification-guide.md` for which lemma shapes make good rules).

For long forward arguments, the structured form reads like a paper proof:

```iml
[@@by intros
   @> [%have p_div_m := m mod p = 0 by ([%use prime_div_sq p m] @> auto)]
   @> [%have p_ge_2  := p >= 2      by auto]
   @> ... @> nonlin ()]
```

`[%have name := fact by (tactic)]` proves `fact` on the spot with the given
sub-tactic and adds it as a named hypothesis.

## Lemma-ladder architecture

- **Characteristic lemmas, then opacity.** For each recursive function, prove a
  small API (bounds, idempotence, homomorphism over `@`, membership
  characterization, ...). Once the API exists, add `[@@disable f, g, ...]` to
  downstream lemmas so the prover reasons through the API instead of unrolling
  definitions into a mess. Symmetrically, open a proof with
  `[%simp_only f, g]` when you *do* want exactly those definitions unfolded.
- **One fact per lemma, ≤ ~4 hypotheses.** Many expert lemmas are one-liners
  that exist purely to name an arithmetic step (`[@@by arith]`) or perform a
  substitution `auto` won't do in place
  (`b = c && (a*b) mod p = 1 ==> (a*c) mod p = 1`).
- **Extract the crux identity.** For each induction, one lemma states the exact
  algebraic identity that makes the step work (name it `..._key` or
  `..._step`), proved by `nonlin ()`/`auto` over fresh variables, and the step
  becomes `[%use key ...] @> auto`. See `arithmetic-playbook.md` and
  `induction-control.md`.
- **General theorem first, corollaries by instantiation.** Prove the parametric
  version, then each special case is `[%use general ...]` + one algebra lemma.
- **Reuse across files** with `[@@@import "mod.iml"]` and fully-qualified
  `[%use Mod.Theorems.div_mod n 10]`. Build layered theory files (mod, gcd,
  sets) rather than re-proving support lemmas per theorem.

## Calibration: how much work to expect

From the expert corpus (per main theorem):

| difficulty | lemmas | examples |
|---|---|---|
| polynomial identity over fixed vars | 0 | Pythagoras: two `[@@by auto]` theorems |
| small inductive result | 4–15 | series sums, div-by-3 rule |
| medium | 20–50 | FTA, Euler's theorem, ballot problem |
| hard | 70–120 | Wilson, Erdős–Szekeres, derangements |

If a "medium" target is resisting after 5 lemmas, the ladder is not too long —
it is too short. Fine granularity is normal, and so is visible process: experts
leave duplicate restatements, commented-out dead ends, and even a transcribed
stuck subgoal promoted into its own lemma (proved standalone with `[@@disable]`,
then `[%use]`d back). These are legitimate moves, not failures.

## Literate style

- Header comment: what theorem, source, author.
- Section banners (`(* ===== PHASE B: sqrt(-1) mod p ===== *)`) with a prose
  proof sketch at the top of each phase.
- Numbered step comments inside long `[@@by]` scripts, one per `[%use]`.

## Reading prover feedback

- **Refuted (with counterexample)** means the statement *as written* is false —
  tactics cannot prove a false statement, so read the counterexample before
  touching the proof. Usual causes: a missing guard, an encoding bug (`eval`
  your definitions!), or an underspecified value — e.g. `List.hd`/`List.tl`
  applied to a possibly-empty list can refute a true-looking lemma through the
  junk value of `List.tl []`. Prefer pattern-matching statements
  (`match p with x :: rest -> ...`) over `hd`/`tl` phrasings.
- **`Refuted_generalized_subgoal`** is not a refutation of your goal: `auto`
  generalized a subgoal during the waterfall and the *generalization* is false.
  Take more control — supply the induction scheme, `[@@disable]` the offending
  definitions, or add the missing lemma.
- **`auto` can "fail" silently** — a no-op or partial subgoals instead of an
  error (so `t <|> auto` fallbacks may never trigger). If a script isn't
  progressing, inspect the goal state rather than rearranging tactics.

## Antipatterns

1. **Tactic permutation.** Shuffling `simp`/`expand`/`auto` variants hoping one
   sticks. Because `auto` fails silently, you get no signal that the *approach*
   is wrong. The fix is step 4 of the loop: stop, inspect the scheme, plan
   per-case in prose.
2. **Spraying `[@@rw]` on everything.** Rules that can't fire are decoration;
   worse, bad rules (e.g. unconditioned associativity) destabilize `auto`
   globally. Install few, well-shaped rules; `[%use]` the rest.

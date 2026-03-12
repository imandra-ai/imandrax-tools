---
name: verification-with-verify-and-instance
description: Verify (prove a goal or find a counter-example) using `verify` and `instance` commands
---

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

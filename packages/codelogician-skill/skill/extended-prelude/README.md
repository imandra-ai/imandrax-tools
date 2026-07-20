---
name: extended-prelude
description: Additional prelude functions, general purpose utilities. Including Int_conv, LChar_utils, etc. Mostly as a reference for implementing your own. Can also be copied into your project and then imported.
---

# Extended prelude

This directory contains additional prelude functions, general purpose utilities. Including `Int_conv`, `LChar_utils`, etc.

They are not part of the standard IML prelude which is loaded by default.

The recommended way to use these utility modules / functions is to copy paste the whole directory into your project and then import the modules you need.

## Contents

- `int_conv.iml` — integer/string conversion utilities.
- `lchar_utils.iml` — logic-mode character utilities.
- `mod_theorems.iml` — **a reusable congruence/divisibility lemma library for `mod`**
  (`mod_range`, `mod_add`, `mod_mul`, `mod_mod`, `div_mod`, ...). Import it and
  `open Mod.Theorems` whenever a proof involves `mod`, divisibility, or parity —
  these ~16 facts are the working vocabulary of such proofs and are hard to
  re-derive from scratch (builtin `mod` is not induction-friendly; the library
  proves everything on a hand-rolled Euclidean `emod` and transfers the facts).
  See `verification/arithmetic-playbook.md` for how to use them.
  Adapted from the imandra-100-theorems project (Grant Passmore, Imandra).

Example usage:

```iml
[@@@import Mod, "mod_theorems.iml"]
open Mod.Theorems

theorem sum_digits_mod3 n = ...
[@@by [%use div_mod n 10] @> [%use mod_add (10 * (n / 10)) (n mod 10) 3] @> ... @> auto]
```

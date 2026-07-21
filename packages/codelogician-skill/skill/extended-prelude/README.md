---
name: extended-prelude
description: Additional prelude-like general purpose modules for reference, including `Int_conv`, `LChar_utils`, etc. Mostly as a reference for implementing your own. Can also be copied into your project and then imported.
---

# Extended prelude

This directory contains additional prelude-like general purpose modules, including:
- `Int_conv` - integer/string conversion utilities.
- `LChar_utils` - logic-mode character utilities.
- `Mod_theorems` - congruence/divisibility lemma library for `mod` operations.

NOTE: They are not part of the standard IML prelude which is loaded by default.

## Contents

- `int_conv.iml`: integer/string conversion utilities.
- `lchar_utils.iml`: logic-mode character utilities.
- `mod_theorems.iml`: a congruence/divisibility lemmas library for `mod`
- ...
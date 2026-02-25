---
name: imandrax
description: Help with IML (Imandra Modeling Language) - formal verification, theorem proving, counter-example generation, region decomposition / test-case generation, etc. Use when working with .iml files or Imandra / ImandraX related projects.
---

# IML (Imandra Modeling Language) Expert

## Terminology
- IML (Imandra Modeling Language), a formalized Higher-Order subset of OCaml extended with theorem proving tactics and verification annotations. 
- ImandraX is the reasoning engine that powers IML.
- Region decomposition is a powerful feature of ImandraX for analyzing the state-space of functions. It automatically partitions a function’s input domain into disjoint regions, each characterized by a set of constraints and a corresponding simplified invariant behavior of the function within that region. Test cases can be further generated from these regions.

## Typical workflow when working with IML
- Write IML code, corresponding to your specification or program to be verified / tested. Admit IML code with ImandraX and correct type errors if any.
- Based on requirements, resort to corresponding commands or annotations to invoke ImandraX:
  - Option 1. Verification (prove a goal or find a counter-example)
    - Use `verify <func>` / `instance <func>`, which are dual to each other, with the former attempts to prove the goal and return a counter-example if it fails, and the latter attempts to find a counter-example and return it if it succeeds.
    - For more general theorem proving, especially with tactics, use `theorem <name> <vars> = <body>`, `lemma <name> <vars> = <body>`, and `axiom <name> <vars> = <body>`.
  - Option 2. Region decomposition / Enumerative analysis / Test-case generation
    - Use `[@@decomp top <decomp-args> ()]` attached to function definitions to invoke region decomposition.
    - Generate test cases (Python or TypeScript) from regions with subcommands in `codelogician` CLI.


## Interact with ImandraX

To type-check, invoke verification, or invoke region decomposition, you pass your IML code to ImandraX.

- `codelogician-tools` CLI: the preferred way for file-system-based agents to interact with ImandraX through LLM-friendly interface.
  - The most important and frequently used subcommand is `check [IML-FILE]`, which tries to type-check and admit all structures in the file.
- Other interfaces: `imandrax-api` Python pacakge, `imandrax-cli` CLI

## For more information

```tree {name: skill-dir-structure}
imandrax/
├── advanced/ # Advanced topics and tips
│   ├── avoid-higher-order-functions-in-proofs.md # Notes on potential issues with higher-order functions like List.map in IML proofs
│   ├── full-verification-guide.md # Full verification guide for ImandraX, including tactic usage.
│   ├── opaque-functions.md # Notes on using opaque functions in IML to mock functionality
│   ├── proof-tips.md # Practical tips for writing proofs in IML.
│   ├── region-decomp-advanced-features.md # Advanced features in Region Decomposition, including composition operators and refiners
│   └── termination-proving.md # Termination proving using [@@measure ...] and the Ordinal module
├── examples/ # Worked examples
│   └── basic-iml-syntax.md
├── reference/ # Language and API reference
│   ├── api/ # Module-level API docs
│   │   ├── Int.md
│   │   ├── LChar.md
│   │   ├── LString.md
│   │   ├── List.md
│   │   ├── Map.md
│   │   ├── Multiset.md
│   │   ├── Option.md
│   │   ├── Real.md
│   │   ├── Result.md
│   │   ├── Set.md
│   │   ├── String.md
│   │   └── top-level.md
│   ├── all-module-signatures.md # Signatures of all modules in IML prelude
│   ├── ordinal.md # Reference for ordinals used in termination proofs
│   └── tactics.md # Complete reference for all proof tactics
├── codelogician-cli.md # Guide for using the `codelogician-tools` CLI to interact with ImandraX and access additional features.
├── iml-syntax.md # IML syntax guide, highlighting its difference with OCaml, some examples, and tips and pitfalls.
├── region-decomp-intro.md # Intro to region decomposition, including concept explanations, basic usage, and common errors.
└── verification-with-verify-and-instance.md # Verify (prove a goal or find a counter-example) using `verify` and `instance` commands
```

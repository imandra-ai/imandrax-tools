---
name: codelogician
description: Use IML (Imandra Modeling Language) / ImandraX to reason about software engineering (programs and specifications) - formal verification, theorem proving, counter-example generation, region decomposition / test-case generation, etc. Read me whenever you see mentioning of IML / Imandra / CodeLogician.
---

# CodeLogician

## Concepts
- CodeLogician: agent that utilize IML/ImandraX to reason about programs and their properties (specifications).
- IML (Imandra Modeling Language), a formalized Higher-Order subset of OCaml extended with theorem proving tactics and verification annotations. ImandraX is the reasoning engine that powers IML.
- Verification: a process of proving a goal (some properties) or finding a counter-example if the goal is not satisfied. The process can be fully automated or interactive (guided with tactics).
- Region decomposition is a powerful feature of ImandraX for analyzing the state-space of functions. It automatically partitions a function’s input domain into disjoint regions, each characterized by a set of constraints and a corresponding simplified invariant behavior of the function within that region. Test cases can be further generated from these regions.
- `codelogician` / `codelogician-lite` CLI: the preferred way for coding agents to interact with ImandraX through a LLM-friendly interface.

## Typical workflow when working with IML
- Write IML code, corresponding to your specification or program to be verified / tested. Admit IML code with ImandraX and correct type errors if any.
- Based on requirements, resort to corresponding commands or annotations to invoke ImandraX:
  - Option 1. Verification (prove a goal or find a counter-example)
    - Use `verify <func>` / `instance <func>`, which are dual to each other, with the former attempts to prove the goal and return a counter-example if it fails, and the latter attempts to find a counter-example and return it if it succeeds.
    - For more general theorem proving, especially with tactics, use `theorem <name> <vars> = <body>`, `lemma <name> <vars> = <body>`, and `axiom <name> <vars> = <body>`.
  - Option 2. Region decomposition / Enumerative analysis / Test-case generation
    - Use `[@@decomp top <decomp-args> ()]` attached to function definitions to invoke region decomposition.
    - Generate test cases (Python or TypeScript) from regions with subcommands in `codelogician` CLI.

## Getting started

- [IML Language Guide](./iml-syntax.md): how to write IML (essential)
- [codelogician-cli.md](./codelogician-cli.md): the most common way for a coding agent to interact with ImandraX (essential)
- [module-import-syntax.md](./import-syntax.md): modular development with IML


## Full list of references in skill directory

Along with `SKILL.md` (this file), we have the following materials:

```tree
./
├── advanced/ # Advanced topics and tips
│   ├── avoid-higher-order-functions-in-proofs.md # Notes on potential issues with higher-order functions like List.map in IML proofs
│   ├── opaque-functions.md # Notes on using opaque functions in IML to mock functionality
│   ├── region-decomp-advanced-features.md # Advanced features in Region Decomposition, including composition operators and refiners
│   └── unit-testing.md # Writing unit tests (in the context of regular software development) in IML using `verify` with `ground_eval` and `expand`. Can be useful in incrementally building up IML projects.
├── error-fix-data/ # Data for common error and fix
│   └── README.md # IML error and fixes database. Provides `error_corpus.json`, a collection of common IML errors and their fixes. Search it using jq or grep to find relevant errors and their fixes.
├── extended-prelude/
│   └── README.md # Additional prelude functions, general purpose utilities. Including Int_conv, LChar_utils, etc. Mostly as a reference for implementing your own. Can also be copied into your project and then imported.
├── reference/ # Language and API reference
│   ├── prelude/ # Module-level API docs
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
│   ├── all-prelude-module-signatures.md # Signatures of all modules in IML prelude. For a quick scan of what is available by default in IML.
│   ├── ordinal.md # Reference for ordinals used in termination proofs
│   └── tactics.md # Reference for proof tactics
├── verification/ # Verification guide
│   ├── proof-notes-and-practices.md # Notes and practical guidance for writing proofs in IML.
│   └── verification-guide.md # Verification guide for ImandraX, including tactic usage. Read this when working with any non-trivial proof-obligation tasks spawned by `let rec` (termination), `instance`, `verify`, `lemma` and `theorem`.
├── SKILL.md
├── codelogician-cli.md # Guide for using the  `codelogician` / `codelogician-lite` CLI to interact with ImandraX and access additional features.
├── iml-language-guide.md # IML language guide. Covers the syntax and built-in annotations, and how ImandraX works with IML. Includes examples, tips and pitfalls.
├── import-syntax.md # Import syntax in IML. For multi-file (multi-module) projects. Useful for separating types and functions definition from VGs and region-decompositions triggering commands.
├── region-decomp-intro.md # Intro to region decomposition, including concept explanations, basic usage, and common errors.
├── termination-proving.md # Termination proving using [@@measure ...] and the Ordinal module. Every `let rec` definition triggers a termination proving task. Read this when the default termination proving heuristics fail.
└── verification-with-verify-and-instance.md # Basic verification (prove a property or find a counter-example) with `verify` and `instance` commands.
```

Note: there are some *.iml examples and *.json files as well that are not shown in the tree. Find them yourself if needed.
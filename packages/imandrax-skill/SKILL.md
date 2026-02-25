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
- Based on requirements, resort to corresponding annotations to guide ImandraX
  - Option 1. Verification (prove a goal or find a counter-example)
    - Use `verify <func>` / `instance <func>`, which are dual to each other, with the former attempts to prove the goal and return a counter-example if it fails, and the latter attempts to find a counter-example and return it if it succeeds.
    - For more general theorem proving, especcially with tactics, use `theorem <name> <vars> = <body>`, `lemma <name> <vars> = <body>`, and `axiom <name> <vars> = <body>`.
  - Option 2. Region decomposition / Enumerative analysis / Test-case generation
    - Use `[@@decomp top <decomp-args> ()]` attached to function definitions to invoke region decomposition.
    - Generate test cases (Python or TypeScript) from regions with subcommands in `codelogician` CLI.


## Interact with ImandraX

To type-check, invoke verification, or invoke region decomposition, you pass your IML code to ImandraX.

- `codelogician` CLI: the preferred way for file-system-based agents to interact with ImandraX through LLM-friendly interface.
  - installation status: !`command -v codelogician &>/dev/null && echo "codelogician is installed" || echo "codelogician CLI is not installed. Install using "uv add codelogician" or "pip install codelogician"`
  - Note: `codelogician-tools` is a more light-weight CLI that is bundle with `codelogician` and should be preferred if available.
- Other interfaces: `imandrax-api` Python pacakge, `imandrax-cli` CLI

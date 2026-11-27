---
name: task-and-artifacts
description: Conceptual guide about tasks and artifacts
tags: [concept]
---

Task is a fundamental concept when interacting with ImandraX.
When the ImandraX server processes a request, it spawns tasks to perform the work.

## Task kinds

- Task kinds: `Task_unspecified`, `Task_eval`, `Task_check_po`, `Task_proof_check`, `Task_decomp`
- Three common task kinds:
  - `Task_check_po`: check the proof obligation
    - from `theorem`, `lemma`, `verify`, or `instance` definitions in IML.
    - from all `let rec` definitions: termination proofs
  - `Task_decomp`: decompose the proof obligation
    - from `[@@decomp top ()]` attributes or direct `decompose` / `decompose_full` endpoint calls
  - `Task_eval`: evaluate the expression
    - from `eval <expr>` commands

## Timeout

tasks have timeouts. see [timeouts](timeouts.md)

## Caching

- Tasks are cached by the server
- Task cache is keyed by the task itself
- One common way to invalidate a task is renaming identifiers.

## `[@@no_validate]` will suppress PO tasks in a definition

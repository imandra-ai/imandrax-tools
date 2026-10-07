---
name: eval-command
description: The `eval <expr>` syntax evaluates a closed IML expression and prints its value. Combined with `--task-filter=anonymous`, you can only run `eval` and ignores all verification as if you were running a script.
---

# The `eval` IML command

`eval <expr>` evaluates a closed IML expression and reports its value. It is the
quickest way to sanity-check that a function computes what you expect while
building up IML code, the equivalent of typing an expression at a REPL.

```iml
let rec fib (n:int) : int =
  if n <= 1 then n else fib (n-1) + fib (n-2)

eval fib 10          (* 55 *)
eval List.rev [1;2;3] (* [3;2;1] *)
eval (1, true, [4;5]) (* (1, true, [4;5]) *)
eval "hello" ^ " world" (* "hello world" *)
```

- `eval` takes an expression, not a binding (unlike `let`)
- CodeLogician CLI's `check` subcommand reports one result per `eval`, in source order, under `eval_result_1`, `eval_result_2`, ... with the value in the `value_as_ocaml` field.
- Note: `eval <expr>` as a syntaxic structure has no direct relation with "eval" in `codelogician eval ...`.

# Tip: compute without proofs

Combining with `--task-filter=anonymous` in `codelogician` CLI, you can only run `eval` and ignores all verification. This can be very useful because it allows you to treat an IML file as a script.

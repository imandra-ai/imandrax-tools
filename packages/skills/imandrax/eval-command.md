---
name: eval-command
description: The `eval <expr>` syntax evaluates a closed IML expression and prints its value. With `--task-filter=anonymous`, you can only run `eval` computation and ignores all verification as if you are running a script. With `--json`, you can write test cases by computation as if you are unit-testing regular programs.
---

## The `eval` IML command

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

## Tip: compute without proofs

Combining with `--task-filter=anonymous` in `codelogician` CLI, you can only run `eval` and ignores all verification. This can be very useful because it allows you to treat an IML file as a script.

## Tip: test by compute

```iml
let rec insert (x : int) = function
  | [] -> [x]
  | y :: ys as l -> if x <= y then x :: l else y :: insert x ys

let rec sort = function
  | [] -> []
  | x :: xs -> insert x (sort xs)

let assert_eq lhs rhs = (lhs = rhs)

eval (assert_eq (sort [3; 1; 2]) [1; 2; 3])
eval (assert_eq (sort [1; 2; 3]) [1; 2; 3])
eval (assert_eq (sort []) [])
```

We can use `codelogician eval check` to verify that all `eval` expressions in the file return `true`. With `--task-filter=anonymous`, no verification is performed so it will be faster if you only care about the computation results.

```sh
codelogician eval check my_insert_sort.iml --task-filter=anonymous --json | jq '.eval_res.eval_results | length > 0 and all(.[]; .value_as_ocaml == "true")'
# -> true
```

Organize these in a script or Makefile for easy reuse.

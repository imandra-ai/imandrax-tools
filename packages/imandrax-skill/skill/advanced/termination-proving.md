Since IML serves as both a programming language and a logic, function termination must be provable:

- For common recursion patterns, termination is proven automatically
- For complex cases, provide explicit termination measures using `[@@measure ...]`

**Ordinals in Termination Proofs:**
Termination measures in IML must return values of type `Ordinal.t`. This type represents ordinals up to ε₀ in Cantor Normal Form, providing a well-founded ordering essential for proving termination. When defining measures, you can use functions like `Ordinal.of_int` to convert integers to ordinals, or construct more complex ordinals for nested recursion.

**Example with explicit measure:**
```iml
let left_pad_measure n xs =
  Ordinal.of_int (n - List.length xs)

let rec left_pad c n xs =
  if List.length xs >= n then
    xs
  else
    left_pad c n (c :: xs)
[@@measure left_pad_measure n xs]
```

The measure shows that `n - List.length xs` decreases with each recursive call and remains non-negative, proving termination. The `Ordinal.of_int` function converts this integer difference to an ordinal value that Imandra can use to establish a well-founded ordering.

Note that `[@@measure ...]` can only be used for top-level functions, i.e., functions that are not defined within other functions.

# Last resort to bypass verification completely
As a **last resort**, `[@@no_validate]` disable verification for a function completely:

```iml
let rec left_pad c n xs =
  if List.length xs >= n then
    xs
  else
    left_pad c n (c :: xs)
[@@no_validate]
```

**Important limitation**: Using `[@@no_validate]` SIGNIFICANTLY limits Imandra's reasoning capabilities for this function.

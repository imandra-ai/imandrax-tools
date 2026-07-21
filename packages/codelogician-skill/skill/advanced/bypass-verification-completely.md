---
name: bypass-verification-completely
description: Last resort to bypass verification completely using `[@@no_validate]`. Don't use this unless you absolutely have the reason to.
---

`[@@no_validate]` disable verification for a function completely:

- Example 1: termination proving
```iml
let rec left_pad c n xs =
  if List.length xs >= n then
    xs
  else
    left_pad c n (c :: xs)
[@@no_validate]
```
- Example 2: theorem
```iml
theorem gcd_theorem = ...
[@@no_validate]
```

**Important limitation**: Using `[@@no_validate]` makes all downstream verification-related analysis unusable.
OCaml/IML infix operator keywords (`land`, `lor`, `lxor`, `lsl`, `lsr`, `asr`, `mod`) cannot be rebound directly with `let <op> = ...`. Wrap the operator name in parentheses to turn it into a prefix identifier:

```iml
let ( land ) : int -> int -> int = fun (x : int) (y : int) -> 0 [@@opaque]
```

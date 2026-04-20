Using OCaml stdlib in IML, causing:
```
Error{ Kind.name = "TypeErr" }:
  Unknown identifier `Array.make`.
  in file 'repro.iml', line 2 columns 13..22 [29..38]
  2 |   let arr = Array.make 5 0 in
                  ^^^^^^^^^^
```
- error_kind = `TypeErr`
- "Unknown identifier" in message
- capture: `Array.make` @ identifier
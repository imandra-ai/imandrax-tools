```
lemma triv (x : int) = x + 0 = x [@@by auto ()]
```

```
- err_kind: '{ Kind.name = "TypeErr" }'
  msg:
    msg: |-
      Application failed: expected argument of type `Tactic.goal`
      but got (() : unit)
    location:
      start:
        line: 17
        col: 40
      stop:
        line: 17
        col: 46
    error_src: |-
      15 |    (The fix for the user is to drop the parens: `[@@by auto]`.) *)
      16 |
      17 | lemma triv (x : int) = x + 0 = x [@@by auto ()]
         |                                        ^^^^^^
      18 |
      19 |
```


Should we say something like "`auto` has tactic type `t`, not `unit -> t`, so `auto ()` is a mistake."

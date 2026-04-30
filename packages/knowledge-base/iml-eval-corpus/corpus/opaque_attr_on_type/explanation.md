The `[@@opaque]` attribute can only be attached to a value binding (e.g.
`let f : t -> t = () [@@opaque]`), not to a type declaration. 

Drop the attribute from the type and, if you need an opaque value of that
type, attach `[@@opaque]` to a `let` binding instead:

```iml
type t

let mk : unit -> t = () [@@opaque]
```

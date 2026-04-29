The `[@@opaque]` attribute can only be attached to a value binding (e.g.
`let f : t -> t = () [@@opaque]`), not to a type declaration. Attaching it
to a `type` declaration leaves the type unprocessed and triggers an internal
error from ImandraX (`CIR.Type.Def.mk_l: uid ... must be 'to_be_rewritten'`)
the moment the type is referenced.

Drop the attribute from the type and, if you need an opaque value of that
type, attach `[@@opaque]` to a `let` binding instead:

```iml
type t

let mk : unit -> t = () [@@opaque]
```

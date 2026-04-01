(** Zarith-compatible [Z] and [Q] over the [num] library.

    Why this exists
    ===============

    ImandraX's prelude binds IML's [int] and [real] to zarith's [Z.t]/[Q.t].
    zarith cannot be compiled to WebAssembly: its primitives are C stubs,
    [zarith_stubs_js] reimplements them in JavaScript and is not picked up by
    [wasm_of_ocaml] at all, so every [ml_z_*] primitive traps as
    "not implemented" in both JS-hosted wasm and WASI mode.

    The [num] library does have a wasm implementation ([runtime/wasm/nat.wat]),
    but its [nat] is an abstract custom block with no compare function, so
    OCaml's structural equality raises [Invalid_argument "compare: abstract
    value"] on any value containing one. IML's [(=)] is polymorphic and applied
    to arbitrary data, so that is fatal.

    So values here are represented as {b canonical strings} and the arithmetic
    is delegated to [num]. Structural equality then works — on bare values and
    on records/lists containing them — because equal numbers have equal
    strings. Canonical forms:

    - [Z.t]: decimal, no leading zeros, ['-'] prefix when negative, ["0"] for zero.
    - [Q.t]: ["n/d"] with [d >= 2], [gcd(|n|,d) = 1]; a bare ["n"] when [d = 1].

    [Q]'s canonical form is deliberately the same as the JSON wire format, so
    encoding a [real] is the identity.

    The canonicality contract
    =========================

    Everything rests on one invariant:

    {b Two values are numerically equal iff their string representations are
    byte-identical.}

    Every constructor below funnels through a canonicalising function
    ([wrap] for [Z], [canon] for [Q]) to maintain it. [Z.of_int] and
    [Z.of_nativeint] skip the bignum round-trip only because [string_of_int]
    and [Nativeint.to_string] are already canonical — which also keeps the hot
    path cheap, [Z.of_nativeint] being how extracted integer literals arrive
    (83 occurrences in the prelude).

    {b If canonicality is violated the failure is silent and semantic, not a
    crash.} ["1/2"] and ["2/4"] would be unequal values that are numerically
    equal; ["007"] would be unequal to ["7"]. IML's [(=)] would simply return
    the wrong answer and nothing would raise. So: any new constructor, parser,
    or arithmetic result must be canonicalised before it escapes this module.

    Do not be tempted by the cheaper variant — keep the strings but skip
    normalisation, and compare with [Q.compare] instead of byte equality. It
    saves a gcd per construction and breaks the contract, and because
    polymorphic [(=)] cannot be intercepted, the resulting equality bug is
    unfixable without coming back here.

    Alternative representation
    --------------------------

    It is the {i canonicality}, not the stringness, that makes structural
    equality work. A canonical [{ sign : int; mag : int array }] in base 2^30
    would work identically and without the per-operation conversion — that is
    the documented optimization path, and this module's interface is narrow
    enough (~20 functions per module) that swapping it touches nothing else.
    It was not done for v1 because it means writing and validating 300-500
    lines of arithmetic (Euclidean division especially) instead of inheriting
    it from a library already verified to match IML. Nothing on opam helps:
    [bignum], [bigdecimal] and [zarith-ppx] all wrap zarith. The trigger for
    doing the work is a measured arithmetic cost; nobody has measured yet.

    Other caveats
    -------------

    - [Stdlib.compare] on these values is lexicographic, not numeric
      ([compare "10" "9" < 0]). Safe only because IML's ordering operators are
      monomorphic and bound explicitly to the functions here. The one leak is
      key order inside the prelude's [Map]/[Set], which stays a consistent
      total order — correct for lookup and equality, but not numeric order.
    - Non-finite floats have no representation here; [Q.of_float] rejects them
      rather than inventing zarith's [inf]/[undef] values.
    - [Z.to_int] raises [Failure] where zarith's raises [Z.Overflow].
*)

module B = Big_int

(* Z: arbitrary-precision integers
   ================================ *)

module Z = struct
  type t = string

  let un = B.big_int_of_string
  let wrap = B.string_of_big_int

  let zero = "0"
  let one = "1"
  let minus_one = "-1"

  (* [string_of_int] and [Nativeint.to_string] are already canonical, so the
     hot path for extracted integer literals avoids a bignum round-trip. *)
  let of_int = string_of_int
  let of_nativeint = Nativeint.to_string
  let of_int64 = Int64.to_string
  let of_string s = wrap (un s)

  let to_string s = s
  let to_int = int_of_string
  let to_float s = B.float_of_big_int (un s)

  let equal : t -> t -> bool = String.equal
  let compare a b = B.compare_big_int (un a) (un b)
  let sign s = B.sign_big_int (un s)

  let lt a b = compare a b < 0
  let leq a b = compare a b <= 0
  let gt a b = compare a b > 0
  let geq a b = compare a b >= 0

  let lift1 f a = wrap (f (un a))
  let lift2 f a b = wrap (f (un a) (un b))

  let add = lift2 B.add_big_int
  let sub = lift2 B.sub_big_int
  let mul = lift2 B.mult_big_int
  let neg = lift1 B.minus_big_int
  let abs = lift1 B.abs_big_int

  (* IML's [/] and [mod] are Euclidean: the remainder is always non-negative.
     [num]'s [div_big_int]/[mod_big_int] agree; verified against ImandraX. *)
  let ediv = lift2 B.div_big_int
  let erem = lift2 B.mod_big_int

  let min a b = if leq a b then a else b
  let max a b = if geq a b then a else b

  let pow base exp = wrap (B.power_big_int_positive_int (un base) exp)

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( ~- ) = neg

  let pp_print out s = Format.pp_print_string out s
end

(* Q: arbitrary-precision rationals
   ================================= *)

module Q = struct
  type t = string

  let bzero = B.zero_big_int
  let bone = B.unit_big_int

  (** Reduce [n/d] to canonical form. *)
  let canon n d =
    if B.sign_big_int d = 0 then raise Division_by_zero;
    let n, d =
      if B.sign_big_int d < 0 then (B.minus_big_int n, B.minus_big_int d) else (n, d)
    in
    let g = B.gcd_big_int n d in
    let g = if B.sign_big_int g = 0 then bone else g in
    (* [g] divides both exactly, so truncating vs Euclidean division agree. *)
    let n = B.div_big_int n g and d = B.div_big_int d g in
    if B.eq_big_int d bone then B.string_of_big_int n
    else B.string_of_big_int n ^ "/" ^ B.string_of_big_int d

  (** Split a canonical (or merely well-formed) string into numerator/denominator. *)
  let parts s =
    match String.index_opt s '/' with
    | None -> (B.big_int_of_string s, bone)
    | Some i ->
      ( B.big_int_of_string (String.sub s 0 i)
      , B.big_int_of_string (String.sub s (i + 1) (String.length s - i - 1)) )

  let make n d = canon (B.big_int_of_string n) (B.big_int_of_string d)
  let of_bigint (n : Z.t) = canon (B.big_int_of_string n) bone
  let of_int i = canon (B.big_int_of_int i) bone
  let of_ints n d = canon (B.big_int_of_int n) (B.big_int_of_int d)
  let of_string s = let n, d = parts s in canon n d
  let to_string s = s

  let zero = "0"
  let one = "1"

  let num s = B.string_of_big_int (fst (parts s))
  let den s = B.string_of_big_int (snd (parts s))

  (** Truncates toward zero, matching IML's [Real.to_int] (NOT [div_big_int],
      which floors and would give -4 rather than -3 for -7/2). *)
  let to_bigint s : Z.t =
    let n, d = parts s in
    let q =
      if B.sign_big_int n >= 0 then B.div_big_int n d
      else B.minus_big_int (B.div_big_int (B.minus_big_int n) d)
    in
    B.string_of_big_int q

  let to_float s = let n, d = parts s in B.float_of_big_int n /. B.float_of_big_int d

  (** Exact: decomposes the float into mantissa * 2^exp, no rounding. *)
  let of_float (f : float) =
    if Float.is_nan f || Float.abs f = Float.infinity then
      invalid_arg "Q.of_float: not finite";
    if f = 0.0 then zero
    else begin
      let m, e = Float.frexp f in
      let mi = B.big_int_of_string (Int64.to_string (Int64.of_float (Float.ldexp m 53))) in
      let e = e - 53 in
      if e >= 0 then canon (B.mult_big_int mi (B.power_int_positive_int 2 e)) bone
      else canon mi (B.power_int_positive_int 2 (-e))
    end

  let compare a b =
    let na, da = parts a and nb, db = parts b in
    B.compare_big_int (B.mult_big_int na db) (B.mult_big_int nb da)

  let equal : t -> t -> bool = String.equal
  let sign s = B.sign_big_int (fst (parts s))

  let lt a b = compare a b < 0
  let leq a b = compare a b <= 0
  let gt a b = compare a b > 0
  let geq a b = compare a b >= 0

  let arith f a b =
    let na, da = parts a and nb, db = parts b in
    let n, d = f (na, da) (nb, db) in
    canon n d

  let add = arith (fun (na, da) (nb, db) ->
    (B.add_big_int (B.mult_big_int na db) (B.mult_big_int nb da), B.mult_big_int da db))

  let sub = arith (fun (na, da) (nb, db) ->
    (B.sub_big_int (B.mult_big_int na db) (B.mult_big_int nb da), B.mult_big_int da db))

  let mul = arith (fun (na, da) (nb, db) ->
    (B.mult_big_int na nb, B.mult_big_int da db))

  let div = arith (fun (na, da) (nb, db) ->
    (B.mult_big_int na db, B.mult_big_int da nb))

  let neg s = let n, d = parts s in canon (B.minus_big_int n) d
  let abs s = let n, d = parts s in canon (B.abs_big_int n) d

  let min a b = if leq a b then a else b
  let max a b = if geq a b then a else b

  let pow base exp =
    let n, d = parts base in
    if exp >= 0 then
      canon (B.power_big_int_positive_int n exp) (B.power_big_int_positive_int d exp)
    else
      canon (B.power_big_int_positive_int d (-exp)) (B.power_big_int_positive_int n (-exp))

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( ~- ) = neg

  let pp_print out s = Format.pp_print_string out s
end

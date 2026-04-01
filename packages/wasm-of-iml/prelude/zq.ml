(** Zarith-compatible [Z] and [Q] over a canonical sign-magnitude bignum.

    Why this exists
    ===============

    ImandraX's prelude binds IML's [int] and [real] to zarith's [Z.t]/[Q.t],
    and zarith's primitives are C stubs. [wasm_of_ocaml] has no C toolchain:
    every [external] must be answered by a runtime implementation. The only
    such implementation for [ml_z_*] is [zarith_stubs_js], whose wasm runtime
    ([runtime.wat] in that package) is not an implementation at all but an
    import table — each primitive forwards to a JavaScript function operating
    on a JS [BigInt]. That works in JS-hosted wasm and cannot work under
    [--enable wasi], where there is no JS host to satisfy the imports.

    The [num] library is pure OCaml and does compile, but its [nat] is an
    abstract custom block with no compare function, so OCaml's structural
    equality raises [Invalid_argument "compare: abstract value"] on any value
    containing one. IML's [(=)] is polymorphic and applied to arbitrary data,
    so that is fatal.

    So the arithmetic is implemented here, over a representation chosen to make
    structural equality correct.

    The canonicality contract
    =========================

    Everything rests on one invariant:

    {b Two values are numerically equal iff their representations are
    structurally equal.}

    This is what lets IML's polymorphic [(=)] — which we cannot intercept —
    give the right answer for an [int] nested inside a record or a list. The
    representation is therefore canonical by construction:

    - [Z.t] is sign-magnitude. [sign] is in [-1; 0; 1] and is [0] {i iff} the
      magnitude is empty, so zero has exactly one representation. Limbs carry
      no most-significant zero, so every integer has exactly one limb array.
    - [Q.t] is a pair with [den > 0] and [gcd (|num|) den = 1]; zero is
      exactly [0/1]. So every rational has exactly one pair.

    Every constructor below funnels through a normalising function ([Mag.norm]
    and [Z.make] for integers, [Q.canon] for rationals) to maintain this.

    {b If canonicality is violated the failure is silent and semantic, not a
    crash.} An unreduced [2/4] would be unequal to [1/2]; a magnitude with a
    trailing zero limb would be unequal to the same number without it. IML's
    [(=)] would simply return the wrong answer and nothing would raise. So: any
    new constructor, parser, or arithmetic result must be normalised before it
    escapes this module.

    Why base 2^14
    =============

    [wasm_of_ocaml] represents an OCaml [int] as a WasmGC [i31ref], so
    [Sys.int_size] is 31 on that target and [max_int] is 2^30 - 1 — not the 63
    bits available natively. Schoolbook multiplication forms
    [limb + limb*limb + carry], which must stay under that bound:

    - base 2^14 gives < 2^28 + 2^15. Safe.
    - base 2^15 gives 2^30 + 1. Overflows silently on the target we ship.

    A base picked from [Sys.int_size] would be faster natively, but it would
    mean the native tests exercise different arithmetic from the wasm build —
    unacceptable for the one invariant that fails silently. One base, one code
    path, correct on 31-, 32- and 63-bit [int] alike.

    Other caveats
    -------------

    - [Stdlib.compare] on these values is {i not} numeric order — it compares
      sign, then limb count, then limbs from the least significant end. Safe
      only because IML's ordering operators are monomorphic and bound
      explicitly to the functions here. The one leak is key order inside the
      prelude's [Map]/[Set], which stays a consistent total order — correct for
      lookup and equality, but not numeric order.
    - Non-finite floats have no representation here; [Q.of_float] rejects them
      rather than inventing zarith's [inf]/[undef] values.
    - [Z.to_int] raises [Failure] where zarith's raises [Z.Overflow], and
      rejects [min_int] (one past the representable magnitude).
    - Conversions to float scale rather than divide; see [Q.to_float]. Getting
      this wrong produces [nan] for ratios a double can represent perfectly
      well, which is why both directions are round-trip tested.
*)

(* Checked on the target, at startup, because the failure it guards against is
   silent: pick too large a base and every native test still passes while the
   31-bit wasm build computes wrong answers. [Mag.mul] forms the worst-case
   term below, so this is the exact bound that matters. *)
let () =
  let base = 1 lsl 14 in
  let worst_mul_term = ((base - 1) * (base - 1)) + (base - 1) + (base - 1) in
  if Sys.int_size < 31 || worst_mul_term > max_int || worst_mul_term < 0 then
    failwith
      (Printf.sprintf "Zq: base 2^14 overflows this target's int (int_size=%d, max_int=%d)"
         Sys.int_size max_int)

(* Magnitudes
   ========== *)

(** Unsigned bignums: limbs little-endian in base [base], with no
    most-significant zero limb. The empty array is zero. *)
module Mag = struct
  let base_bits = 14
  let base = 1 lsl base_bits
  let mask = base - 1

  type t = int array

  let zero : t = [||]
  let is_zero (a : t) = Array.length a = 0

  (** Drop most-significant zero limbs. Every function returning a magnitude
      must end here, or the canonicality contract breaks. *)
  let norm (a : t) : t =
    let n = ref (Array.length a) in
    while !n > 0 && a.(!n - 1) = 0 do
      decr n
    done;
    if !n = Array.length a then a else Array.sub a 0 !n

  let compare (a : t) (b : t) =
    let la = Array.length a and lb = Array.length b in
    if la <> lb then Stdlib.compare la lb
    else begin
      let rec go i =
        if i < 0 then 0
        else if a.(i) <> b.(i) then Stdlib.compare a.(i) b.(i)
        else go (i - 1)
      in
      go (la - 1)
    end

  let add (a : t) (b : t) : t =
    let la = Array.length a and lb = Array.length b in
    let lmax = if la > lb then la else lb in
    let r = Array.make (lmax + 1) 0 in
    let carry = ref 0 in
    for i = 0 to lmax - 1 do
      let s = (if i < la then a.(i) else 0) + (if i < lb then b.(i) else 0) + !carry in
      r.(i) <- s land mask;
      carry := s lsr base_bits
    done;
    r.(lmax) <- !carry;
    norm r

  (** Requires [a >= b]. *)
  let sub (a : t) (b : t) : t =
    let la = Array.length a and lb = Array.length b in
    let r = Array.make la 0 in
    let borrow = ref 0 in
    for i = 0 to la - 1 do
      let d = a.(i) - (if i < lb then b.(i) else 0) - !borrow in
      if d < 0 then begin
        r.(i) <- d + base;
        borrow := 1
      end
      else begin
        r.(i) <- d;
        borrow := 0
      end
    done;
    assert (!borrow = 0);
    norm r

  let mul (a : t) (b : t) : t =
    let la = Array.length a and lb = Array.length b in
    if la = 0 || lb = 0 then zero
    else begin
      let r = Array.make (la + lb) 0 in
      for i = 0 to la - 1 do
        let ai = a.(i) in
        if ai <> 0 then begin
          let carry = ref 0 in
          for j = 0 to lb - 1 do
            (* < 2^14 + 2^28 + 2^14, so this fits a 31-bit int. *)
            let t = r.(i + j) + (ai * b.(j)) + !carry in
            r.(i + j) <- t land mask;
            carry := t lsr base_bits
          done;
          let k = ref (i + lb) in
          while !carry <> 0 do
            let t = r.(!k) + !carry in
            r.(!k) <- t land mask;
            carry := t lsr base_bits;
            incr k
          done
        end
      done;
      norm r
    end

  (** [a * m + c], for [m] and [c] small enough that [limb * m + carry] stays
      below [max_int]. The bound is [m <= 10^4]: that keeps the carry inside a
      single limb, which is all the two spare limbs below allow for. *)
  let mul_small_add (a : t) (m : int) (c : int) : t =
    assert (m <= 10_000 && c >= 0);
    let la = Array.length a in
    let r = Array.make (la + 2) 0 in
    let carry = ref c in
    for i = 0 to la - 1 do
      let t = (a.(i) * m) + !carry in
      r.(i) <- t land mask;
      carry := t lsr base_bits
    done;
    let k = ref la in
    while !carry <> 0 do
      r.(!k) <- !carry land mask;
      carry := !carry lsr base_bits;
      incr k
    done;
    norm r

  (** Divide by a small positive [d], returning quotient and remainder. *)
  let divmod_small (a : t) (d : int) : t * int =
    let la = Array.length a in
    let q = Array.make la 0 in
    let r = ref 0 in
    for i = la - 1 downto 0 do
      (* [!r < d <= 10^4] and [base = 2^14], so this stays under 2^28. *)
      let cur = (!r * base) + a.(i) in
      q.(i) <- cur / d;
      r := cur mod d
    done;
    (norm q, !r)

  let shift_left_bits (a : t) (s : int) : t =
    if s = 0 then a
    else begin
      let la = Array.length a in
      let r = Array.make (la + 1) 0 in
      let carry = ref 0 in
      for i = 0 to la - 1 do
        let t = (a.(i) lsl s) + !carry in
        r.(i) <- t land mask;
        carry := t lsr base_bits
      done;
      r.(la) <- !carry;
      norm r
    end

  (** Number of significant bits; 0 for zero. *)
  let numbits (a : t) =
    let la = Array.length a in
    if la = 0 then 0
    else begin
      let top = ref a.(la - 1) and n = ref 0 in
      while !top > 0 do
        incr n;
        top := !top lsr 1
      done;
      ((la - 1) * base_bits) + !n
    end

  (** Is any bit below position [k] set? The sticky bit for rounding. *)
  let any_bit_below (a : t) (k : int) =
    let limb = k / base_bits and off = k mod base_bits in
    let found = ref false in
    for i = 0 to Stdlib.min (limb - 1) (Array.length a - 1) do
      if a.(i) <> 0 then found := true
    done;
    if (not !found) && off > 0 && limb < Array.length a then
      if a.(limb) land ((1 lsl off) - 1) <> 0 then found := true;
    !found

  (** Requires the value to fit in 63 bits. *)
  let to_int64 (a : t) =
    let acc = ref 0L in
    let b64 = Int64.of_int base in
    for i = Array.length a - 1 downto 0 do
      acc := Int64.add (Int64.mul !acc b64) (Int64.of_int a.(i))
    done;
    !acc

  let shift_right_bits (a : t) (s : int) : t =
    if s = 0 then a
    else begin
      let la = Array.length a in
      let r = Array.make la 0 in
      let carry = ref 0 in
      for i = la - 1 downto 0 do
        r.(i) <- (a.(i) lsr s) lor !carry;
        carry := (a.(i) land ((1 lsl s) - 1)) lsl (base_bits - s)
      done;
      norm r
    end

  (** Shift by an arbitrary number of bits, whole limbs included. *)
  let shift_left (a : t) (k : int) : t =
    if is_zero a || k = 0 then a
    else begin
      let limbs = k / base_bits and bits = k mod base_bits in
      let shifted = shift_left_bits a bits in
      let r = Array.make (Array.length shifted + limbs) 0 in
      Array.blit shifted 0 r limbs (Array.length shifted);
      norm r
    end

  let shift_right (a : t) (k : int) : t =
    let limbs = k / base_bits and bits = k mod base_bits in
    let la = Array.length a in
    if limbs >= la then zero else shift_right_bits (Array.sub a limbs (la - limbs)) bits

  (** Knuth TAOCP 4.3.1 algorithm D. Requires [Array.length v >= 2] and
      [compare u v >= 0]. *)
  let divmod_knuth (u : t) (v : t) : t * t =
    let n = Array.length v in
    let lu = Array.length u in
    (* Normalise so the divisor's top limb is at least base/2. *)
    let s = ref 0 in
    let top = ref v.(n - 1) in
    while !top < base / 2 do
      top := !top * 2;
      incr s
    done;
    let s = !s in
    (* [s] was chosen so the shift cannot grow [v], but it can grow [u]; [un]
       has the extra limb algorithm D requires either way. *)
    let vn = shift_left_bits v s in
    let us = shift_left_bits u s in
    let un = Array.make (lu + 1) 0 in
    Array.blit us 0 un 0 (Array.length us);
    let m = lu - n in
    let q = Array.make (m + 1) 0 in
    for j = m downto 0 do
      (* Estimate the next quotient limb from the top two limbs. *)
      let num = (un.(j + n) * base) + un.(j + n - 1) in
      let qhat = ref (num / vn.(n - 1)) in
      let rhat = ref (num mod vn.(n - 1)) in
      let continue_ = ref true in
      while !continue_ do
        if !qhat >= base || (!qhat * vn.(n - 2)) > ((!rhat * base) + un.(j + n - 2)) then begin
          decr qhat;
          rhat := !rhat + vn.(n - 1);
          if !rhat >= base then continue_ := false
        end
        else continue_ := false
      done;
      (* Multiply and subtract. *)
      let borrow = ref 0 and carry = ref 0 in
      for i = 0 to n - 1 do
        let p = (!qhat * vn.(i)) + !carry in
        carry := p lsr base_bits;
        let t = un.(i + j) - (p land mask) - !borrow in
        if t < 0 then begin
          un.(i + j) <- t + base;
          borrow := 1
        end
        else begin
          un.(i + j) <- t;
          borrow := 0
        end
      done;
      let t = un.(j + n) - !carry - !borrow in
      if t < 0 then begin
        (* Estimate was one too large: add the divisor back. *)
        un.(j + n) <- t + base;
        decr qhat;
        let carry = ref 0 in
        for i = 0 to n - 1 do
          let s = un.(i + j) + vn.(i) + !carry in
          un.(i + j) <- s land mask;
          carry := s lsr base_bits
        done;
        un.(j + n) <- (un.(j + n) + !carry) land mask
      end
      else un.(j + n) <- t;
      q.(j) <- !qhat
    done;
    let r = shift_right_bits (norm (Array.sub un 0 n)) s in
    (norm q, r)

  (** Truncating division: [a = b * q + r] with [0 <= r < b]. *)
  let divmod (a : t) (b : t) : t * t =
    if is_zero b then raise Division_by_zero;
    if compare a b < 0 then (zero, a)
    else if Array.length b = 1 then begin
      let q, r = divmod_small a b.(0) in
      (q, norm [| r |])
    end
    else divmod_knuth a b

  let rec gcd (a : t) (b : t) : t = if is_zero b then a else gcd b (snd (divmod a b))
end

(* Z: arbitrary-precision integers
   ================================ *)

module Z = struct
  type t = {
    sign : int;  (** [-1], [0] or [1]; [0] iff [mag] is empty *)
    mag : Mag.t;
  }

  (** The only way to build a [t]. Enforces "sign is 0 iff magnitude is 0". *)
  let make sign (mag : Mag.t) = if Mag.is_zero mag then { sign = 0; mag = Mag.zero } else { sign; mag }

  let zero = { sign = 0; mag = Mag.zero }
  let one = { sign = 1; mag = [| 1 |] }
  let minus_one = { sign = -1; mag = [| 1 |] }

  let sign z = z.sign
  let equal : t -> t -> bool = ( = )

  let compare a b =
    if a.sign <> b.sign then Stdlib.compare a.sign b.sign
    else if a.sign >= 0 then Mag.compare a.mag b.mag
    else Mag.compare b.mag a.mag

  let lt a b = compare a b < 0
  let leq a b = compare a b <= 0
  let gt a b = compare a b > 0
  let geq a b = compare a b >= 0
  let min a b = if leq a b then a else b
  let max a b = if geq a b then a else b

  let neg z = make (- z.sign) z.mag
  let abs z = make (Stdlib.abs z.sign) z.mag

  let add a b =
    if a.sign = 0 then b
    else if b.sign = 0 then a
    else if a.sign = b.sign then make a.sign (Mag.add a.mag b.mag)
    else begin
      let c = Mag.compare a.mag b.mag in
      if c = 0 then zero
      else if c > 0 then make a.sign (Mag.sub a.mag b.mag)
      else make b.sign (Mag.sub b.mag a.mag)
    end

  let sub a b = add a (neg b)
  let mul a b = make (a.sign * b.sign) (Mag.mul a.mag b.mag)

  (** Truncating division, the building block for the Euclidean pair below. *)
  let tdivmod a b =
    if b.sign = 0 then raise Division_by_zero;
    let q, r = Mag.divmod a.mag b.mag in
    (make (a.sign * b.sign) q, make a.sign r)

  (* IML's [/] and [mod] are Euclidean: the remainder is always non-negative.
     Verified against ImandraX — [-7/2 = -4], [-7 mod 2 = 1], [7/(-2) = -3],
     [7 mod (-2) = 1]. Truncation gives the wrong quotient for a negative
     dividend, so correct it by one step toward the remainder's sign. *)
  let ediv_erem a b =
    let q, r = tdivmod a b in
    if r.sign >= 0 then (q, r)
    else if b.sign > 0 then (sub q one, add r b)
    else (add q one, sub r b)

  let ediv a b = fst (ediv_erem a b)
  let erem a b = snd (ediv_erem a b)
  let gcd a b = make 1 (Mag.gcd (abs a).mag (abs b).mag)

  (** Exact division: [b] must divide [a]. Used only where the remainder is
      known to be zero, so truncating and Euclidean division agree. *)
  let divexact a b = fst (tdivmod a b)

  let of_int64 (v : int64) =
    if Int64.equal v 0L then zero
    else begin
      let sign = if Int64.compare v 0L < 0 then -1 else 1 in
      let limbs = ref [] in
      let x = ref v in
      let b64 = Int64.of_int Mag.base in
      while not (Int64.equal !x 0L) do
        (* [Int64.rem] keeps the dividend's sign, and no limb can be
           [Int64.min_int], so [abs] is safe even for [min_int] itself. *)
        limbs := Int64.to_int (Int64.abs (Int64.rem !x b64)) :: !limbs;
        x := Int64.div !x b64
      done;
      make sign (Mag.norm (Array.of_list (List.rev !limbs)))
    end

  let of_int i = of_int64 (Int64.of_int i)
  let of_nativeint n = of_int64 (Int64.of_nativeint n)

  let to_int z =
    let acc = ref 0 in
    (try
       for i = Array.length z.mag - 1 downto 0 do
         if !acc > (Stdlib.max_int - z.mag.(i)) / Mag.base then raise Exit;
         acc := (!acc * Mag.base) + z.mag.(i)
       done
     with Exit -> failwith "Z.to_int: out of range");
    if z.sign < 0 then - !acc else !acc

  let numbits z = Mag.numbits z.mag
  let shift_left z k = make z.sign (Mag.shift_left z.mag k)

  (* Round to nearest, ties to even — the same result [strtod] gives for the
     decimal spelling of the value.

     Accumulating [acc *. base +. limb] in a float would instead round at every
     limb, which drifts up to an ulp for values past 2^53. *)
  let to_float z =
    if z.sign = 0 then 0.0
    else begin
      let nb = Mag.numbits z.mag in
      let f =
        if nb <= 53 then Int64.to_float (Mag.to_int64 z.mag)
        else begin
          (* Keep 53 significant bits plus one round bit, and remember whether
             anything nonzero fell off below them. *)
          let drop = nb - 54 in
          let hi = Mag.to_int64 (Mag.shift_right z.mag drop) in
          let sticky = Mag.any_bit_below z.mag drop in
          let round_bit = Int64.logand hi 1L in
          let kept = Int64.shift_right_logical hi 1 in
          let kept =
            if Int64.equal round_bit 1L && (sticky || Int64.equal (Int64.logand kept 1L) 1L)
            then Int64.add kept 1L
            else kept
          in
          (* [kept] is at most 2^53, so this conversion is exact. *)
          Float.ldexp (Int64.to_float kept) (drop + 1)
        end
      in
      if z.sign < 0 then -.f else f
    end

  (* Decimal conversion goes four digits at a time: [10^4 * limb + carry] stays
     well under [max_int] at base 2^14, and it quarters the number of passes. *)
  let chunk = 10000
  let chunk_digits = 4
  let pow10 = [| 1; 10; 100; 1000; 10000 |]

  let of_string s =
    let n = String.length s in
    if n = 0 then failwith "Z.of_string: empty";
    let sign, start =
      match s.[0] with '-' -> (-1, 1) | '+' -> (1, 1) | _ -> (1, 0)
    in
    if start >= n then failwith "Z.of_string: no digits";
    let mag = ref Mag.zero in
    let i = ref start in
    while !i < n do
      let take = Stdlib.min chunk_digits (n - !i) in
      let acc = ref 0 in
      for k = 0 to take - 1 do
        let c = s.[!i + k] in
        if c < '0' || c > '9' then failwith "Z.of_string: not a decimal integer";
        acc := (!acc * 10) + (Char.code c - Char.code '0')
      done;
      mag := Mag.mul_small_add !mag pow10.(take) !acc;
      i := !i + take
    done;
    make sign !mag

  let to_string z =
    if z.sign = 0 then "0"
    else begin
      let buf = Buffer.create 16 in
      let rec go m acc =
        if Mag.is_zero m then acc
        else begin
          let q, r = Mag.divmod_small m chunk in
          go q (r :: acc)
        end
      in
      match go z.mag [] with
      | [] -> "0"
      | hd :: tl ->
        if z.sign < 0 then Buffer.add_char buf '-';
        Buffer.add_string buf (string_of_int hd);
        List.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%04d" c)) tl;
        Buffer.contents buf
    end

  let pow base exp =
    if exp < 0 then invalid_arg "Z.pow: negative exponent";
    let rec go b e acc = if e = 0 then acc else go (mul b b) (e lsr 1) (if e land 1 = 1 then mul acc b else acc) in
    go base exp one

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( ~- ) = neg
  let pp_print out z = Format.pp_print_string out (to_string z)
end

(* Q: arbitrary-precision rationals
   ================================= *)

module Q = struct
  (** [den] is always strictly positive and coprime with [num]; zero is
      exactly [0/1]. See the canonicality contract at the top of the file. *)
  type t = {
    qnum : Z.t;
    qden : Z.t;
  }

  let canon n d =
    if Z.sign d = 0 then raise Division_by_zero;
    let n, d = if Z.sign d < 0 then (Z.neg n, Z.neg d) else (n, d) in
    if Z.sign n = 0 then { qnum = Z.zero; qden = Z.one }
    else begin
      let g = Z.gcd n d in
      if Z.equal g Z.one then { qnum = n; qden = d }
      else { qnum = Z.divexact n g; qden = Z.divexact d g }
    end

  let make n d = canon n d
  let of_bigint (n : Z.t) = { qnum = n; qden = Z.one }
  let of_int i = of_bigint (Z.of_int i)
  let of_ints n d = canon (Z.of_int n) (Z.of_int d)

  let zero = { qnum = Z.zero; qden = Z.one }
  let one = { qnum = Z.one; qden = Z.one }

  let num q = q.qnum
  let den q = q.qden

  let of_string s =
    match String.index_opt s '/' with
    | None -> of_bigint (Z.of_string s)
    | Some i ->
      canon
        (Z.of_string (String.sub s 0 i))
        (Z.of_string (String.sub s (i + 1) (String.length s - i - 1)))

  (* The canonical form is also the JSON wire format for a [real]: "n/d", or a
     bare "n" when the denominator is 1. *)
  let to_string q =
    if Z.equal q.qden Z.one then Z.to_string q.qnum
    else Z.to_string q.qnum ^ "/" ^ Z.to_string q.qden

  (** Truncates toward zero, matching IML's [Real.to_int] — NOT a floor, which
      would give -4 rather than -3 for -7/2. *)
  let to_bigint q =
    let a = Z.ediv (Z.abs q.qnum) q.qden in
    if Z.sign q.qnum < 0 then Z.neg a else a

  (* Converting numerator and denominator separately and dividing is wrong for
     anything a double cannot hold: either side overflows to infinity (or
     underflows to zero) and the quotient becomes nan, even when the ratio
     itself is perfectly ordinary. 1/2^1122 — a normal subnormal float — came
     back as 0.0 that way.

     So scale first: shift until the integer quotient carries ~64 significant
     bits, then apply the scale with [ldexp], which saturates to 0.0 or
     infinity only when the true value really is out of range. *)
  let to_float q =
    if Z.sign q.qnum = 0 then 0.0
    else begin
      let n = Z.abs q.qnum and d = q.qden in
      let shift = 64 + Z.numbits d - Z.numbits n in
      let n, d = if shift >= 0 then (Z.shift_left n shift, d) else (n, Z.shift_left d (-shift)) in
      let f = Float.ldexp (Z.to_float (Z.ediv n d)) (-shift) in
      if Z.sign q.qnum < 0 then -.f else f
    end

  (** Exact: decomposes the float into mantissa * 2^exp, no rounding. So 0.1
      becomes 3602879701896397/36028797018963968 — the rational the float
      actually denotes, which is what IML's [Real.of_float] means. *)
  let of_float (f : float) =
    if Float.is_nan f || Float.abs f = Float.infinity then invalid_arg "Q.of_float: not finite";
    if f = 0.0 then zero
    else begin
      let m, e = Float.frexp f in
      let mi = Z.of_int64 (Int64.of_float (Float.ldexp m 53)) in
      let e = e - 53 in
      let two = Z.of_int 2 in
      if e >= 0 then of_bigint (Z.mul mi (Z.pow two e)) else canon mi (Z.pow two (-e))
    end

  let equal : t -> t -> bool = ( = )
  let sign q = Z.sign q.qnum
  let compare a b = Z.compare (Z.mul a.qnum b.qden) (Z.mul b.qnum a.qden)
  let lt a b = compare a b < 0
  let leq a b = compare a b <= 0
  let gt a b = compare a b > 0
  let geq a b = compare a b >= 0
  let min a b = if leq a b then a else b
  let max a b = if geq a b then a else b

  let add a b = canon (Z.add (Z.mul a.qnum b.qden) (Z.mul b.qnum a.qden)) (Z.mul a.qden b.qden)
  let sub a b = canon (Z.sub (Z.mul a.qnum b.qden) (Z.mul b.qnum a.qden)) (Z.mul a.qden b.qden)
  let mul a b = canon (Z.mul a.qnum b.qnum) (Z.mul a.qden b.qden)
  let div a b = canon (Z.mul a.qnum b.qden) (Z.mul a.qden b.qnum)
  let neg q = { qnum = Z.neg q.qnum; qden = q.qden }
  let abs q = { qnum = Z.abs q.qnum; qden = q.qden }

  let pow base exp =
    if exp >= 0 then canon (Z.pow base.qnum exp) (Z.pow base.qden exp)
    else canon (Z.pow base.qden (-exp)) (Z.pow base.qnum (-exp))

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( ~- ) = neg
  let pp_print out q = Format.pp_print_string out (to_string q)
end

(* Correctness tests for the Z/Q bignum.

   Three things are most likely to be subtly wrong, in this order:
   the canonicality contract (fails silently), Knuth division (fails on
   carry/borrow edge cases only), and exact float decomposition.

   [num] is a dev-only dependency here, used purely as a differential oracle:
   it is a separate implementation of the same arithmetic, and its Euclidean
   division was verified against ImandraX when this package was written. *)

(* No [open Iml_prelude]: the prelude shadows Stdlib's List, (=), etc.
   These tests exercise the bignum, so they need plain Stdlib semantics. *)
module Z = Iml_prelude.Z
module Q = Iml_prelude.Q
module B = Big_int

let fails = ref 0

let check name cond =
  if not cond then (incr fails; Printf.printf "FAIL %s\n" name)

let checkeq name ~expect got =
  if expect <> got then begin
    incr fails;
    Printf.printf "FAIL %-34s expect %-14s got %s\n" name expect got
  end

let zs = Z.to_string
let qs = Q.to_string

let bi = B.big_int_of_string

(* Random decimal of the given length, signed. Shared by every differential
   test below. *)
let rand_decimal digits =
  let b = Buffer.create (digits + 1) in
  if Random.bool () then Buffer.add_char b '-';
  Buffer.add_char b (Char.chr (Char.code '1' + Random.int 9));
  for _ = 2 to digits do
    Buffer.add_char b (Char.chr (Char.code '0' + Random.int 10))
  done;
  Buffer.contents b

(* Euclidean division: IML guarantees 0 <= r < |b| and a = b*q + r.
   Checked as a property over many sign combinations, not just samples. *)
let test_euclidean () =
  let vals = [-100; -17; -7; -3; -1; 0; 1; 3; 7; 17; 100] in
  List.iter (fun a ->
    List.iter (fun b ->
      if b <> 0 then begin
        let za = Z.of_int a and zb = Z.of_int b in
        let q = Z.ediv za zb and r = Z.erem za zb in
        let name = Printf.sprintf "euclid %d/%d" a b in
        (* a = b*q + r *)
        checkeq (name ^ " identity") ~expect:(zs (Z.of_int a)) (zs (Z.add (Z.mul zb q) r));
        (* 0 <= r < |b| *)
        check (name ^ " r>=0") (Z.geq r Z.zero);
        check (name ^ " r<|b|") (Z.lt r (Z.abs zb))
      end) vals) vals;
  (* The cases pinned against ImandraX in PLAN.md. *)
  checkeq "-7/2" ~expect:"-4" (zs (Z.ediv (Z.of_int (-7)) (Z.of_int 2)));
  checkeq "-7 mod 2" ~expect:"1" (zs (Z.erem (Z.of_int (-7)) (Z.of_int 2)));
  checkeq "7/-2" ~expect:"-3" (zs (Z.ediv (Z.of_int 7) (Z.of_int (-2))));
  checkeq "7 mod -2" ~expect:"1" (zs (Z.erem (Z.of_int 7) (Z.of_int (-2))));
  checkeq "-7/-2" ~expect:"4" (zs (Z.ediv (Z.of_int (-7)) (Z.of_int (-2))));
  checkeq "-7 mod -2" ~expect:"1" (zs (Z.erem (Z.of_int (-7)) (Z.of_int (-2))))

(* Exact float -> rational. 0.1 is the classic case: it is NOT 1/10. *)
let test_of_float () =
  checkeq "of_float 1.5" ~expect:"3/2" (qs (Q.of_float 1.5));
  checkeq "of_float -1.5" ~expect:"-3/2" (qs (Q.of_float (-1.5)));
  checkeq "of_float 0.0" ~expect:"0" (qs (Q.of_float 0.0));
  checkeq "of_float 2.0" ~expect:"2" (qs (Q.of_float 2.0));
  checkeq "of_float 0.25" ~expect:"1/4" (qs (Q.of_float 0.25));
  checkeq "of_float 1e3" ~expect:"1000" (qs (Q.of_float 1e3));
  (* exactness: 0.1 is 3602879701896397/2^55 *)
  checkeq "of_float 0.1 (exact)"
    ~expect:"3602879701896397/36028797018963968" (qs (Q.of_float 0.1));
  check "of_float roundtrips" (Q.to_float (Q.of_float 0.1) = 0.1);
  check "of_float 3.14 roundtrips" (Q.to_float (Q.of_float 3.14) = 3.14)

(* Canonicality is what makes structural equality correct. This is the
   invariant whose violation is silent, so it is tested hardest: values built
   by different routes must be structurally identical, not merely equal under
   the comparison functions. *)
type rec_with_int = { a : Z.t; b : Q.t }

let test_canonical () =
  checkeq "2/4 reduces" ~expect:"1/2" (qs (Q.of_string "2/4"));
  checkeq "-2/-4 reduces" ~expect:"1/2" (qs (Q.of_string "-2/-4"));
  checkeq "2/-4 sign moves up" ~expect:"-1/2" (qs (Q.of_string "2/-4"));
  checkeq "4/2 becomes int" ~expect:"2" (qs (Q.of_string "4/2"));
  checkeq "0/5 is zero" ~expect:"0" (qs (Q.of_string "0/5"));
  check "1/2 = 2/4 structurally" (Q.of_string "1/2" = Q.of_string "2/4");
  check "structural eq in record"
    ({ a = Z.of_int 1; b = Q.of_string "2/4" } = { a = Z.of_string "1"; b = Q.of_string "1/2" });
  check "structural eq in list" ([ Z.of_int 7 ] = [ Z.of_string "7" ]);
  check "structural neq" (Z.of_int 7 <> Z.of_int 8);
  (* Zero has exactly one representation, whichever way it arises. *)
  check "zero from sub" (Z.sub (Z.of_int 5) (Z.of_int 5) = Z.zero);
  check "zero from mul" (Z.mul (Z.of_int 0) (Z.of_int 99) = Z.zero);
  check "zero from string" (Z.of_string "-0" = Z.zero);
  check "zero from neg" (Z.neg Z.zero = Z.zero);
  check "Q zero from sub" (Q.sub (Q.of_string "3/2") (Q.of_string "3/2") = Q.zero);
  (* Carries and borrows must leave no trailing zero limb: 2^14 is the base. *)
  check "limb boundary up" (Z.add (Z.of_int 16383) Z.one = Z.of_string "16384");
  check "limb boundary down" (Z.sub (Z.of_string "16384") Z.one = Z.of_int 16383);
  check "limb boundary mul" (Z.mul (Z.of_int 128) (Z.of_int 128) = Z.of_string "16384");
  check "two-limb cancel" (Z.sub (Z.of_string "268435456") (Z.of_string "268435456") = Z.zero);
  (* Same number, built by four different routes. *)
  let big = "123456789012345678901234567890" in
  check "route: string vs add"
    (Z.of_string big = Z.add (Z.of_string "123456789012345678901234567889") Z.one);
  check "route: string vs mul10"
    (Z.of_string big = Z.mul (Z.of_string "12345678901234567890123456789") (Z.of_int 10));
  check "route: string vs div"
    (Z.of_string big = Z.ediv (Z.of_string "246913578024691357802469135780") (Z.of_int 2))

(* to_bigint must truncate toward zero, matching IML's Real.to_int. *)
let test_to_bigint () =
  checkeq "to_bigint 7/2" ~expect:"3" (zs (Q.to_bigint (Q.of_string "7/2")));
  checkeq "to_bigint -7/2" ~expect:"-3" (zs (Q.to_bigint (Q.of_string "-7/2")));
  checkeq "to_bigint -1/2" ~expect:"0" (zs (Q.to_bigint (Q.of_string "-1/2")));
  checkeq "to_bigint 5" ~expect:"5" (zs (Q.to_bigint (Q.of_string "5")))

let test_bignum () =
  let big = Z.of_string "123456789012345678901234567890" in
  checkeq "big * 7" ~expect:"864197523086419752308641975230" (zs (Z.mul big (Z.of_int 7)));
  checkeq "big precision kept" ~expect:"123456789012345678901234567890" (zs big);
  checkeq "pow" ~expect:"1024" (zs (Z.pow (Z.of_int 2) 10));
  checkeq "Q pow neg exp" ~expect:"4/9" (qs (Q.pow (Q.of_string "3/2") (-2)));
  checkeq "Q arith" ~expect:"15/14" (qs (Q.div (Q.of_string "3/2") (Q.of_string "7/5")));
  check "Q compare" (Q.lt (Q.of_string "1/2") (Q.of_string "3/4"));
  check "Q compare neg" (Q.lt (Q.of_string "-3/4") (Q.of_string "-1/2"))

(* Conversions at the edges of the machine word and the limb base. *)
let test_conversions () =
  checkeq "of_int max_int" ~expect:(string_of_int max_int) (zs (Z.of_int max_int));
  checkeq "of_int min_int" ~expect:(string_of_int min_int) (zs (Z.of_int min_int));
  checkeq "of_int64 min" ~expect:"-9223372036854775808" (zs (Z.of_int64 Int64.min_int));
  checkeq "of_int64 max" ~expect:"9223372036854775807" (zs (Z.of_int64 Int64.max_int));
  checkeq "of_nativeint" ~expect:"-1" (zs (Z.of_nativeint (-1n)));
  checkeq "to_int roundtrip" ~expect:"12345" (string_of_int (Z.to_int (Z.of_int 12345)));
  checkeq "to_int negative" ~expect:"-12345" (string_of_int (Z.to_int (Z.of_int (-12345))));
  check "to_int overflow raises"
    (try ignore (Z.to_int (Z.of_string "99999999999999999999999999")); false
     with Failure _ -> true);
  check "of_string rejects junk"
    (try ignore (Z.of_string "12a4"); false with Failure _ -> true);
  check "of_string rejects empty"
    (try ignore (Z.of_string ""); false with Failure _ -> true);
  checkeq "of_string plus sign" ~expect:"42" (zs (Z.of_string "+42"));
  checkeq "of_string leading zeros" ~expect:"42" (zs (Z.of_string "0042"));
  check "to_float" (Z.to_float (Z.of_string "1000000") = 1e6);
  check "to_float negative" (Z.to_float (Z.of_int (-8)) = -8.0)

(* Differential testing against [num]. This is the real safety net for
   division: Knuth's algorithm D has correction paths that only trigger on
   specific limb patterns, which fixed samples will not find. *)
let test_differential () =
  Random.init 20260914;
  let cases = 3000 in
  for _ = 1 to cases do
    (* Sizes spanning the single-limb path, the Knuth path, and lopsided
       operand pairs where the quotient needs many limbs. *)
    let da = 1 + Random.int 40 and db = 1 + Random.int 20 in
    let sa = rand_decimal da and sb = rand_decimal db in
    let za = Z.of_string sa and zb = Z.of_string sb in
    let ba = bi sa and bb = bi sb in
    let cmp name got expect =
      if got <> expect then begin
        incr fails;
        Printf.printf "FAIL diff %-8s a=%s b=%s\n  got    %s\n  expect %s\n" name sa sb got expect
      end
    in
    cmp "roundtrip" (zs za) (B.string_of_big_int ba);
    cmp "add" (zs (Z.add za zb)) (B.string_of_big_int (B.add_big_int ba bb));
    cmp "sub" (zs (Z.sub za zb)) (B.string_of_big_int (B.sub_big_int ba bb));
    cmp "mul" (zs (Z.mul za zb)) (B.string_of_big_int (B.mult_big_int ba bb));
    cmp "ediv" (zs (Z.ediv za zb)) (B.string_of_big_int (B.div_big_int ba bb));
    cmp "erem" (zs (Z.erem za zb)) (B.string_of_big_int (B.mod_big_int ba bb));
    cmp "compare" (string_of_int (Z.compare za zb)) (string_of_int (B.compare_big_int ba bb));
    cmp "gcd" (zs (Z.gcd za zb)) (B.string_of_big_int (B.gcd_big_int ba bb));
    (* The Euclidean law again, on large operands. *)
    let q = Z.ediv za zb and r = Z.erem za zb in
    cmp "euclid law" (zs (Z.add (Z.mul zb q) r)) (zs za);
    check "diff r>=0" (Z.geq r Z.zero);
    check "diff r<|b|" (Z.lt r (Z.abs zb))
  done;
  (* Rationals: check canonicality directly rather than through to_string. *)
  for _ = 1 to 1000 do
    let mk () =
      let n = rand_decimal (1 + Random.int 12) and d = rand_decimal (1 + Random.int 12) in
      Q.make (Z.of_string n) (Z.of_string d)
    in
    let a = mk () and b = mk () in
    let canonical name q =
      check (name ^ " den>0") (Z.sign (Q.den q) > 0);
      check (name ^ " reduced") (Z.equal (Z.gcd (Z.abs (Q.num q)) (Q.den q)) Z.one);
      (* Re-canonicalising a canonical value must be the identity, structurally. *)
      check (name ^ " idempotent") (Q.make (Q.num q) (Q.den q) = q)
    in
    canonical "add" (Q.add a b);
    canonical "sub" (Q.sub a b);
    canonical "mul" (Q.mul a b);
    canonical "div" (Q.div a b);
    (* Cross-multiplication agrees with num on ordering. *)
    let cross =
      B.compare_big_int
        (B.mult_big_int (bi (zs (Q.num a))) (bi (zs (Q.den b))))
        (B.mult_big_int (bi (zs (Q.num b))) (bi (zs (Q.den a))))
    in
    check "Q compare vs num" (Q.compare a b = cross);
    (* Field laws that would expose a canonicalisation bug. *)
    check "Q a+b-b = a" (Q.sub (Q.add a b) b = a);
    check "Q a*b/b = a" (Q.div (Q.mul a b) b = a)
  done

(* Knuth's add-back correction — when the estimated quotient limb is one too
   large and the divisor has to be added back — fires on roughly 1 division in
   20000 at base 2^14. The random differential loop below performs ~1800 Knuth
   divisions and reaches it ZERO times, so it needs constructed inputs: these
   pairs were found by searching operand shapes against a model of the
   algorithm. Without them the most delicate branch in the file is dead code
   as far as the tests are concerned. *)
let addback_cases =
  [ "71591368595194289", "4398044526695";
    "71973959105605760", "4398042113605";
    "69374784055413227", "4398046412792";
    "71164774708095083", "4398045541372";
    "72039423572234367", "4398011215715";
    "64743642273007977", "4398046485655" ]

let test_addback () =
  List.iter (fun (sa, sb) ->
    let za = Z.of_string sa and zb = Z.of_string sb in
    let ba = B.big_int_of_string sa and bb = B.big_int_of_string sb in
    let name = "addback " ^ sa in
    checkeq (name ^ " q") ~expect:(B.string_of_big_int (B.div_big_int ba bb)) (zs (Z.ediv za zb));
    checkeq (name ^ " r") ~expect:(B.string_of_big_int (B.mod_big_int ba bb)) (zs (Z.erem za zb));
    (* and with the signs flipped every way, since the correction sits under
       the Euclidean adjustment *)
    List.iter (fun (fa, fb) ->
      let za = if fa then Z.neg za else za and zb = if fb then Z.neg zb else zb in
      let q = Z.ediv za zb and r = Z.erem za zb in
      check (name ^ " signed identity") (Z.equal (Z.add (Z.mul zb q) r) za);
      check (name ^ " signed r>=0") (Z.geq r Z.zero);
      check (name ^ " signed r<|b|") (Z.lt r (Z.abs zb)))
      [ (false, false); (true, false); (false, true); (true, true) ])
    addback_cases

(* [float_of_string] is correctly rounded (it is strtod), so the decimal
   spelling of an integer is exact ground truth for [Z.to_float] — a better
   oracle than another bignum library, which may round as we do. *)
let test_to_float_rounding () =
  Random.init 4242;
  let worst = ref 0 in
  for _ = 1 to 20000 do
    let s = rand_decimal (1 + Random.int 40) in
    let mine = Z.to_float (Z.of_string s) in
    let truth = float_of_string s in
    if mine <> truth then begin
      incr worst;
      if !worst <= 3 then begin
        incr fails;
        Printf.printf "FAIL to_float %s\n  got    %.17g\n  expect %.17g\n" s mine truth
      end
    end
  done;
  if !worst > 3 then Printf.printf "  (%d to_float mismatches in total)\n" !worst;
  (* Boundaries where naive accumulation drifts. *)
  List.iter (fun s -> checkeq ("to_float " ^ s) ~expect:(string_of_float (float_of_string s))
                        (string_of_float (Z.to_float (Z.of_string s))))
    [ "9007199254740993";    (* 2^53 + 1, first integer a double cannot hold *)
      "9007199254740992";    (* 2^53 *)
      "18014398509481983";   (* 2^54 - 1 *)
      "5496766494898856543463281";
      "-5496766494898856543463281" ]

(* Rationals a double cannot hold componentwise. Converting numerator and
   denominator separately gives nan (or 0.0 for subnormals); the true ratio is
   perfectly representable. *)
let test_q_to_float_range () =
  let big = String.concat "" (List.init 40 (fun _ -> "1234567890")) in
  let q = Q.make (Z.of_string (big ^ "0")) (Z.of_string big) in
  checkeq "huge n/d = 10" ~expect:"10." (string_of_float (Q.to_float q));
  let tiny = Q.make Z.one (Z.pow (Z.of_int 2) 1122) in
  check "subnormal denominator" (Q.to_float tiny = Float.ldexp 1.0 (-1122));
  check "overflows to infinity" (Q.to_float (Q.of_bigint (Z.pow (Z.of_int 10) 400)) = infinity);
  check "underflows to zero" (Q.to_float (Q.make Z.one (Z.pow (Z.of_int 10) 400)) = 0.0);
  (* of_float is exact, so to_float must recover every float it is given. *)
  Random.init 99;
  for _ = 1 to 5000 do
    (* Exponent range stays inside the double's, from subnormal up to ~2^999,
       so [of_float] is never handed an infinity. *)
    let f = Float.ldexp (Random.float 2.0 -. 1.0) (Random.int 2060 - 1060) in
    if f <> 0.0 then check "of_float/to_float roundtrip" (Q.to_float (Q.of_float f) = f)
  done;
  List.iter (fun (name, f) -> check ("roundtrip " ^ name) (Q.to_float (Q.of_float f) = f))
    [ "min_float", Float.min_float; "max_float", Float.max_float;
      "epsilon", Float.epsilon; "subnormal", Float.ldexp 1.0 (-1070) ]

(* A value-level oracle for Q: the invariant and law checks above would not
   catch an error that is systematic rather than structural, so compute the
   expected result independently with num and compare. *)
let test_q_differential () =
  Random.init 31337;
  let canon_str n d =
    let n, d = if B.sign_big_int d < 0 then (B.minus_big_int n, B.minus_big_int d) else (n, d) in
    if B.sign_big_int n = 0 then "0"
    else begin
      let g = B.gcd_big_int n d in
      let n = B.div_big_int n g and d = B.div_big_int d g in
      if B.eq_big_int d B.unit_big_int then B.string_of_big_int n
      else B.string_of_big_int n ^ "/" ^ B.string_of_big_int d
    end
  in
  for _ = 1 to 2000 do
    let pick () = (rand_decimal (1 + Random.int 15), rand_decimal (1 + Random.int 15)) in
    let na, da = pick () and nb, db = pick () in
    let a = Q.make (Z.of_string na) (Z.of_string da)
    and b = Q.make (Z.of_string nb) (Z.of_string db) in
    let ba = bi na and bda = bi da and bb = bi nb and bdb = bi db in
    let expect op =
      match op with
      | `Add -> canon_str (B.add_big_int (B.mult_big_int ba bdb) (B.mult_big_int bb bda)) (B.mult_big_int bda bdb)
      | `Sub -> canon_str (B.sub_big_int (B.mult_big_int ba bdb) (B.mult_big_int bb bda)) (B.mult_big_int bda bdb)
      | `Mul -> canon_str (B.mult_big_int ba bb) (B.mult_big_int bda bdb)
      | `Div -> canon_str (B.mult_big_int ba bdb) (B.mult_big_int bda bb)
    in
    checkeq "Q add vs num" ~expect:(expect `Add) (qs (Q.add a b));
    checkeq "Q sub vs num" ~expect:(expect `Sub) (qs (Q.sub a b));
    checkeq "Q mul vs num" ~expect:(expect `Mul) (qs (Q.mul a b));
    checkeq "Q div vs num" ~expect:(expect `Div) (qs (Q.div a b))
  done

let () =
  test_euclidean ();
  test_of_float ();
  test_canonical ();
  test_to_bigint ();
  test_bignum ();
  test_conversions ();
  test_addback ();
  test_to_float_rounding ();
  test_q_to_float_range ();
  test_differential ();
  test_q_differential ();
  if !fails = 0 then print_string "all zq tests passed\n"
  else (Printf.printf "%d FAILURES\n" !fails; exit 1)

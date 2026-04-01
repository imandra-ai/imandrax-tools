(* Old (canonical strings over num) vs new (canonical limbs) arithmetic.

   PLAN.md step 4 asks whether the representation swap is worth anything. The
   workloads below are shaped like what six_swiss actually does — small
   rationals compared and combined in a loop — plus a large-operand case to
   show where the two diverge most. *)

module Znew = Iml_prelude.Z
module Qnew = Iml_prelude.Q
module Zold = Zq_old.Z
module Qold = Zq_old.Q

let time name f =
  (* One untimed pass so allocation behaviour is warm and comparable. *)
  ignore (f ());
  let t0 = Unix.gettimeofday () in
  let r = f () in
  let dt = Unix.gettimeofday () -. t0 in
  Printf.printf "  %-28s %8.1f ms   (%s)\n%!" name (dt *. 1000.) r;
  dt

let report label old_t new_t =
  Printf.printf "  %-28s %8.2fx faster\n\n%!" (label ^ ":") (old_t /. new_t)

(* Order-book shaped: build small rationals, compare, accumulate. *)
let bench_small_q n =
  let old_f () =
    let acc = ref Qold.zero in
    for i = 1 to n do
      let p = Qold.of_ints (100 + (i mod 97)) (1 + (i mod 7)) in
      let q = Qold.of_ints (50 + (i mod 31)) 2 in
      if Qold.lt q p then acc := Qold.add !acc (Qold.mul p q) else acc := Qold.sub !acc q
    done;
    Qold.to_string (Qold.of_bigint (Qold.to_bigint !acc))
  in
  let new_f () =
    let acc = ref Qnew.zero in
    for i = 1 to n do
      let p = Qnew.of_ints (100 + (i mod 97)) (1 + (i mod 7)) in
      let q = Qnew.of_ints (50 + (i mod 31)) 2 in
      if Qnew.lt q p then acc := Qnew.add !acc (Qnew.mul p q) else acc := Qnew.sub !acc q
    done;
    Qnew.to_string (Qnew.of_bigint (Qnew.to_bigint !acc))
  in
  print_endline "small rationals (six_swiss shaped)";
  let o = time "old (strings over num)" old_f in
  let n' = time "new (canonical limbs)" new_f in
  report "speedup" o n'

(* Integer loop: the [Z.of_nativeint] literal path plus compare and add,
   which is what extracted IML spends most of its time on. *)
let bench_small_z n =
  let old_f () =
    let acc = ref Zold.zero in
    for i = 1 to n do
      let x = Zold.of_nativeint (Nativeint.of_int (i land 0xffff)) in
      acc := if Zold.lt !acc x then Zold.add !acc x else Zold.sub !acc (Zold.of_nativeint 1n)
    done;
    Zold.to_string !acc
  in
  let new_f () =
    let acc = ref Znew.zero in
    for i = 1 to n do
      let x = Znew.of_nativeint (Nativeint.of_int (i land 0xffff)) in
      acc := if Znew.lt !acc x then Znew.add !acc x else Znew.sub !acc (Znew.of_nativeint 1n)
    done;
    Znew.to_string !acc
  in
  print_endline "small integers (literals, compare, add)";
  let o = time "old (strings over num)" old_f in
  let n' = time "new (canonical limbs)" new_f in
  report "speedup" o n'

(* Large operands: many limbs, so the per-operation decimal round-trip the old
   representation paid is at its most expensive. *)
let bench_big_z n =
  let s = String.concat "" (List.init 20 (fun _ -> "123456789012345")) in
  let old_f () =
    let a = Zold.of_string s and b = Zold.of_string "982451653" in
    let acc = ref Zold.zero in
    for _ = 1 to n do
      acc := Zold.add !acc (Zold.erem (Zold.mul a a) b)
    done;
    Zold.to_string !acc
  in
  let new_f () =
    let a = Znew.of_string s and b = Znew.of_string "982451653" in
    let acc = ref Znew.zero in
    for _ = 1 to n do
      acc := Znew.add !acc (Znew.erem (Znew.mul a a) b)
    done;
    Znew.to_string !acc
  in
  print_endline "large integers (300 digits, mul + mod)";
  let o = time "old (strings over num)" old_f in
  let n' = time "new (canonical limbs)" new_f in
  report "speedup" o n'

let () =
  bench_small_q 200_000;
  bench_small_z 200_000;
  bench_big_z 2_000

(* Shared module aliases, utilities, and definitions for all parse modules *)

(* Module aliases *)
module Artifact = Imandrax_api_artifact.Artifact
module Uid = Imandrax_api__Uid
module Ty_view = Imandrax_api.Ty_view
module Mir = Imandrax_api_mir
module Type = Imandrax_api_mir.Type
module Term = Imandrax_api_mir.Term
module Decl = Imandrax_api_mir.Decl
module Applied_symbol = Imandrax_api_common.Applied_symbol
module Region = Imandrax_api_mir.Region

module Sir = Semantic_ir

(* Utils
==================== *)

exception Early_return of string

let show_term_view : (Term.term, Type.t) Term.view -> string =
  Term.show_view Term.pp Type.pp

let unzip3 triples =
  List.fold_right
    (fun (x, y, z) (xs, ys, zs) -> (x :: xs, y :: ys, z :: zs))
    triples ([], [], [])

let zip3 xs ys zs =
  List.map2 (fun x (y, z) -> (x, y, z)) xs (List.combine ys zs)

let zip5 l1 l2 l3 l4 l5 =
  List.combine l1 l2 |> List.combine l3 |> List.combine l4 |> List.combine l5
  |> List.map (fun (e, (d, (c, (a, b)))) -> (a, b, c, d, e))

let zip6 l1 l2 l3 l4 l5 l6 =
  List.combine l1 l2 |> List.combine l3 |> List.combine l4 |> List.combine l5 |> List.combine l6
  |> List.map (fun (f, (e, (d, (c, (a, b))))) -> (a, b, c, d, e, f))

(* [x, y, z] -> ([x; y], z) *)
let split_last (xs : 'a list) : 'a list * 'a =
  match List.rev xs with
  | [] -> failwith "Never: empty list"
  | x :: xs -> (List.rev xs, x)

let unwrap : ('a, 'b) result -> 'a = function
  | Ok x -> x
  | Error msg -> failwith msg

(** Create a list of anonymous argument names

Example:
  anonymous_arg_names 3 = ["arg0"; "arg1"; "arg2"]
*)
let anonymous_arg_names (i : int) : string list =
  List.init i (fun i -> "arg" ^ string_of_int i)

let%expect_test "anonymous_arg_names" =
  let names = anonymous_arg_names 3 in
  List.iter (Printf.printf "%s ") names;
  [%expect {| arg0 arg1 arg2 |}]

(* Convert 8-bit bool list to a char *)
let char_of_bools (bools : bool list) : char =
  if List.length bools <> 8 then
    invalid_arg "bools_to_char: list must contain exactly 8 booleans"
  else
    let rec bools_to_int acc = function
      | [] -> acc
      | b :: rest ->
          let bit = if b then 1 else 0 in
          bools_to_int ((acc lsl 1) lor bit) rest
    in
    let ascii_value = bools_to_int 0 bools in
    Char.chr ascii_value

(* Convert a char to a list of bools *)
let bools_of_char (c : char) : bool list =
  let ascii_value = Char.code c in
  let rec int_to_bools acc n bit_pos =
    if bit_pos < 0 then acc
    else
      let bit = (n lsr bit_pos) land 1 in
      int_to_bools ((bit = 1) :: acc) n (bit_pos - 1)
  in
  int_to_bools [] ascii_value 7

let%expect_test "bool list expr to string" =
  let bools = [ false; true; false; false; false; false; false; true ] in
  let c = char_of_bools bools in
  Printf.printf "%c\n" c;
  [%expect {| A |}]

let%expect_test "char to bools" =
  let c = '0' in
  let bools = bools_of_char c in
  List.iter (Printf.printf "%b ") bools;
  [%expect {| false false false false true true false false |}]

(* Mir gymnastics
==================== *)

(** Flatten the arrow type view to a list of types *)
let unpack_arrows (ty_view : (unit, Uid.t, Type.t) Ty_view.view) : string list =
  let rec helper
      (types : string list)
      (ty_view : (unit, Uid.t, Type.t) Ty_view.view) : string list =
    match ty_view with
    | Ty_view.Arrow (_, left_t, right_t) ->
        let left_type =
          match left_t.view with
          | Ty_view.Constr (constr_name_uid, _empty_constr_args) ->
              constr_name_uid.name
          | _ -> failwith "Never: left of arrow type view should be a constr"
        in
        helper (types @ [ left_type ]) right_t.view
    | Ty_view.Constr (constr_name_uid, _empty_constr_args) ->
        let final_type = constr_name_uid.name in
        List.append types [ final_type ]
    | _ ->
        failwith "Never: arrow type view should be either a constr or an arrow"
  in
  helper [] ty_view

(* SIR
==================== *)

(** Parse Constr variant of MIR Ty_view.view to Semantic IR type_expr

Arg:
  ty_view: Constr variant of Mir Ty_view.view
Return: tuple of:
  0: Semantic IR type expression
  1: generic type parameters used (as strings, not UIDs)
*)
let type_expr_of_mir_ty_view_constr (ty_view : (unit, Uid.t, Type.t) Ty_view.view)
    : Sir.type_expr * string list =
  let rec helper
      (ty_view : (unit, Uid.t, Type.t) Ty_view.view)
      (params_acc : string list) : Sir.type_expr * string list =
    match ty_view with
    | Constr ((constr_uid : Uid.t), (constr_args : Type.t list)) -> (
        let constr_name = constr_uid.name in
        match constr_args with
        | [] -> (Sir.TBase constr_name, params_acc)
        | _ ->
            let ( (arg_exprs : Sir.type_expr list),
                  (params_acc_by_arg : string list list) ) =
              constr_args
              |> List.map (fun (ty : Type.t) -> helper ty.view [])
              |> List.split
            in
            ( Sir.TApp (constr_name, arg_exprs),
              params_acc @ (params_acc_by_arg |> List.flatten) ))
    | Var (var_uid : Uid.t) ->
        let type_var_name = var_uid.name in
        (Sir.TVar type_var_name, type_var_name :: params_acc)
    | _ -> failwith "parse_constr_to_semantic_type: expected Constr or Var"
  in

  let ty_expr, params_acc = helper ty_view [] in
  let dedup_params = params_acc |> CCList.uniq ~eq:String.equal in
  (ty_expr, dedup_params)

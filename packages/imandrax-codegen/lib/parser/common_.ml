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

(* [x, y, z] -> ([x; y], z) *)
let split_last (xs : 'a list) : 'a list * 'a =
  match List.rev xs with
  | [] -> failwith "Never: empty list"
  | x :: xs -> (List.rev xs, x)

let unwrap : ('a, 'b) result -> 'a = function
  | Ok x -> x
  | Error msg -> failwith msg

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

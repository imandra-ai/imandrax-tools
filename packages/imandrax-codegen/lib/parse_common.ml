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
module Ast = Ast

(* Shared exception *)
exception Early_return of string

(* Utility functions *)
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

let unwrap : ('a, 'b) result -> 'a = function
  | Ok x -> x
  | Error msg -> failwith msg

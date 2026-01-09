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

(** Parse Constr variant of Ty_view.view to type annotation

Return:
  0: a chained sequence of applied types
  1: generic type parameters used
*)
let parse_constr_to_type_annot_ (ty_view : (unit, Uid.t, Type.t) Ty_view.view) :
    string list * Uid.t list =
  let rec helper
      (ty_view : (unit, Uid.t, Type.t) Ty_view.view)
      (ty_acc : string list)
      (params_acc : Uid.t list) : string list * Uid.t list =
    match ty_view with
    | Constr ((constr_uid : Uid.t), (constr_args : Type.t list)) -> (
        let constr_name = constr_uid.name in
        match constr_args with
        | [] -> (constr_name :: ty_acc, params_acc)
        | [ next_ty ] ->
            let next_view = next_ty.view in
            helper next_view (constr_name :: ty_acc) params_acc
        | _next_ty :: _next_tys ->
            List.iter (fun t -> print_endline (Type.show t)) constr_args;
            failwith
              "Never(parse_constr_to_type_annot): expected Constr with 0 or 1 \
               args")
    | Var (var_uid : Uid.t) ->
        let type_var_name = var_uid.name in
        (type_var_name :: ty_acc, var_uid :: params_acc)
    | _ -> failwith "parse_constr_to_type_annot: expected Constr or Var"
  in

  let ty_acc, params_acc = helper ty_view [] [] in
  (* Replace with python names *)
  let ty_acc_py =
    ty_acc
    |> List.map (fun (caml_type : string) : string ->
           CCList.assoc_opt ~eq:String.equal caml_type
             Ast.ty_view_constr_name_mapping
           |> Option.value ~default:caml_type)
  in
  (List.rev ty_acc_py, params_acc)

type type_expr = Ast.type_expr

(** Parse Constr variant of Ty_view.view to type annotation

Return:
  0: a chained sequence of applied types
  1: generic type parameters used
*)
let parse_constr_to_type_annot (ty_view : (unit, Uid.t, Type.t) Ty_view.view) :
    type_expr * Uid.t list =
  let rec helper
      (ty_view : (unit, Uid.t, Type.t) Ty_view.view)
      (params_acc : Uid.t list) : type_expr * Uid.t list =
    match ty_view with
    | Constr ((constr_uid : Uid.t), (constr_args : Type.t list)) -> (
        let constr_name = constr_uid.name in
        match constr_args with
        | [] -> (Base constr_name, params_acc)
        | _ ->
            let ( (arg_exprs : type_expr list),
                  (params_acc_by_arg : Uid.t list list) ) =
              constr_args
              |> List.map (fun (ty : Type.t) -> helper ty.view [])
              |> List.split
            in
            ( Generic (constr_name, arg_exprs),
              params_acc @ (params_acc_by_arg |> List.flatten) ))
    | Var (var_uid : Uid.t) ->
        let type_var_name = var_uid.name in
        (Base type_var_name, var_uid :: params_acc)
    | _ -> failwith "parse_constr_to_type_annot: expected Constr or Var"
  in

  let ty_acc, params_acc = helper ty_view [] in
  (* Replace with python names *)
  let ty_acc_py =
    Ast.map_type_expr ty_acc ~f:(fun (caml_type : string) : string ->
        CCList.assoc_opt ~eq:String.equal caml_type
          Ast.ty_view_constr_name_mapping
        |> Option.value ~default:caml_type)
  in

  let dedup_params = params_acc |> CCList.uniq ~eq:Uid.equal in
  (ty_acc_py, dedup_params)

(** Define type variable

Example:
  - 'a/92728' -> `a = TypeVar('a')`
*)
let type_var_def_of_uid (uid : Uid.t) : Ast.stmt =
  let name = uid.name in
  Assign
    {
      targets = [ Ast.mk_name_expr name ];
      value =
        Ast.Call
          {
            func = Ast.mk_name_expr "TypeVar";
            args = [ Ast.Constant { value = String name; kind = None } ];
            keywords = [];
          };
      type_comment = None;
    }

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

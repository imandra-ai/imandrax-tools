(** Parser for transforming MIR Model to semantic IR *)

let parse_term = Term.parse_term

open Common_
open Printf

(* Model |-> applied symbol and term *)
let unpack_model (model : (Term.term, Type.t) Imandrax_api_common.Model.t_poly)
    : Type.t Applied_symbol.t_poly * Term.term =
  match model.Mir.Model.consts with
  | [] -> failwith "No constants\n"
  | [ const ] ->
      let app_sym, term = const in
      app_sym, term
  | _ ->
      let s =
        sprintf
          "more than 1 const, not supported\n len = %d"
          (List.length model.Mir.Model.consts)
      in
      failwith s
;;

let parse_model (model : Mir.Model.t) : Sir.Value_assignment.t =
  let (app_sym : Type.t Applied_symbol.t_poly), term = unpack_model model in
  (* We use the type annotation derived in the applied symbol instead of
  in the term because in the term, the type variable information is lost.
  A example will be `let w = []`
  *)
  let type_annot, _type_vars =
    type_expr_of_mir_ty_view_constr app_sym.ty.view
  in
  let (_type_annot : Sir.type_expr), (sir_term_expr : Sir.value) =
    match parse_term term with
    | Ok (type_annot, term_expr) -> type_annot, term_expr
    | Error msg -> failwith msg
  in
  let var_name = app_sym.sym.id.name in
  { var_name; ty = type_annot; tm = sir_term_expr }
;;

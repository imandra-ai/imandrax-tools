open Printf
open Parse_common

let parse_term = Parse_term.parse_term

(* Model |-> applied symbol and term *)
let unpack_model (model : (Term.term, Type.t) Imandrax_api_common.Model.t_poly)
    : Type.t Applied_symbol.t_poly * Term.term =
  match model.Mir.Model.consts with
  | [] -> failwith "No constants\n"
  | [ const ] ->
      let app_sym, term = const in
      (app_sym, term)
  | _ ->
      let s =
        sprintf "more than 1 const, not supported\n len = %d"
          (List.length model.Mir.Model.consts)
      in
      failwith s

(*
A model consists of an applied symbol and a term
- term can be extracted to
  - type definition statements
  - a value expression
  - a type annotation for the value expression
*)
let parse_model (model : (Term.term, Type.t) Imandrax_api_common.Model.t_poly) :
    Ast.stmt list =
  let (app_sym : Type.t Applied_symbol.t_poly), term = unpack_model model in
  let ty_defs, type_annot, term_expr =
    match parse_term term with
    | Ok (ty_defs, type_annot, term_expr) -> (ty_defs, type_annot, term_expr)
    | Error msg -> failwith msg
  in

  let assign_stmt =
    let target = app_sym.sym.id.name in
    match type_annot with
    | None ->
        Ast.Assign
          {
            targets = [ Ast.Name { Ast.id = target; ctx = Ast.Load } ];
            value = term_expr;
            type_comment = None;
          }
    | Some type_annot ->
        Ast.AnnAssign
          {
            target = Ast.Name { Ast.id = target; ctx = Ast.Load };
            annotation = type_annot;
            value = Some term_expr;
            simple = 1;
          }
  in
  List.append ty_defs [ assign_stmt ]

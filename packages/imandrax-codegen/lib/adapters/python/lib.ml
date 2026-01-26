(** Top-level APIs *)

open Printf
module Artifact = Imandrax_api_artifact.Artifact
module Uid = Imandrax_api__Uid
module Ty_view = Imandrax_api.Ty_view
module Mir = Imandrax_api_mir
module Type = Imandrax_api_mir.Type
module Term = Imandrax_api_mir.Term
module Decl = Imandrax_api_mir.Decl
module Applied_symbol = Imandrax_api_common.Applied_symbol
module Region = Imandrax_api_mir.Region
module Sir = Semantic_ir.Types

(* TODO: this will be removed *)
module Parser = Parser

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

(** Parse a MIR model into an AST assign statement

```py
target_var: TargetType = target_value
```
*)
let parse_model (model : (Term.term, Type.t) Imandrax_api_common.Model.t_poly) :
    Ast.stmt =
  let (app_sym : Type.t Applied_symbol.t_poly), term = unpack_model model in
  let (sir_type_annot : Sir.type_expr option), (sir_term_expr : Sir.value) =
    match Parser.Term.parse_term term with
    | Ok (type_annot, term_expr) -> (type_annot, term_expr)
    | Error msg -> failwith msg
  in

  let type_annot : Ast.expr option =
    sir_type_annot |> Option.map Transform.annot_of_sir_type_expr
  in
  let term_expr : Ast.expr = sir_term_expr |> Transform.ast_expr_of_sir_value in
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
  assign_stmt

(** Parse a MIR Decl.t to corresponding AST statments for type declaration *)
let parse_decl (decl : (Term.t, Type.t) Decl.t_poly) :
    (Ast.stmt list, string) result =
  match Parser.Decl.parse_decl decl with
  | Ok sir_type_decl ->
      let stmts = Transform.stmts_of_sir_type_decl sir_type_decl in
      Ok stmts
  | Error msg -> Error msg

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
module Sir = Semantic_ir

(** Parse a MIR model into an AST assign statement

```py
a = TypeVar('a')  # used in TargetType

target_var: TargetType = target_value
```
*)
let parse_model (model : (Term.term, Type.t) Imandrax_api_common.Model.t_poly)
    : Ast.stmt list =
  let value_assign = Sir.Parser.Model.parse_model model in
  Transform.ast_stmts_of_sir_value_assignment value_assign
;;

(** Parse a MIR Decl.t to corresponding AST statments for type declaration *)
let parse_decl (decl : (Term.t, Type.t) Decl.t_poly)
    : (Ast.stmt list, string) result =
  match Sir.Parser.Decl.parse_decl decl with
  | Ok sir_type_decl ->
      let stmts = Transform.stmts_of_sir_type_decl sir_type_decl in
      Ok stmts
  | Error msg -> Error msg
;;

(** Parse a MIR Fun_decomp.t to corresponding AST statments for test definitions *)
let parse_fun_decomp
    (test_format : [< `Dict | `Function ])
    (fun_decomp : Mir.Fun_decomp.t)
    : Ast.stmt list =
  let (test_suite : Sir.test_suite) =
    Sir.Parser.Fun_decomp.parse_fun_decomp fun_decomp
  in
  match test_format with
  | `Function -> test_suite |> List.map Transform.test_func_def_of_test_decl
  | `Dict -> [ Transform.test_data_dict_of_test_suite test_suite ]
;;

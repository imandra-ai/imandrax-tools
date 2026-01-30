(** TypeScript adapter: SIR to TypeScript source code *)

module Mir = Imandrax_api_mir
module Type = Imandrax_api_mir.Type
module Term = Imandrax_api_mir.Term
module Decl = Imandrax_api_mir.Decl
module Sir = Semantic_ir
module Extra_imports = Config.Extra_imports

(** Parse a MIR model into TypeScript const declaration

    ```ts
    const target_var: TargetType = target_value;
    ```
*)
let parse_model (model : (Term.term, Type.t) Imandrax_api_common.Model.t_poly)
    : string * Extra_imports.t =
  let value_assign = Sir.Parser.Model.parse_model model in
  Emit.emit_value_assignment value_assign
;;

(** Parse a MIR Decl.t to corresponding TypeScript type declaration *)
let parse_decl (decl : (Term.t, Type.t) Decl.t_poly)
    : (string * Extra_imports.t, string) result =
  match Sir.Parser.Decl.parse_decl decl with
  | Ok sir_type_decl -> Ok (Emit.emit_type_decl sir_type_decl)
  | Error msg -> Error msg
;;

(** Parse a MIR Fun_decomp.t to corresponding TypeScript test data object *)
let parse_fun_decomp (fun_decomp : Mir.Fun_decomp.t) : string * Extra_imports.t =
  let test_suite = Sir.Parser.Fun_decomp.parse_fun_decomp fun_decomp in
  Emit.emit_test_suite_dict test_suite
;;

module Type = Imandrax_api_mir.Type
module Term = Imandrax_api_mir.Term
module Decl = Imandrax_api_mir.Decl
module Mir = Imandrax_api_mir

(** Parse a MIR model into an AST assign statement *)
val parse_model
  : (Term.term, Type.t) Imandrax_api_common.Model.t_poly -> Ast.stmt list

(** Parse a MIR Decl.t to corresponding AST statments for type declaration *)
val parse_decl : (Term.t, Type.t) Decl.t_poly -> (Ast.stmt list, string) result

(** Parse a MIR Fun_decomp.t to corresponding AST statments for test definitions *)
val parse_fun_decomp
  : [< `Dict | `Function ] -> Mir.Fun_decomp.t -> Ast.stmt list

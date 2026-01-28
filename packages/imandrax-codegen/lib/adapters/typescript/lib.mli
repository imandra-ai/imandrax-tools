(** TypeScript adapter: SIR to TypeScript source code *)

module Type = Imandrax_api_mir.Type
module Term = Imandrax_api_mir.Term
module Decl = Imandrax_api_mir.Decl
module Mir = Imandrax_api_mir

(** Parse a MIR model into TypeScript const declaration *)
val parse_model
  :  (Term.term, Type.t) Imandrax_api_common.Model.t_poly
  -> string

(** Parse a MIR Decl.t to corresponding TypeScript type declaration *)
val parse_decl : (Term.t, Type.t) Decl.t_poly -> (string, string) result

(** Parse a MIR Fun_decomp.t to corresponding TypeScript test definitions *)
val parse_fun_decomp : [< `Dict | `Function ] -> Mir.Fun_decomp.t -> string

(** TypeScript adapter: SIR to TypeScript source code *)

module Type = Imandrax_api_mir.Type
module Term = Imandrax_api_mir.Term
module Decl = Imandrax_api_mir.Decl
module Mir = Imandrax_api_mir
module Extra_imports = Config.Extra_imports

(** Parse a MIR model into TypeScript const declaration *)
val parse_model
  :  (Term.term, Type.t) Imandrax_api_common.Model.t_poly
  -> string * Extra_imports.t

(** Parse a MIR Decl.t to corresponding TypeScript type declaration *)
val parse_decl : (Term.t, Type.t) Decl.t_poly -> (string * Extra_imports.t, string) result

(** Parse a MIR Fun_decomp.t to corresponding TypeScript test data object *)
val parse_fun_decomp : Mir.Fun_decomp.t -> string * Extra_imports.t

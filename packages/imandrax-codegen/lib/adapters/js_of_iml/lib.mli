(** js_of_iml adapter: SIR to OCaml JSON codecs + Python bindings *)

module Sir = Semantic_ir
module Mir = Imandrax_api_mir

(** Parse a MIR type declaration into OCaml JSON codec source code *)
val parse_decl
  : (Mir.Term.t, Mir.Type.t) Mir.Decl.t_poly
  -> (Sir.type_decl * string, string) result

(** Emit OCaml JSON codec for a SIR type declaration *)
val emit_ocaml_codec : Sir.type_decl -> string

(** Emit Python type definition for a SIR type declaration *)
val emit_python_type : Sir.type_decl -> string

(** Emit Python _to_dict function for a SIR type declaration *)
val emit_python_to_dict : Sir.type_decl -> string

(** Emit complete OCaml wrapper module.
    Returns: (ocaml_codecs, ocaml_wrapper_main) *)
val emit_ocaml_wrapper
  :  type_decls:Sir.type_decl list
  -> module_name:string
  -> func_name:string
  -> args:(string * Sir.type_expr) list
  -> ret_type:Sir.type_expr
  -> string * string

(** Emit complete Python binding module *)
val emit_python_module
  :  type_decls:Sir.type_decl list
  -> func_name:string
  -> args:(string * Sir.type_expr) list
  -> ret_type:Sir.type_expr
  -> js_wrapper_path:string
  -> string

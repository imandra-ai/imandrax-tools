(** js_of_iml adapter: SIR to OCaml JSON codecs + Python bindings *)

module Mir = Imandrax_api_mir
module Type = Imandrax_api_mir.Type
module Term = Imandrax_api_mir.Term
module Decl = Imandrax_api_mir.Decl
module Sir = Semantic_ir

(** Parse a MIR type declaration to SIR, then emit OCaml JSON codec *)
let parse_decl (decl : (Term.t, Type.t) Decl.t_poly)
    : (Sir.type_decl * string, string) result =
  match Sir.Parser.Decl.parse_decl decl with
  | Ok sir_type_decl ->
    Ok (sir_type_decl, Emit_ocaml.emit_type_codec sir_type_decl)
  | Error msg -> Error msg
;;

let emit_ocaml_codec = Emit_ocaml.emit_type_codec
let emit_python_type = Emit_python.emit_type_def
let emit_python_to_dict = Emit_python.emit_to_dict_func

let emit_ocaml_wrapper ~type_decls ~module_name ~func_name ~args ~ret_type =
  let codecs = Emit_ocaml.emit_type_codecs type_decls in
  let wrapper_main =
    Emit_ocaml.emit_wrapper_main ~module_name ~func_name ~args ~ret_type
  in
  (codecs, wrapper_main)
;;

let emit_python_module ~type_decls ~func_name ~args ~ret_type ~js_wrapper_path =
  Emit_python.emit_module ~type_decls ~func_name ~args ~ret_type ~js_wrapper_path
;;

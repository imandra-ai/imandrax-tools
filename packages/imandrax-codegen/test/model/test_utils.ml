open Printf
open Imandrax_codegen
open Imandrax_codegen.Parse

module Mir = Imandrax_api_mir
module Type = Imandrax_api_mir.Type
module Term = Imandrax_api_mir.Term
module Model = Imandrax_api_mir.Model

(* Load a Mir.model from a yaml file *)
let load_artifact (sub_dir : string option) (name : string) : Model.t =
  let yaml_str =
    match sub_dir with
    | None -> CCIO.File.read_exn (sprintf "../data/model/%s.yaml" name)
    | Some sub_dir ->
      CCIO.File.read_exn (sprintf "../data/model/%s/%s.yaml" sub_dir name)
  in
  let yaml = Yaml.of_string_exn yaml_str in
  let name, iml_code =
    match yaml with
    | `O assoc ->
      let name =
        match List.assoc_opt "name" assoc with
        | Some (`String name) -> name
        | _ -> failwith "invalid yaml"
      in
      let iml_code =
        match List.assoc_opt "iml" assoc with
        | Some (`String s) -> s
        | _ -> failwith "invalid yaml"
      in
      name, iml_code
    | _ -> failwith "invalid yaml"
  in
  printf "name: %s\n" name;
  printf "iml_code:\n%s\n\n" iml_code;

  Art_utils.yaml_to_model yaml

let test_parse_model (sub_dir : string option) (name : string) : unit =
  let model = load_artifact sub_dir name in
  let _app_sym, term = unpack_model model in
  let type_annot, expr =
    match parse_term term with
    | Ok (type_annot, expr) -> type_annot, expr
    | Error msg -> failwith msg
  in

  printf "Type annot:\n";
  (match type_annot with
  | None -> print_endline "None"
  | Some type_annot -> print_endline (Ast.show_expr type_annot));
  printf "\n";

  printf "Expr:\n";
  print_endline (Ast.show_expr expr);
  ()

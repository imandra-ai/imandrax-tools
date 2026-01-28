open Printf
open Imandrax_codegen

module Mir = Imandrax_api_mir
module Type = Imandrax_api_mir.Type
module Term = Imandrax_api_mir.Term
module Model = Imandrax_api_mir.Model
module Sir = Semantic_ir


let workspace_dir = "../../.."

let art_data_dir =
  let art_data_dir_rel_to_workspace = Sys.getenv "TEST_DATA_ART_DIR" in
  workspace_dir ^ "/" ^ art_data_dir_rel_to_workspace

let art_data_model_dir = art_data_dir ^ "/model"

(* Load a Mir.model from a yaml file *)
let load_artifact (sub_dir : string) (name : string) : (string * string * Model.t) =
  let yaml_str =
    if sub_dir |> CCString.is_empty then
      CCIO.File.read_exn (sprintf "%s/%s.yaml" art_data_model_dir name)
      else
      CCIO.File.read_exn (sprintf "%s/%s/%s.yaml" art_data_model_dir sub_dir name)
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

  (name, iml_code, Art_utils.yaml_to_model yaml )

let parse_model (model : Model.t) : Sir.Value_assignment.t =
  Sir.Parser.Model.parse_model model

(** Generate SIR S-expressions from YAML artifacts.

    Reads YAML artifacts from TEST_DATA_ART_DIR/{model,decl,fun_decomp}/
    and writes SIR S-expressions to TEST_DATA_SIR_DIR/{model,decl,fun_decomp}/.

    Usage: TEST_DATA_ART_DIR=test/data/art TEST_DATA_SIR_DIR=test/data/sir dune exec gen_sir.exe *)

module Codegen = Imandrax_codegen
module Sir = Semantic_ir

let art_dir = Sys.getenv "TEST_DATA_ART_DIR"
let sir_dir = Sys.getenv "TEST_DATA_SIR_DIR"

(** Navigate nested YAML by key path (supports object keys and array indices) *)
let yaml_get_path yaml path =
  List.fold_left (fun acc key ->
    match acc with
    | `O assoc -> List.assoc key assoc
    | `A arr -> List.nth arr (int_of_string key)
    | _ -> failwith ("Cannot index: " ^ key)
  ) yaml path

(** Mode name -> YAML to SIR S-expression converter (None = skip file) *)
let (modes : (string * (Yaml.value -> string option)) list) =
  [ "model", (fun yaml ->
      let model = Codegen.Art_utils.yaml_to_model yaml in
      let sir = Sir.Parser.Model.parse_model model in
      Some (Sexplib.Sexp.to_string_hum (Sir.Value_assignment.sexp_of_t sir)))
  ; "decl", (fun yaml ->
      let artifact = yaml_get_path yaml ["get_decls_res"; "decls"; "0"; "artifact"] in
      let decl = Codegen.Art_utils.yaml_to_decl artifact in
      match Sir.Parser.Decl.parse_decl decl with
      | Ok sir -> Some (Sexplib.Sexp.to_string_hum (Sir.sexp_of_type_decl sir))
      | Error _ -> None)
  ; "fun_decomp", (fun yaml ->
      let artifact = yaml_get_path yaml ["decomp_res"; "artifact"] in
      let fd = Codegen.Art_utils.yaml_to_fun_decomp artifact in
      let sir = Sir.Parser.Fun_decomp.parse_fun_decomp fd in
      Some (Sexplib.Sexp.to_string_hum (Sir.sexp_of_test_suite sir)))
  ]

let rec ensure_dir path =
  if not (Sys.file_exists path) then (ensure_dir (Filename.dirname path); Sys.mkdir path 0o755)

let rec find_files ext dir =
  if Sys.is_directory dir then
    Sys.readdir dir |> Array.to_list |> List.concat_map (fun e -> find_files ext (Filename.concat dir e))
  else if Filename.check_suffix dir ext then [dir]
  else []

let process_mode (mode, convert) =
  let src = Filename.concat art_dir mode in
  let dst = Filename.concat sir_dir mode in
  if not (Sys.file_exists src) then 0
  else
    find_files ".yaml" src |> List.filter_map (fun path ->
      try
        let yaml = CCIO.File.read_exn path |> Yaml.of_string_exn in
        match convert yaml with
        | Some sexp ->
            let out = Filename.concat dst (String.sub path (String.length src + 1)
              (String.length path - String.length src - 6) ^ ".sexp") in
            ensure_dir (Filename.dirname out);
            CCIO.File.write_exn out sexp;
            Some ()
        | None -> None
      with _ -> None
    ) |> List.length

let () =
  let total = List.fold_left (fun acc m -> acc + process_mode m) 0 modes in
  Printf.printf "Generated %d SIR files\n" total

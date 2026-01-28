(** Generate SIR S-expressions from YAML artifacts *)

open Printf
module Codegen = Imandrax_codegen
module Sir = Semantic_ir

(* Directory configuration *)
let art_dir = Sys.getenv "TEST_DATA_ART_DIR"
let sir_dir = Sys.getenv "TEST_DATA_SIR_DIR"

(* Supported modes *)
type mode =
  | Model
  | Decl
  | FunDecomp

let string_of_mode = function
  | Model -> "model"
  | Decl -> "decl"
  | FunDecomp -> "fun_decomp"

(* Load YAML file and parse to corresponding MIR type *)
let load_yaml_file (path : string) : Yaml.value =
  let yaml_str = CCIO.File.read_exn path in
  Yaml.of_string_exn yaml_str

(* Helper to extract nested YAML value by path *)
let yaml_get_path (yaml : Yaml.value) (path : string list) : Yaml.value =
  List.fold_left
    (fun acc key ->
      match acc with
      | `O assoc -> (
          match List.assoc_opt key assoc with
          | Some v -> v
          | None -> failwith (sprintf "Missing key: %s" key))
      | `A arr -> (
          let idx = int_of_string key in
          match List.nth_opt arr idx with
          | Some v -> v
          | None -> failwith (sprintf "Index out of bounds: %d" idx))
      | _ -> failwith (sprintf "Cannot index into non-object/array with key: %s" key))
    yaml path

(* Generate SIR S-expression string for model *)
let gen_sir_model (yaml : Yaml.value) : string =
  let model = Codegen.Art_utils.yaml_to_model yaml in
  let sir = Sir.Parser.Model.parse_model model in
  let sexp = Sir.Value_assignment.sexp_of_t sir in
  Sexplib.Sexp.to_string_hum sexp

(* Generate SIR S-expression string for decl *)
(* Decl YAML structure: .get_decls_res.decls[0].artifact *)
let gen_sir_decl (yaml : Yaml.value) : string option =
  let artifact_yaml = yaml_get_path yaml ["get_decls_res"; "decls"; "0"; "artifact"] in
  let decl = Codegen.Art_utils.yaml_to_decl artifact_yaml in
  match Sir.Parser.Decl.parse_decl decl with
  | Ok sir ->
      let sexp = Sir.sexp_of_type_decl sir in
      Some (Sexplib.Sexp.to_string_hum sexp)
  | Error msg ->
      eprintf "Warning: could not parse decl: %s\n" msg;
      None

(* Generate SIR S-expression string for fun_decomp *)
(* Fun_decomp YAML structure: .decomp_res.artifact *)
let gen_sir_fun_decomp (yaml : Yaml.value) : string =
  let artifact_yaml = yaml_get_path yaml ["decomp_res"; "artifact"] in
  let fun_decomp = Codegen.Art_utils.yaml_to_fun_decomp artifact_yaml in
  let sir = Sir.Parser.Fun_decomp.parse_fun_decomp fun_decomp in
  let sexp = Sir.sexp_of_test_suite sir in
  Sexplib.Sexp.to_string_hum sexp

(* Find all YAML files in a directory recursively *)
let find_yaml_files (dir : string) : string list =
  let rec walk acc path =
    if Sys.is_directory path then
      let entries = Sys.readdir path |> Array.to_list in
      List.fold_left
        (fun acc entry -> walk acc (Filename.concat path entry))
        acc entries
    else if Filename.check_suffix path ".yaml" || Filename.check_suffix path ".yml"
    then path :: acc
    else acc
  in
  walk [] dir |> List.sort String.compare

(* Compute relative path from source dir *)
let relative_path ~(base : string) (path : string) : string =
  let base_len = String.length base in
  if String.length path > base_len && String.sub path 0 base_len = base then
    let rest = String.sub path (base_len + 1) (String.length path - base_len - 1) in
    rest
  else
    path

(* Ensure directory exists *)
let rec ensure_dir path =
  if not (Sys.file_exists path) then begin
    ensure_dir (Filename.dirname path);
    Sys.mkdir path 0o755
  end

(* Process a single YAML file and generate SIR *)
let process_file (mode : mode) (src_path : string) (dst_path : string) : bool =
  try
    let yaml = load_yaml_file src_path in
    let sexp_opt =
      match mode with
      | Model -> Some (gen_sir_model yaml)
      | Decl -> gen_sir_decl yaml
      | FunDecomp -> Some (gen_sir_fun_decomp yaml)
    in
    match sexp_opt with
    | Some sexp_str ->
        ensure_dir (Filename.dirname dst_path);
        CCIO.File.write_exn dst_path sexp_str;
        true
    | None -> false
  with e ->
    eprintf "Error processing %s: %s\n" src_path (Printexc.to_string e);
    false

(* Process all files for a given mode *)
let process_mode (mode : mode) : int * int =
  let src_dir = Filename.concat art_dir (string_of_mode mode) in
  let dst_dir = Filename.concat sir_dir (string_of_mode mode) in

  if not (Sys.file_exists src_dir) then begin
    eprintf "Source directory does not exist: %s\n" src_dir;
    (0, 0)
  end else begin
    let yaml_files = find_yaml_files src_dir in
    let results =
      yaml_files
      |> List.map (fun src_path ->
             let rel_path = relative_path ~base:src_dir src_path in
             let dst_path =
               Filename.concat dst_dir (Filename.chop_extension rel_path ^ ".sexp")
             in
             let success = process_file mode src_path dst_path in
             if success then printf "  %s -> %s\n" rel_path (Filename.basename dst_path);
             success)
    in
    let success_count = List.filter (fun x -> x) results |> List.length in
    let total_count = List.length results in
    (success_count, total_count)
  end

let () =
  printf "Generating SIR S-expressions...\n\n";

  (* Process each mode *)
  let modes = [ Model; Decl; FunDecomp ] in
  let stats =
    modes
    |> List.map (fun mode ->
           printf "[%s]\n" (string_of_mode mode);
           let success, total = process_mode mode in
           printf "  Generated %d/%d files\n\n" success total;
           (mode, success, total))
  in

  (* Summary *)
  printf "Summary:\n";
  List.iter
    (fun (mode, success, total) ->
      printf "  %s: %d/%d\n" (string_of_mode mode) success total)
    stats;

  let total_success = List.fold_left (fun acc (_, s, _) -> acc + s) 0 stats in
  let total_files = List.fold_left (fun acc (_, _, t) -> acc + t) 0 stats in
  printf "  Total: %d/%d files\n" total_success total_files

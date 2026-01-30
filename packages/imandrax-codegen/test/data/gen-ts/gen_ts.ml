(** Generate TypeScript code from SIR S-expressions.

    Reads SIR S-expressions from TEST_DATA_SIR_DIR/{model,decl,fun_decomp}/
    and writes TypeScript code to TEST_DATA_TS_DIR/{model,decl,fun_decomp}/.

    Usage: TEST_DATA_SIR_DIR=test/data/sir TEST_DATA_TS_DIR=test/data/ts dune exec gen_ts.exe *)

module Sir = Semantic_ir

let sir_dir = Sys.getenv "TEST_DATA_SIR_DIR"
let ts_dir = Sys.getenv "TEST_DATA_TS_DIR"

(** Mode name -> SIR deserializer -> TypeScript emitter *)
let (modes : (string * (Sexplib.Sexp.t -> string)) list) =
  let concat_import_and_body body_code import =
    Typescript_adapter.Config.Extra_imports.lib_content import ^ body_code
  in
  [ ( "model"
    , fun s ->
        Sir.Value_assignment.t_of_sexp s
        |> Typescript_adapter.Emit.emit_value_assignment
        |> fun (code, import) -> concat_import_and_body code import )
  ; ( "decl"
    , fun s ->
        Sir.type_decl_of_sexp s
        |> Typescript_adapter.Emit.emit_type_decl
        |> fun (code, import) -> concat_import_and_body code import )
  ; ( "fun_decomp"
    , fun s ->
        Sir.test_suite_of_sexp s
        |> Typescript_adapter.Emit.emit_test_suite_dict
        |> fun (code, import) -> concat_import_and_body code import )
  ]
;;

let rec ensure_dir path =
  if not (Sys.file_exists path)
  then (
    ensure_dir (Filename.dirname path);
    Sys.mkdir path 0o755)
;;

let rec find_files ext dir =
  if Sys.is_directory dir
  then
    Sys.readdir dir
    |> Array.to_list
    |> List.concat_map (fun e -> find_files ext (Filename.concat dir e))
  else if Filename.check_suffix dir ext
  then [ dir ]
  else []
;;

let process_mode (mode, emit) =
  let src = Filename.concat sir_dir mode in
  let dst = Filename.concat ts_dir mode in
  if not (Sys.file_exists src)
  then 0
  else
    find_files ".sexp" src
    |> List.filter_map (fun path ->
           try
             let sexp = CCIO.File.read_exn path |> Sexplib.Sexp.of_string in
             let out =
               Filename.concat
                 dst
                 (String.sub
                    path
                    (String.length src + 1)
                    (String.length path - String.length src - 6)
                 ^ ".ts")
             in
             ensure_dir (Filename.dirname out);
             CCIO.File.write_exn out (emit sexp);
             Some ()
           with
           | _ -> None)
    |> List.length
;;

let () =
  let total = List.fold_left (fun acc m -> acc + process_mode m) 0 modes in
  Printf.printf "Generated %d TypeScript files\n" total
;;

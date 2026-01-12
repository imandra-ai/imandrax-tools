open Printf
module Codegen = Imandrax_codegen

(* Type definitions for better code organization *)
type input_source =
  | Stdin
  | File of string

type output_target =
  | Stdout
  | File of string

type file_format =
  | YAML
  | JSON

type parse_mode =
  | Model
  | FunDecomp
  | Decl

type config = {
  input: input_source;
  output: output_target;
  format: file_format;
  mode: parse_mode;
  output_as_dict: bool;
}

(* Helper functions *)

(** Log a message to stderr unless output is going to stdout. *)
let log use_stdout fmt =
  if not use_stdout then
    printf fmt
  else
    ifprintf stdout fmt

(** Read content from either stdin or a file path. *)
let read_content = function
  | Stdin -> CCIO.read_all stdin
  | File path -> CCIO.File.read_exn path

(** Validate that input content is not empty. *)
let validate_content content =
  if String.trim content = "" then
    failwith
      "Empty input: stdin contains no data (check if previous command in \
       pipeline failed)"

(** Determine the file format (YAML or JSON) based on file extension or stdin. *)
let determine_format input_file use_stdin =
  if use_stdin then
    JSON
  else if
    Filename.check_suffix input_file ".yaml"
    || Filename.check_suffix input_file ".yml"
  then
    YAML
  else if Filename.check_suffix input_file ".json" then
    JSON
  else
    failwith "Input file must be .json, .yaml, or .yml"

(** Parse YAML input according to the specified mode (Model or FunDecomp). *)
let parse_yaml_input mode input use_stdout =
  log use_stdout "Parsing YAML file...\n";
  let yaml_str = read_content input in
  validate_content yaml_str;
  let yaml = Yaml.of_string_exn yaml_str in
  match mode with
  | Model -> `Model (Codegen.Art_utils.yaml_to_model ~debug:false yaml)
  | FunDecomp -> `FunDecomp (Codegen.Art_utils.yaml_to_fun_decomp ~debug:false yaml)
  | Decl -> `Decl (Codegen.Art_utils.yaml_to_decl ~debug:false yaml)

(** Parse JSON input according to the specified mode (Model or FunDecomp). *)
let parse_json_input mode input use_stdout =
  log use_stdout "Parsing JSON file...\n";
  let content = read_content input in
  validate_content content;
  let json = Yojson.Safe.from_string content in
  match mode with
  | Model -> `Model (Codegen.Art_utils.json_to_model ~debug:false json)
  | FunDecomp -> `FunDecomp (Codegen.Art_utils.json_to_fun_decomp ~debug:false json)
  | Decl -> `Decl (Codegen.Art_utils.json_to_decl ~debug:false json)

(** Parse input based on configuration (delegates to YAML or JSON parser). *)
let parse_input config =
  let use_stdout =
    match config.output with
    | Stdout -> true
    | _ -> false
  in
  match config.format with
  | YAML -> parse_yaml_input config.mode config.input use_stdout
  | JSON -> parse_json_input config.mode config.input use_stdout

(** Convert parsed input to Python AST statements. *)
let convert_to_ast parsed_input use_stdout output_as_dict =
  log use_stdout "Converting to Python AST...\n";
  match parsed_input with
  | `Model model -> Codegen.Parse.parse_model model
  | `FunDecomp fun_decomp -> Codegen.Parse.parse_fun_decomp ~output_as_dict fun_decomp
  | `Decl decl -> (
    let parsed_decl = Codegen.Parse.parse_decl decl in
    match parsed_decl with
    | Ok ((stmts)) -> stmts
    | Error msg -> failwith msg
  )

(** Write AST statements as JSON to stdout or a file. *)
let write_output output stmts =
  let json_out = `List (List.map Codegen.Ast.stmt_to_yojson stmts) in
  match output with
  | Stdout ->
    Yojson.Safe.to_channel stdout json_out;
    print_newline ()
  | File path ->
    printf "Writing output to: %s\n" path;
    CCIO.with_out path (fun out -> Yojson.Safe.pretty_to_channel out json_out);
    printf "Done. Output written to %s\n" path

(** Handle errors by printing appropriate messages and exiting with error code. *)
let handle_error use_stdin = function
  | Failure msg ->
    eprintf "Error parsing input: %s\n" msg;
    exit 1
  | Yojson.Json_error msg ->
    eprintf "JSON parse error: %s\n" msg;
    (match use_stdin with
    | Stdin ->
      eprintf
        "Hint: Ensure stdin contains valid JSON (use 'yq -o json' to convert \
         YAML to JSON)\n"
    | _ -> ());
    exit 1
  | e ->
    eprintf "Unexpected error: %s\n" (Printexc.to_string e);
    exit 1

(** Print usage information and exit. *)
let print_usage () =
  eprintf "Usage: %s [input_file.json|yaml|-] <output_file.json|-> --mode <model|fun-decomp|decl> [--as-dict]\n"
    Sys.argv.(0);
  eprintf
    "  input_file: input JSON/YAML file path, or '-' for stdin (defaults to \
     JSON)\n";
  eprintf "  output_file: output JSON file path, or '-' for stdout\n";
  eprintf "  --mode: specify parsing mode (model or fun-decomp) [required]\n";
  eprintf
    "  --as-dict: output tests as dict values instead of functions (optional, only for fun-decomp mode)\n";
  exit 1

(* Main program *)
let () =
  (* Parse command line arguments *)
  if Array.length Sys.argv < 2 then
    print_usage ();

  (* Parse arguments *)
  let mode = ref None in
  let input_file = ref None in
  let output_file = ref None in
  let output_as_dict = ref false in

  let rec parse_args i =
    if i >= Array.length Sys.argv then ()
    else
      let arg = Sys.argv.(i) in
      match arg with
      | "--mode" ->
        if i + 1 >= Array.length Sys.argv then (
          eprintf "Error: --mode requires an argument\n";
          print_usage ()
        );
        let mode_str = Sys.argv.(i + 1) in
        (match mode_str with
        | "model" -> mode := Some Model
        | "fun-decomp" -> mode := Some FunDecomp
        | "decl" -> mode := Some Decl
        | _ ->
          eprintf "Error: Invalid mode '%s'. Must be 'model', 'fun-decomp', or 'decl'\n" mode_str;
          print_usage ());
        parse_args (i + 2)
      | "--as-dict" ->
        output_as_dict := true;
        parse_args (i + 1)
      | _ when String.length arg > 0 && arg.[0] = '-' && arg <> "-" ->
        eprintf "Error: Unknown flag '%s'\n" arg;
        print_usage ()
      | _ ->
        if !input_file = None then (
          input_file := Some arg;
          parse_args (i + 1)
        )
        else if !output_file = None then (
          output_file := Some arg;
          parse_args (i + 1)
        )
        else (
          eprintf "Error: Unexpected argument '%s'\n" arg;
          print_usage ()
        )
  in
  parse_args 1;

  let mode =
    match !mode with
    | None ->
      eprintf "Error: --mode flag is required\n";
      print_usage ()
    | Some m -> m
  in

  let input_file =
    match !input_file with
    | None ->
      eprintf "Error: input file is required\n";
      print_usage ()
    | Some f -> f
  in

  let output_file =
    match !output_file with
    | None -> "-"
    | Some f -> f
  in
  let output_as_dict = !output_as_dict in

  (* Validate --as-dict only used with fun-decomp mode *)
  if output_as_dict && mode = Model then (
    eprintf "Warning: --as-dict flag is ignored for model mode\n"
  );

  let input =
    if input_file = "-" then
      Stdin
    else
      File input_file
  in
  let output =
    if output_file = "-" then
      Stdout
    else
      File output_file
  in

  (* Validate input file exists *)
  (match input with
  | File path when not (Sys.file_exists path) ->
    eprintf "Error: Input file '%s' does not exist\n" path;
    exit 1
  | _ -> ());

  let format =
    try determine_format input_file (input = Stdin)
    with Failure msg ->
      eprintf "Error: %s\n" msg;
      exit 1
  in

  let config = { input; output; format; mode; output_as_dict } in

  (* Log configuration *)
  let use_stdout =
    match output with
    | Stdout -> true
    | _ -> false
  in
  if not use_stdout then (
    (match input with
    | Stdin -> printf "Reading from: stdin\n"
    | File path -> printf "Reading from: %s\n" path);
    printf "Mode: %s\n" (match mode with Model -> "model" | FunDecomp -> "fun-decomp" | Decl -> "decl");
    printf "Output to: %s\n" output_file
  );

  (* Main processing pipeline *)
  try
    let parsed = parse_input config in
    log use_stdout "Successfully parsed input\n";

    let stmts = convert_to_ast parsed use_stdout config.output_as_dict in
    log use_stdout "Generated %d statements\n" (List.length stmts);

    write_output output stmts
  with e -> handle_error input e

module Codegen = Imandrax_codegen

type parse_mode = Model | FunDecomp | Decl
type test_format = [ `Function | `Dict ]
type lang = Python | TypeScript

let is_yaml_file path =
  Filename.check_suffix path ".yaml" || Filename.check_suffix path ".yml"

let is_json_content content =
  let trimmed = String.trim content in
  String.length trimmed > 0 && (trimmed.[0] = '{' || trimmed.[0] = '[')

let read_and_parse mode input =
  let content = match input with
    | "-" -> CCIO.read_all stdin
    | path ->
      if not (Sys.file_exists path) then
        failwith (Printf.sprintf "Input file '%s' does not exist" path);
      CCIO.File.read_exn path
  in
  if String.trim content = "" then failwith "Empty input";
  let parse_json json = match mode with
    | Model -> `Model (Codegen.Art_utils.json_to_model ~debug:false json)
    | FunDecomp -> `FunDecomp (Codegen.Art_utils.json_to_fun_decomp ~debug:false json)
    | Decl -> `Decl (Codegen.Art_utils.json_to_decl ~debug:false json)
  in
  let parse_yaml yaml = match mode with
    | Model -> `Model (Codegen.Art_utils.yaml_to_model ~debug:false yaml)
    | FunDecomp -> `FunDecomp (Codegen.Art_utils.yaml_to_fun_decomp ~debug:false yaml)
    | Decl -> `Decl (Codegen.Art_utils.yaml_to_decl ~debug:false yaml)
  in
  let use_yaml = (input <> "-" && is_yaml_file input) || (input = "-" && not (is_json_content content)) in
  if use_yaml then
    parse_yaml (Yaml.of_string_exn content)
  else
    parse_json (Yojson.Safe.from_string content)

let to_python_ast test_format = function
  | `Model m -> Python_adapter.Lib.parse_model m
  | `FunDecomp fd -> Python_adapter.Lib.parse_fun_decomp test_format fd
  | `Decl d ->
    match Python_adapter.Lib.parse_decl d with
    | Ok stmts -> stmts
    | Error msg -> failwith msg

let to_typescript = function
  | `Model m ->
    let code, imports = Typescript_adapter.Lib.parse_model m in
    Typescript_adapter.Config.Extra_imports.lib_content imports ^ code
  | `FunDecomp fd ->
    let code, imports = Typescript_adapter.Lib.parse_fun_decomp fd in
    Typescript_adapter.Config.Extra_imports.lib_content imports ^ code
  | `Decl d ->
    match Typescript_adapter.Lib.parse_decl d with
    | Ok (code, imports) ->
      Typescript_adapter.Config.Extra_imports.lib_content imports ^ code
    | Error msg -> failwith msg

let python_ast_to_json stmts =
  `List (List.map Python_adapter.Ast.stmt_to_yojson stmts)

let write_json output json =
  match output with
  | "-" -> Yojson.Safe.to_channel stdout json; print_newline ()
  | path -> CCIO.with_out path (fun out -> Yojson.Safe.pretty_to_channel out json)

let write_string output s =
  match output with
  | "-" -> print_string s; if not (String.ends_with ~suffix:"\n" s) then print_newline ()
  | path -> CCIO.File.write_exn path s

let usage () =
  Printf.eprintf "Usage: %s <input|-> [output|-] --mode <model|fun-decomp|decl> [--lang <python|typescript>] [--as-dict]\n"
    Sys.argv.(0);
  exit 1

let parse_mode_of_string = function
  | "model" -> Model
  | "fun-decomp" -> FunDecomp
  | "decl" -> Decl
  | s -> failwith (Printf.sprintf "Invalid mode '%s'" s)

let parse_lang_of_string = function
  | "python" | "py" -> Python
  | "typescript" | "ts" -> TypeScript
  | s -> failwith (Printf.sprintf "Invalid lang '%s'" s)

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let mode = ref None in
  let lang = ref Python in
  let as_dict = ref false in
  let positional = ref [] in

  let rec parse = function
    | [] -> ()
    | "--mode" :: m :: rest ->
      mode := Some (parse_mode_of_string m);
      parse rest
    | "--lang" :: l :: rest ->
      lang := parse_lang_of_string l;
      parse rest
    | "--as-dict" :: rest ->
      as_dict := true;
      parse rest
    | arg :: _ when String.length arg > 1 && arg.[0] = '-' && arg.[1] <> '-' && arg <> "-" ->
      failwith (Printf.sprintf "Unknown flag '%s'" arg)
    | arg :: rest ->
      positional := arg :: !positional;
      parse rest
  in

  (try parse args with Failure msg -> Printf.eprintf "Error: %s\n" msg; usage ());

  let mode = match !mode with Some m -> m | None -> Printf.eprintf "Error: --mode required\n"; usage () in
  let lang = !lang in
  let positional = List.rev !positional in
  let input, output = match positional with
    | [i] -> i, "-"
    | [i; o] -> i, o
    | _ -> usage ()
  in
  let test_format : test_format = if !as_dict then `Dict else `Function in

  try
    let parsed = read_and_parse mode input in
    match lang with
    | Python ->
      parsed |> to_python_ast test_format |> python_ast_to_json |> write_json output
    | TypeScript ->
      parsed |> to_typescript |> write_string output
  with
  | Failure msg -> Printf.eprintf "Error: %s\n" msg; exit 1
  | Yojson.Json_error msg ->
    Printf.eprintf "JSON parse error: %s\n" msg;
    if input = "-" then
      Printf.eprintf "Hint: stdin must be valid JSON (use 'yq -o json' to convert YAML)\n";
    exit 1
  | e -> Printf.eprintf "Error: %s\n" (Printexc.to_string e); exit 1

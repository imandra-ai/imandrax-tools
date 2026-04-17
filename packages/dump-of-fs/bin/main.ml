open Cmdliner

type output_format = Json | Yaml_fmt

let rec yojson_to_yaml (v : Yojson.Safe.t) : Yaml.value =
  match v with
  | `Null -> `Null
  | `Bool b -> `Bool b
  | `Int i -> `Float (float_of_int i)
  | `Intlit s -> `String s
  | `Float f -> `Float f
  | `String s -> `String s
  | `List xs -> `A (List.map yojson_to_yaml xs)
  | `Tuple xs -> `A (List.map yojson_to_yaml xs)
  | `Assoc kvs -> `O (List.map (fun (k, v) -> (k, yojson_to_yaml v)) kvs)
  | `Variant (_, _) -> `Null

let die fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline s;
      exit 1)
    fmt

let render tree = function
  | Json -> Yojson.Safe.pretty_to_string tree
  | Yaml_fmt ->
    (match Yaml.to_string (yojson_to_yaml tree) with
     | Ok s -> s
     | Error (`Msg m) -> die "rendering YAML: %s" m)

let run root format output no_default_ignore ignores =
  let root =
    try Unix.realpath root
    with Unix.Unix_error (e, _, _) ->
      die "resolving root %s: %s" root (Unix.error_message e)
  in
  (match (Unix.stat root).st_kind with
   | S_DIR -> ()
   | _ -> die "root %s is not a directory" root);
  let opts = Dump_of_fs.{ no_default_ignore; ignores } in
  let tree =
    try Dump_of_fs.build_tree root opts with Failure msg -> die "%s" msg
  in
  let rendered = render tree format in
  match output with
  | None -> print_endline rendered
  | Some path ->
    let oc =
      try open_out path
      with Sys_error e -> die "writing output %s: %s" path e
    in
    output_string oc rendered;
    close_out oc

(* ==========================================================================
   CLI
   ========================================================================== *)

let format_conv =
  Arg.enum [ ("json", Json); ("yaml", Yaml_fmt) ]

let root_arg =
  let doc = "Root directory to dump." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"ROOT" ~doc)

let format_arg =
  let doc = "Output format: json|yaml." in
  Arg.(value & opt format_conv Json & info [ "format" ] ~doc)

let output_arg =
  let doc = "Output file (stdout if omitted)." in
  Arg.(value & opt (some string) None & info [ "output"; "o" ] ~docv:"FILE" ~doc)

let no_default_ignore_arg =
  let doc = "Disable default ignores (.gitignore + dotfiles)." in
  Arg.(value & flag & info [ "no-default-ignore" ] ~doc)

let ignores_arg =
  let doc = "Additional ignore pattern (glob syntax); repeatable." in
  Arg.(value & opt_all string [] & info [ "ignore" ] ~docv:"PATTERN" ~doc)

let cmd =
  let info =
    Cmd.info "dump-of-fs"
      ~doc:"Dump a file tree into a single JSON/YAML document"
  in
  let term =
    Term.(
      const run
      $ root_arg
      $ format_arg
      $ output_arg
      $ no_default_ignore_arg
      $ ignores_arg)
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)

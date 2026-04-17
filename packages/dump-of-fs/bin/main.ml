open Cmdliner

type output_format = Json | Yaml_fmt
type multiline_style = Literal | Folded

let die fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline s;
      exit 1)
    fmt

(* ==========================================================================
   Custom YAML emitter

   Walks a Yojson.Safe.t and emits libyaml events directly, so that per-scalar
   styling is possible. Multiline strings are emitted as literal block scalars
   (`|`), everything else is left to libyaml to pick a sensible style.
   ========================================================================== *)

let emit_yaml ~multiline (v : Yojson.Safe.t) :
    (string, [ `Msg of string ]) result =
  let open Yaml.Stream in
  let ( let* ) = Result.bind in
  let* e = emitter ~len:(1 lsl 20) () in
  let scalar_ev ?(style : Yaml.scalar_style = `Any) value =
    Event.Scalar
      {
        anchor = None;
        tag = None;
        value;
        plain_implicit = true;
        quoted_implicit = true;
        style;
      }
  in
  let multiline_style : Yaml.scalar_style =
    match multiline with Literal -> `Literal | Folded -> `Folded
  in
  let pick_string_style s : Yaml.scalar_style =
    if String.contains s '\n' then multiline_style else `Any
  in
  let rec go (v : Yojson.Safe.t) =
    match v with
    | `Null -> emit e (scalar_ev ~style:`Plain "")
    | `Bool b -> emit e (scalar_ev ~style:`Plain (if b then "true" else "false"))
    | `Int i -> emit e (scalar_ev ~style:`Plain (string_of_int i))
    | `Intlit s -> emit e (scalar_ev ~style:`Plain s)
    | `Float f ->
      let s =
        if Float.is_integer f && Float.abs f < 1e18 then
          string_of_int (int_of_float f)
        else Printf.sprintf "%.17g" f
      in
      emit e (scalar_ev ~style:`Plain s)
    | `String s -> emit e (scalar_ev ~style:(pick_string_style s) s)
    | `List xs | `Tuple xs ->
      let* () =
        emit e
          (Event.Sequence_start
             { anchor = None; tag = None; implicit = true; style = `Block })
      in
      let* () =
        List.fold_left
          (fun acc x ->
            let* () = acc in
            go x)
          (Ok ()) xs
      in
      emit e Event.Sequence_end
    | `Assoc kvs ->
      let* () =
        emit e
          (Event.Mapping_start
             { anchor = None; tag = None; implicit = true; style = `Block })
      in
      let* () =
        List.fold_left
          (fun acc (k, v) ->
            let* () = acc in
            let* () = emit e (scalar_ev ~style:(pick_string_style k) k) in
            go v)
          (Ok ()) kvs
      in
      emit e Event.Mapping_end
    | `Variant _ -> emit e (scalar_ev ~style:`Plain "")
  in
  let* () = emit e (Event.Stream_start { encoding = `Utf8 }) in
  let* () = emit e (Event.Document_start { version = None; implicit = true }) in
  let* () = go v in
  let* () = emit e (Event.Document_end { implicit = true }) in
  let* () = emit e Event.Stream_end in
  Ok (emitter_buf e)

let render ~multiline tree = function
  | Json -> Yojson.Safe.pretty_to_string tree
  | Yaml_fmt ->
    (match emit_yaml ~multiline tree with
     | Ok s -> s
     | Error (`Msg m) -> die "rendering YAML: %s" m)

let run root format output no_default_ignore ignores multiline =
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
  let rendered = render ~multiline tree format in
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

let multiline_conv =
  Arg.enum [ ("literal", Literal); ("folded", Folded) ]

let multiline_arg =
  let doc =
    "YAML block scalar style for multiline strings: literal (|) preserves \
     newlines; folded (>) turns single newlines into spaces. Only affects \
     --format yaml."
  in
  Arg.(
    value & opt multiline_conv Literal
    & info [ "yaml-multiline" ] ~docv:"STYLE" ~doc)

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
      $ ignores_arg
      $ multiline_arg)
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)

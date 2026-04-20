open Dump_of_fs

(* ==========================================================================
   Test helpers
   ========================================================================== *)

let mk_tmp () = Filename.temp_dir "dump_of_fs_test_" ""

let rec mkdir_p d =
  if not (Sys.file_exists d) then begin
    mkdir_p (Filename.dirname d);
    try Unix.mkdir d 0o755 with Unix.Unix_error (EEXIST, _, _) -> ()
  end

let write root rel content =
  let path = Filename.concat root rel in
  mkdir_p (Filename.dirname path);
  let oc = open_out path in
  output_string oc content;
  close_out oc

let run root = build_tree root default_options

let yojson_testable =
  Alcotest.testable
    (fun ppf v -> Format.pp_print_string ppf (Yojson.Safe.pretty_to_string v))
    Yojson.Safe.equal

let check_json = Alcotest.check yojson_testable

let contains_substring ~needle s =
  let nl = String.length needle in
  let sl = String.length s in
  if nl = 0 then true
  else
    let rec loop i =
      if i + nl > sl then false
      else if String.sub s i nl = needle then true
      else loop (i + 1)
    in
    loop 0

let expect_failure msg_needle thunk =
  match thunk () with
  | exception Failure msg ->
    if not (contains_substring ~needle:msg_needle msg) then
      Alcotest.failf "expected failure containing %S, got: %s" msg_needle msg
  | _ -> Alcotest.fail "expected Failure, got ok"

(* ==========================================================================
   Basic tree + extension stripping
   ========================================================================== *)

let nests_dirs_and_strips_extensions () =
  let tmp = mk_tmp () in
  write tmp "a.md" "hello";
  write tmp "sub/b.txt" "world";
  let got = run tmp in
  check_json "nested" (`Assoc [
    ("a", `String "hello");
    ("sub", `Assoc [ ("b", `String "world") ]);
  ]) got

let files_without_extension_use_raw_name () =
  let tmp = mk_tmp () in
  write tmp "README" "x";
  check_json "readme" (`Assoc [ ("README", `String "x") ]) (run tmp)

(* ==========================================================================
   JSON injection
   ========================================================================== *)

let injects_json_as_parsed_object () =
  let tmp = mk_tmp () in
  write tmp "data.json" {|{"x": 1, "y": [2, 3]}|};
  check_json "object"
    (`Assoc [
      ("data",
       `Assoc [ ("x", `Int 1); ("y", `List [ `Int 2; `Int 3 ]) ]);
    ])
    (run tmp)

let injects_json_scalar_root () =
  let tmp = mk_tmp () in
  write tmp "n.json" "42";
  check_json "scalar" (`Assoc [ ("n", `Int 42) ]) (run tmp)

let injects_json_array_root () =
  let tmp = mk_tmp () in
  write tmp "list.json" "[1,2,3]";
  check_json "array"
    (`Assoc [ ("list", `List [ `Int 1; `Int 2; `Int 3 ]) ])
    (run tmp)

(* ==========================================================================
   YAML injection
   ========================================================================== *)

let injects_yaml_extension () =
  let tmp = mk_tmp () in
  write tmp "c.yaml" "name: test\ncount: 2\n";
  check_json "yaml"
    (`Assoc [
      ("c", `Assoc [ ("count", `Int 2); ("name", `String "test") ]);
    ])
    (run tmp)

let injects_yml_extension () =
  let tmp = mk_tmp () in
  write tmp "c.yml" "k: v\n";
  check_json "yml" (`Assoc [ ("c", `Assoc [ ("k", `String "v") ]) ]) (run tmp)

(* ==========================================================================
   Default ignores
   ========================================================================== *)

let dotfiles_ignored_by_default () =
  let tmp = mk_tmp () in
  write tmp "visible.txt" "v";
  write tmp ".hidden" "h";
  check_json "dotfile" (`Assoc [ ("visible", `String "v") ]) (run tmp)

let no_default_ignore_includes_dotfiles () =
  let tmp = mk_tmp () in
  write tmp "visible.txt" "v";
  write tmp ".hidden" "h";
  let got =
    build_tree tmp { no_default_ignore = true; ignores = [] }
  in
  check_json "all"
    (`Assoc [ (".hidden", `String "h"); ("visible", `String "v") ])
    got

let gitignore_respected_by_default () =
  let tmp = mk_tmp () in
  let rc = Sys.command (Printf.sprintf "git init -q %s" (Filename.quote tmp)) in
  assert (rc = 0);
  write tmp ".gitignore" "skipme.txt\n";
  write tmp "keep.txt" "k";
  write tmp "skipme.txt" "s";
  check_json "gitignore" (`Assoc [ ("keep", `String "k") ]) (run tmp)

(* ==========================================================================
   Error paths
   ========================================================================== *)

let errors_on_file_key_collision () =
  let tmp = mk_tmp () in
  write tmp "foo.md" "a";
  write tmp "foo.txt" "b";
  expect_failure "collision" (fun () -> ignore (run tmp))

let errors_on_dir_vs_file_collision () =
  let tmp = mk_tmp () in
  write tmp "foo.md" "a";
  write tmp "foo/bar.md" "b";
  expect_failure "collision" (fun () -> ignore (run tmp))

let errors_on_bad_json () =
  let tmp = mk_tmp () in
  write tmp "bad.json" "{not valid";
  expect_failure "parsing JSON" (fun () -> ignore (run tmp))

let errors_on_bad_yaml () =
  let tmp = mk_tmp () in
  write tmp "bad.yaml" "[unterminated\n";
  expect_failure "parsing YAML" (fun () -> ignore (run tmp))

(* ==========================================================================
   Driver
   ========================================================================== *)

let () =
  Alcotest.run "dump_of_fs" [
    ( "tree", [
        Alcotest.test_case "nests dirs and strips extensions" `Quick
          nests_dirs_and_strips_extensions;
        Alcotest.test_case "files without extension use raw name" `Quick
          files_without_extension_use_raw_name;
      ]);
    ( "json", [
        Alcotest.test_case "json object" `Quick injects_json_as_parsed_object;
        Alcotest.test_case "json scalar" `Quick injects_json_scalar_root;
        Alcotest.test_case "json array" `Quick injects_json_array_root;
      ]);
    ( "yaml", [
        Alcotest.test_case "yaml" `Quick injects_yaml_extension;
        Alcotest.test_case "yml" `Quick injects_yml_extension;
      ]);
    ( "ignores", [
        Alcotest.test_case "dotfiles ignored" `Quick dotfiles_ignored_by_default;
        Alcotest.test_case "no-default-ignore includes dotfiles" `Quick
          no_default_ignore_includes_dotfiles;
        Alcotest.test_case "gitignore respected" `Quick
          gitignore_respected_by_default;
      ]);
    ( "errors", [
        Alcotest.test_case "file collision" `Quick errors_on_file_key_collision;
        Alcotest.test_case "dir-vs-file collision" `Quick
          errors_on_dir_vs_file_collision;
        Alcotest.test_case "bad json" `Quick errors_on_bad_json;
        Alcotest.test_case "bad yaml" `Quick errors_on_bad_yaml;
      ]);
  ]

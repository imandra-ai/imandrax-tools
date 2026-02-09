[@@@ocamlformat "break-string-literals=never"]

module Sir = Semantic_ir

let workspace_dir = "../../.."
let default_TEST_DATA_SIR_DIR = "test/data/sir"

let sir_data_dir =
  let art_data_dir_rel_to_workspace =
    Sys.getenv_opt "TEST_DATA_SIR_DIR"
    |> CCOption.get_or ~default:default_TEST_DATA_SIR_DIR
  in
  workspace_dir ^ "/" ^ art_data_dir_rel_to_workspace
;;

let read_sir (name : string) : Sir.type_decl =
  let sir_file = {%string|%{sir_data_dir}/decl/%{name}.sexp|} in
  let sir_sexp = CCIO.File.read_exn sir_file in
  Sir.type_decl_of_sexp (Sexplib.Sexp.of_string sir_sexp)
;;

let run_test : string -> unit = fun name ->
  let sir = read_sir name in
  let code, _ = Typescript_adapter.Emit.emit_type_decl sir in
  print_endline code;
;;


(*$
  let data =
    [ "real_and_option"
    ; "record"
    ; "record_with_composite_type"
    ; "variant_simple"
    ; "variant_two"
    ; "variant_with_payload"
    ; "variant_with_composite_payload"
    ; "variant_poly"
    ; "variant_poly_two_var"
    ; "nested_generics"
    ]
  in
  print_endline "";
  List.iter
    (fun name ->
      let test =
        [%string
          "let%expect_test \"%{name}\" =
  run_test \"%{name}\";
  [%expect
    {||}]
;;
"]
      in
      print_endline test)
    data
*)
let%expect_test "real_and_option" =
  run_test "real_and_option";
  [%expect
    {||}]
;;

let%expect_test "record" =
  run_test "record";
  [%expect
    {||}]
;;

let%expect_test "record_with_composite_type" =
  run_test "record_with_composite_type";
  [%expect
    {||}]
;;

let%expect_test "variant_simple" =
  run_test "variant_simple";
  [%expect
    {||}]
;;

let%expect_test "variant_two" =
  run_test "variant_two";
  [%expect
    {||}]
;;

let%expect_test "variant_with_payload" =
  run_test "variant_with_payload";
  [%expect
    {||}]
;;

let%expect_test "variant_with_composite_payload" =
  run_test "variant_with_composite_payload";
  [%expect
    {||}]
;;

let%expect_test "variant_poly" =
  run_test "variant_poly";
  [%expect
    {||}]
;;

let%expect_test "variant_poly_two_var" =
  run_test "variant_poly_two_var";
  [%expect
    {||}]
;;

let%expect_test "nested_generics" =
  run_test "nested_generics";
  [%expect
    {||}]
;;

(*$*)

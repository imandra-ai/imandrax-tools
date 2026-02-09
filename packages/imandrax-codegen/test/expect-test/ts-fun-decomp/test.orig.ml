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

let read_sir (name : string) : Sir.test_suite =
  let sir_file = {%string|%{sir_data_dir}/fun_decomp/%{name}.sexp|} in
  let sir_sexp = CCIO.File.read_exn sir_file in
  Sir.test_suite_of_sexp (Sexplib.Sexp.of_string sir_sexp)
;;

let run_test : string -> unit = fun name ->
    let sir = read_sir name in
    let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
    print_endline code;
;;

(*$
  let data =
    [ "basic"
    ; "nested_conditions"
    ; "variant_simple"
    ; "complex_variant_record"
    ; "option_type"
    ; "variant_with_data"
    ; "composite_record"
    ; "primitive_bool"
    ; "with_basis"
    ; "composite_tuple"
    ; "primitive_int"
    ; "with_guards"
    ; "list_operations"
    ; "primitive_real"
    ; "multiple_parameters"
    ; "variant_poly"
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
let%expect_test "basic" =
  run_test "basic";
  [%expect
    {||}]
;;

let%expect_test "nested_conditions" =
  run_test "nested_conditions";
  [%expect
    {||}]
;;

let%expect_test "variant_simple" =
  run_test "variant_simple";
  [%expect
    {||}]
;;

let%expect_test "complex_variant_record" =
  run_test "complex_variant_record";
  [%expect
    {||}]
;;

let%expect_test "option_type" =
  run_test "option_type";
  [%expect
    {||}]
;;

let%expect_test "variant_with_data" =
  run_test "variant_with_data";
  [%expect
    {||}]
;;

let%expect_test "composite_record" =
  run_test "composite_record";
  [%expect
    {||}]
;;

let%expect_test "primitive_bool" =
  run_test "primitive_bool";
  [%expect
    {||}]
;;

let%expect_test "with_basis" =
  run_test "with_basis";
  [%expect
    {||}]
;;

let%expect_test "composite_tuple" =
  run_test "composite_tuple";
  [%expect
    {||}]
;;

let%expect_test "primitive_int" =
  run_test "primitive_int";
  [%expect
    {||}]
;;

let%expect_test "with_guards" =
  run_test "with_guards";
  [%expect
    {||}]
;;

let%expect_test "list_operations" =
  run_test "list_operations";
  [%expect
    {||}]
;;

let%expect_test "primitive_real" =
  run_test "primitive_real";
  [%expect
    {||}]
;;

let%expect_test "multiple_parameters" =
  run_test "multiple_parameters";
  [%expect
    {||}]
;;

let%expect_test "variant_poly" =
  run_test "variant_poly";
  [%expect
    {||}]
;;

(*$*)

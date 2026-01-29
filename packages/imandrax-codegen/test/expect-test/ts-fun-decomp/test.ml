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
  let sir = read_sir \"%{name}\" in
  let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
  print_endline code;
  [%expect
    {||}]
;;
"]
      in
      print_endline test)
    data
*)
let%expect_test "basic" =
  let sir = read_sir "basic" in
  let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
  print_endline code;
  [%expect
    {|
    const tests = {
      "test_1": {
        input: { x: 1 },
        expected: 3
      },
      "test_2": {
        input: { x: 0 },
        expected: 1
      }
    };
    |}]
;;

let%expect_test "nested_conditions" =
  let sir = read_sir "nested_conditions" in
  let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
  print_endline code;
  [%expect
    {|
    const tests = {
      "test_1": {
        input: { x: 0, y: 0 },
        expected: 0
      },
      "test_2": {
        input: { y: 1, x: 0 },
        expected: 1
      },
      "test_3": {
        input: { x: 1, y: 0 },
        expected: 1
      },
      "test_4": {
        input: { x: 1, y: 1 },
        expected: 2
      }
    };
    |}]
;;

let%expect_test "variant_simple" =
  let sir = read_sir "variant_simple" in
  let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
  print_endline code;
  [%expect
    {|
    const tests = {
      "test_1": {
        input: { c: { tag: "Red", payload: null } },
        expected: 1
      },
      "test_2": {
        input: { c: { tag: "Green", payload: null } },
        expected: 2
      },
      "test_3": {
        input: { c: { tag: "Blue", payload: null } },
        expected: 3
      }
    };
    |}]
;;

let%expect_test "complex_variant_record" =
  let sir = read_sir "complex_variant_record" in
  let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
  print_endline code;
  [%expect
    {|
    const tests = {
      "test_1": {
        input: { u: { id: 0, active: { tag: "Inactive", payload: null } } },
        expected: -1
      },
      "test_2": {
        input: { u: { id: 0, active: { tag: "Active", payload: null } } },
        expected: 0
      },
      "test_3": {
        input: { u: { id: 1, active: { tag: "Active", payload: null } } },
        expected: 1
      }
    };
    |}]
;;

let%expect_test "option_type" =
  let sir = read_sir "option_type" in
  let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
  print_endline code;
  [%expect
    {|
    const tests = {
      "test_1": {
        input: { opt: { tag: "Some", payload: 0 } },
        expected: 0
      },
      "test_2": {
        input: { opt: { tag: "Some", payload: 1 } },
        expected: 1
      },
      "test_3": {
        input: { opt: null },
        expected: 0
      }
    };
    |}]
;;

let%expect_test "variant_with_data" =
  let sir = read_sir "variant_with_data" in
  let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
  print_endline code;
  [%expect
    {|
    const tests = {
      "test_1": {
        input: { s: { tag: "Rectangle", payload: [0, 1] } },
        expected: 0
      },
      "test_2": {
        input: { s: { tag: "Circle", payload: 0 } },
        expected: 0
      }
    };
    |}]
;;

let%expect_test "composite_record" =
  let sir = read_sir "composite_record" in
  let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
  print_endline code;
  [%expect
    {|
    const tests = {
      "test_1": {
        input: { p: { x: 0, y: 1 } },
        expected: "positive"
      },
      "test_2": {
        input: { p: { x: -38, y: 38 } },
        expected: "origin"
      },
      "test_3": {
        input: { p: { x: 0, y: -1 } },
        expected: "negative"
      }
    };
    |}]
;;

let%expect_test "primitive_bool" =
  let sir = read_sir "primitive_bool" in
  let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
  print_endline code;
  [%expect
    {|
    const tests = {
      "test_1": {
        input: { a: false, b: false },
        expected: 0
      },
      "test_2": {
        input: { a: false, b: true },
        expected: 2
      },
      "test_3": {
        input: { a: true, b: false },
        expected: 2
      },
      "test_4": {
        input: { a: true, b: true },
        expected: 1
      }
    };
    |}]
;;

let%expect_test "with_basis" =
  let sir = read_sir "with_basis" in
  let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
  print_endline code;
  [%expect
    {|
    const tests = {
      "test_1": {
        input: { x: 0 },
        expected: 0
      },
      "test_2": {
        input: { x: 1 },
        expected: 1
      }
    };
    |}]
;;

let%expect_test "composite_tuple" =
  let sir = read_sir "composite_tuple" in
  let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
  print_endline code;
  [%expect
    {|
    const tests = {
      "test_1": {
        input: { _x_1_25: [0, 1] },
        expected: 1
      },
      "test_2": {
        input: { _x_1_25: [0, 0] },
        expected: 0
      },
      "test_3": {
        input: { _x_1_25: [0, -1] },
        expected: 1
      }
    };
    |}]
;;

let%expect_test "primitive_int" =
  let sir = read_sir "primitive_int" in
  let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
  print_endline code;
  [%expect
    {|
    const tests = {
      "test_1": {
        input: { x: 1 },
        expected: 1
      },
      "test_2": {
        input: { x: 0 },
        expected: 0
      },
      "test_3": {
        input: { x: -1 },
        expected: -1
      }
    };
    |}]
;;

let%expect_test "with_guards" =
  let sir = read_sir "with_guards" in
  let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
  print_endline code;
  [%expect
    {|
    const tests = {
      "test_1": {
        input: { x: 0, y: 0 },
        expected: 0
      },
      "test_2": {
        input: { x: 1, y: 0 },
        expected: 0
      },
      "test_3": {
        input: { y: 1, x: 1 },
        expected: 3
      },
      "test_4": {
        input: { y: 2, x: 1 },
        expected: 2
      },
      "test_5": {
        input: { y: 1, x: 2 },
        expected: 1
      }
    };
    |}]
;;

let%expect_test "list_operations" =
  let sir = read_sir "list_operations" in
  let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
  print_endline code;
  [%expect
    {|
    const tests = {
      "test_1": {
        input: { xs: [] },
        expected: 0
      },
      "test_2": {
        input: { xs: [0] },
        expected: 0
      },
      "test_3": {
        input: { xs: [1, 0] },
        expected: 1
      }
    };
    |}]
;;

let%expect_test "primitive_real" =
  let sir = read_sir "primitive_real" in
  let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
  print_endline code;
  [%expect
    {|
    const tests = {
      "test_1": {
        input: { temp: 0 },
        expected: true
      }
    };
    |}]
;;

let%expect_test "multiple_parameters" =
  let sir = read_sir "multiple_parameters" in
  let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
  print_endline code;
  [%expect
    {|
    const tests = {
      "test_1": {
        input: { b: 1, c: 2, a: 0 },
        expected: 0
      },
      "test_2": {
        input: { a: 0, c: 1, b: 1 },
        expected: 0
      },
      "test_3": {
        input: { b: 0, a: 0, c: 0 },
        expected: 0
      },
      "test_4": {
        input: { b: 0, a: 1, c: 1 },
        expected: 0
      },
      "test_5": {
        input: { a: 0, c: -1, b: -1 },
        expected: 0
      },
      "test_6": {
        input: { a: 1, c: -1, b: 0 },
        expected: 0
      }
    };
    |}]
;;

let%expect_test "variant_poly" =
  let sir = read_sir "variant_poly" in
  let code, _ = Typescript_adapter.Emit.emit_test_suite_dict sir in
  print_endline code;
  [%expect
    {|
    const tests = {
      "test_1": {
        input: { c: { tag: "Labeled", payload: [0, 0.] } },
        expected: 3
      },
      "test_2": {
        input: { c: { tag: "Single", payload: 0 } },
        expected: -1
      },
      "test_3": {
        input: { c: { tag: "Single", payload: 1 } },
        expected: 1
      },
      "test_4": {
        input: { c: { tag: "Pair", payload: [0, 0.] } },
        expected: -2
      },
      "test_5": {
        input: { c: { tag: "Pair", payload: [0, -1.] } },
        expected: 2
      },
      "test_6": {
        input: { c: { tag: "Empty", payload: null } },
        expected: 0
      }
    };
    |}]
;;

(*$*)

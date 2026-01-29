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
  let sir = read_sir \"%{name}\" in
  let code, _ = Typescript_adapter.Emit.emit_type_decl sir in
  print_endline code;
  [%expect
    {||}]
;;
"]
      in
      print_endline test)
    data
*)
let%expect_test "real_and_option" =
  let sir = read_sir "real_and_option" in
  let code, _ = Typescript_adapter.Emit.emit_type_decl sir in
  print_endline code;
  [%expect
    {|
    type my_ty = {
      x: number;
      y: number | null;
      z: number;
    };
    |}]
;;

let%expect_test "record" =
  let sir = read_sir "record" in
  let code, _ = Typescript_adapter.Emit.emit_type_decl sir in
  print_endline code;
  [%expect
    {|
    type point = {
      x: number;
      y: number;
    };
    |}]
;;

let%expect_test "record_with_composite_type" =
  let sir = read_sir "record_with_composite_type" in
  let code, _ = Typescript_adapter.Emit.emit_type_decl sir in
  print_endline code;
  [%expect
    {|
    type shape = {
      circle: number | null;
    };
    |}]
;;

let%expect_test "variant_simple" =
  let sir = read_sir "variant_simple" in
  let code, _ = Typescript_adapter.Emit.emit_type_decl sir in
  print_endline code;
  [%expect
    {|
    type color =
      | { tag: "Red"; payload: null }
      | { tag: "Green"; payload: null }
      | { tag: "Blue"; payload: null };
    |}]
;;

let%expect_test "variant_two" =
  let sir = read_sir "variant_two" in
  let code, _ = Typescript_adapter.Emit.emit_type_decl sir in
  print_endline code;
  [%expect
    {|
    type shape =
      | { tag: "Circle"; payload: number }
      | { tag: "Polygon"; payload: rect };
    |}]
;;

let%expect_test "variant_with_payload" =
  let sir = read_sir "variant_with_payload" in
  let code, _ = Typescript_adapter.Emit.emit_type_decl sir in
  print_endline code;
  [%expect
    {|
    type shape =
      | { tag: "Point"; payload: null }
      | { tag: "Circle"; payload: number }
      | { tag: "Rectangle"; payload: [number, number] }
      | { tag: "Triangle"; payload: { a: number; b: number; c: number } };
    |}]
;;

let%expect_test "variant_with_composite_payload" =
  let sir = read_sir "variant_with_composite_payload" in
  let code, _ = Typescript_adapter.Emit.emit_type_decl sir in
  print_endline code;
  [%expect
    {|
    type shape2 =
      | { tag: "Circle"; payload: number | null };
    |}]
;;

let%expect_test "variant_poly" =
  let sir = read_sir "variant_poly" in
  let code, _ = Typescript_adapter.Emit.emit_type_decl sir in
  print_endline code;
  [%expect
    {|
    type shape_poly<a> =
      | { tag: "Point"; payload: null }
      | { tag: "Circle"; payload: a }
      | { tag: "Rectangle"; payload: [a, a] }
      | { tag: "Triangle"; payload: { a: a; b: a; c: a } };
    |}]
;;

let%expect_test "variant_poly_two_var" =
  let sir = read_sir "variant_poly_two_var" in
  let code, _ = Typescript_adapter.Emit.emit_type_decl sir in
  print_endline code;
  [%expect
    {|
    type container<a, b> =
      | { tag: "Empty"; payload: null }
      | { tag: "Single"; payload: a }
      | { tag: "Pair"; payload: [a, b] }
      | { tag: "Labeled"; payload: { key: a; value: b } }
      | { tag: "Multi"; payload: [a[], b[]] };
    |}]
;;

let%expect_test "nested_generics" =
  let sir = read_sir "nested_generics" in
  let code, _ = Typescript_adapter.Emit.emit_type_decl sir in
  print_endline code;
  [%expect
    {|
    type my_ty =
      | { tag: "My_ty"; payload: tagged<validated<maybe<identity<number>>>> };
    |}]
;;

(*$*)

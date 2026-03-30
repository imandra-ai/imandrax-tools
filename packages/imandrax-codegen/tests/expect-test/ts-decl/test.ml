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
    {|
    type my_ty = {
      x: number;
      y: Option<number>;
      z: number;
    };
    |}]
;;

let%expect_test "record" =
  run_test "record";
  [%expect
    {|
    type point = {
      x: number;
      y: number;
    };
    |}]
;;

let%expect_test "record_with_composite_type" =
  run_test "record_with_composite_type";
  [%expect
    {|
    type shape = {
      circle: Option<number>;
    };
    |}]
;;

let%expect_test "variant_simple" =
  run_test "variant_simple";
  [%expect
    {|
    type color =
      | { tag: "Red"; payload: null }
      | { tag: "Green"; payload: null }
      | { tag: "Blue"; payload: null };
    |}]
;;

let%expect_test "variant_two" =
  run_test "variant_two";
  [%expect
    {|
    type shape =
      | { tag: "Circle"; payload: number }
      | { tag: "Polygon"; payload: rect };
    |}]
;;

let%expect_test "variant_with_payload" =
  run_test "variant_with_payload";
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
  run_test "variant_with_composite_payload";
  [%expect
    {|
    type shape2 =
      | { tag: "Circle"; payload: Option<number> };
    |}]
;;

let%expect_test "variant_poly" =
  run_test "variant_poly";
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
  run_test "variant_poly_two_var";
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
  run_test "nested_generics";
  [%expect
    {|
    type my_ty =
      | { tag: "My_ty"; payload: tagged<validated<maybe<identity<number>>>> };
    |}]
;;

(*$*)

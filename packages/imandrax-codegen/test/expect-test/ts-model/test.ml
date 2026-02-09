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

let read_sir (name : string) : Sir.Value_assignment.t =
  let sir_file = {%string|%{sir_data_dir}/model/%{name}.sexp|} in
  let sir_sexp = CCIO.File.read_exn sir_file in
  Sir.Value_assignment.t_of_sexp (Sexplib.Sexp.of_string sir_sexp)
;;

let run_test : string -> unit = fun name ->
  let sir_val_assignment = read_sir name in
  let code, _ = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
;;

(*$
  let data =
    [ "primitive/int"
    ; "primitive/real"
    ; "primitive/LChar"
    ; "primitive/LString"
    ; "primitive/tuple_of_bool_and_int"
    ; "primitive/record"
    ; "primitive/variant1"
    ; "primitive/variant2"
    ; "primitive/variant3"
    ; "primitive/bool_list"
    ; "primitive/single_element_int_list"
    ; "primitive/empty_list"
    ; "primitive/int_option"
    ; "primitive/int_option_none"
    ; "composite/variant_and_record"
    ; "composite/inline_record"
    ; "composite/map_default_value_only"
    ; "composite/map_int_bool_0"
    ; "composite/map_int_bool_1"
    ; "composite/map_int_bool_2"
    ; "composite/multiset_empty"
    ; "composite/multiset_nonempty"
    ; "composite/set_empty"
    ; "composite/set_nonempty"
    ; "polymorphic/annotated_polymorphic"
    ; "polymorphic/annotated_polymorphic_weird_type_name"
    ; "polymorphic/nested_poly"
    ; "polymorphic/poly_3"
    ]
  in
  print_endline "";
  List.iter
    (fun name ->
      let code =
        [%string
          "let%expect_test \"%{name}\" =
   run_test \"%{name}\";
   [%expect
     {||}]
;;
"]
      in
      print_endline code)
    data
*)
let%expect_test "primitive/int" =
   run_test "primitive/int";
   [%expect
     {| const w: number = 2; |}]
;;

let%expect_test "primitive/real" =
   run_test "primitive/real";
   [%expect
     {| const w: number = 3.14; |}]
;;

let%expect_test "primitive/LChar" =
   run_test "primitive/LChar";
   [%expect
     {| const w: string = "\x00"; |}]
;;

let%expect_test "primitive/LString" =
   run_test "primitive/LString";
   [%expect
     {| const w: string[] = ["h", "i"]; |}]
;;

let%expect_test "primitive/tuple_of_bool_and_int" =
   run_test "primitive/tuple_of_bool_and_int";
   [%expect
     {| const w: [boolean, number] = [true, 2]; |}]
;;

let%expect_test "primitive/record" =
   run_test "primitive/record";
   [%expect
     {| const w: user = { id: 1, active: true }; |}]
;;

let%expect_test "primitive/variant1" =
   run_test "primitive/variant1";
   [%expect
     {| const w: status = { tag: "Active", payload: null }; |}]
;;

let%expect_test "primitive/variant2" =
   run_test "primitive/variant2";
   [%expect
     {| const w: status = { tag: "Waitlist", payload: 1 }; |}]
;;

let%expect_test "primitive/variant3" =
   run_test "primitive/variant3";
   [%expect
     {| const w: status = { tag: "Waitlist", payload: [2, true] }; |}]
;;

let%expect_test "primitive/bool_list" =
   run_test "primitive/bool_list";
   [%expect
     {| const w: boolean[] = [true, false]; |}]
;;

let%expect_test "primitive/single_element_int_list" =
   run_test "primitive/single_element_int_list";
   [%expect
     {| const w: number[] = [1]; |}]
;;

let%expect_test "primitive/empty_list" =
   run_test "primitive/empty_list";
   [%expect
     {|
     type a = unknown;
     const w: a[] = [];
     |}]
;;

let%expect_test "primitive/int_option" =
   run_test "primitive/int_option";
   [%expect
     {| const w: Option<number> = { tag: "Some", payload: 2 }; |}]
;;

let%expect_test "primitive/int_option_none" =
   run_test "primitive/int_option_none";
   [%expect
     {|
     type a = unknown;
     const w: Option<a> = null;
     |}]
;;

let%expect_test "composite/variant_and_record" =
   run_test "composite/variant_and_record";
   [%expect
     {| const w: movement = { tag: "Move", payload: [{ x: 1, y: 2, z: 3. }, { tag: "North", payload: null }] }; |}]
;;

let%expect_test "composite/inline_record" =
   run_test "composite/inline_record";
   [%expect
     {| const w: event = { tag: "Scroll", payload: 2. }; |}]
;;

let%expect_test "composite/map_default_value_only" =
   run_test "composite/map_default_value_only";
   [%expect
     {|
     type a = unknown;
     const w: DefaultMap<a, boolean> = new DefaultMap((() => false), []);
     |}]
;;

let%expect_test "composite/map_int_bool_0" =
   run_test "composite/map_int_bool_0";
   [%expect
     {| const w: DefaultMap<number, boolean> = new DefaultMap((() => false), []); |}]
;;

let%expect_test "composite/map_int_bool_1" =
   run_test "composite/map_int_bool_1";
   [%expect
     {| const w: DefaultMap<number, boolean> = new DefaultMap((() => false), [[2, true] as const]); |}]
;;

let%expect_test "composite/map_int_bool_2" =
   run_test "composite/map_int_bool_2";
   [%expect
     {| const w: DefaultMap<number, boolean> = new DefaultMap((() => false), [[2, true] as const, [3, false] as const]); |}]
;;

let%expect_test "composite/multiset_empty" =
   run_test "composite/multiset_empty";
   [%expect
     {|
     type a = unknown;
     const w: DefaultMap<a, number> = new DefaultMap((() => 0), []);
     |}]
;;

let%expect_test "composite/multiset_nonempty" =
   run_test "composite/multiset_nonempty";
   [%expect
     {| const w: DefaultMap<number, number> = new DefaultMap((() => 0), [[1, 2] as const, [3, 1] as const, [2, 2] as const]); |}]
;;

let%expect_test "composite/set_empty" =
   run_test "composite/set_empty";
   [%expect
     {|
     type a = unknown;
     const w: DefaultMap<a, boolean> = new DefaultMap((() => false), []);
     |}]
;;

let%expect_test "composite/set_nonempty" =
   run_test "composite/set_nonempty";
   [%expect
     {| const w: DefaultMap<number, boolean> = new DefaultMap((() => false), [[1, true] as const, [3, true] as const, [2, true] as const]); |}]
;;

let%expect_test "polymorphic/annotated_polymorphic" =
   run_test "polymorphic/annotated_polymorphic";
   [%expect
     {|
     type a = unknown;
     const w: a[] = [];
     |}]
;;

let%expect_test "polymorphic/annotated_polymorphic_weird_type_name" =
   run_test "polymorphic/annotated_polymorphic_weird_type_name";
   [%expect
     {| const w: _a_0[] = []; |}]
;;

let%expect_test "polymorphic/nested_poly" =
   run_test "polymorphic/nested_poly";
   [%expect
     {| const w: container3<number, number, container2<number, number>> = { v: [1, 2, { v: [3, 4] }] }; |}]
;;

let%expect_test "polymorphic/poly_3" =
   run_test "polymorphic/poly_3";
   [%expect
     {| const w: container3<number, number, number> = { tag: "Three", payload: [1, 2, 3] }; |}]
;;

(*$*)

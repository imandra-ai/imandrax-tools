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

let sir_data_model_dir = sir_data_dir ^ "/model"

let load_sir_model (sub_dir : string) (name : string) : Sir.Value_assignment.t =
  let sub_dir_prefix = if sub_dir = "" then "" else sub_dir ^ "/" in
  let sir_file = sub_dir_prefix ^ name ^ ".sexp" in
  let sir_sexp = CCIO.File.read_exn (sir_data_model_dir ^ "/" ^ sir_file) in
  Sir.Value_assignment.t_of_sexp (Sexplib.Sexp.of_string sir_sexp)
;;

(*$
  let data =
    [ "primitive", "int"
    ; "primitive", "real"
    ; "primitive", "LChar"
    ; "primitive", "LString"
    ; "primitive", "tuple_of_bool_and_int"
    ; "primitive", "record"
    ; "primitive", "variant1"
    ; "primitive", "variant2"
    ; "primitive", "variant3"
    ; "primitive", "bool_list"
    ; "primitive", "single_element_int_list"
    ; "primitive", "empty_list"
    ; "primitive", "int_option"
    ; "primitive", "int_option_none"
    ]
  in
  print_endline "";
  List.iter
    (fun (sub_dir, name) ->
      let code =
        [%string
          "let%expect_test \"%{sub_dir}/%{name}\" =
  let sir_val_assignment = load_sir_model \"%{sub_dir}\" \"%{name}\" in
  let code, _ = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
   [%expect
     {||}]
;;
"]
      in
      print_endline code)
    data
*)
let%expect_test "primitive/int" =
  let sir_val_assignment = load_sir_model "primitive" "int" in
  let code, _ = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
   [%expect
     {| const w: number = 2; |}]
;;

let%expect_test "primitive/real" =
  let sir_val_assignment = load_sir_model "primitive" "real" in
  let code, _ = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
   [%expect
     {| const w: float = 3.14; |}]
;;

let%expect_test "primitive/LChar" =
  let sir_val_assignment = load_sir_model "primitive" "LChar" in
  let code, _ = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
   [%expect
     {| const w: string = "\000"; |}]
;;

let%expect_test "primitive/LString" =
  let sir_val_assignment = load_sir_model "primitive" "LString" in
  let code, _ = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
   [%expect
     {| const w: string[] = ["h", "i"]; |}]
;;

let%expect_test "primitive/tuple_of_bool_and_int" =
  let sir_val_assignment = load_sir_model "primitive" "tuple_of_bool_and_int" in
  let code, _ = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
   [%expect
     {| const w: [boolean, number] = [true, 2]; |}]
;;

let%expect_test "primitive/record" =
  let sir_val_assignment = load_sir_model "primitive" "record" in
  let code, _ = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
   [%expect
     {| const w: user = { id: 1, active: true }; |}]
;;

let%expect_test "primitive/variant1" =
  let sir_val_assignment = load_sir_model "primitive" "variant1" in
  let code, _ = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
   [%expect
     {| const w: status = { tag: "Active", payload: null }; |}]
;;

let%expect_test "primitive/variant2" =
  let sir_val_assignment = load_sir_model "primitive" "variant2" in
  let code, _ = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
   [%expect
     {| const w: status = { tag: "Waitlist", payload: 1 }; |}]
;;

let%expect_test "primitive/variant3" =
  let sir_val_assignment = load_sir_model "primitive" "variant3" in
  let code, _ = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
   [%expect
     {| const w: status = { tag: "Waitlist", payload: [2, true] }; |}]
;;

let%expect_test "primitive/bool_list" =
  let sir_val_assignment = load_sir_model "primitive" "bool_list" in
  let code, _ = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
   [%expect
     {| const w: boolean[] = [true, false]; |}]
;;

let%expect_test "primitive/single_element_int_list" =
  let sir_val_assignment = load_sir_model "primitive" "single_element_int_list" in
  let code, _ = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
   [%expect
     {| const w: number[] = [1]; |}]
;;

let%expect_test "primitive/empty_list" =
  let sir_val_assignment = load_sir_model "primitive" "empty_list" in
  let code, _ = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
   [%expect
     {| const w: _a_0[] = []; |}]
;;

let%expect_test "primitive/int_option" =
  let sir_val_assignment = load_sir_model "primitive" "int_option" in
  let code, _ = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
   [%expect
     {| const w: number | null = { tag: "Some", payload: 2 }; |}]
;;

let%expect_test "primitive/int_option_none" =
  let sir_val_assignment = load_sir_model "primitive" "int_option_none" in
  let code, _ = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
   [%expect
     {| const w: _a_0 | null = null; |}]
;;

(*$*)

module Sir = Semantic_ir

let workspace_dir = "../../.."

let art_data_dir =
  let art_data_dir_rel_to_workspace = Sys.getenv "TEST_DATA_SIR_DIR" in
  workspace_dir ^ "/" ^ art_data_dir_rel_to_workspace

let art_data_model_dir = art_data_dir ^ "/model"

let load_sir_model (sub_dir : string) (name : string) : Sir.Value_assignment.t =
  let sub_dir_prefix = if sub_dir = "" then "" else sub_dir ^ "/" in
  let sir_file = sub_dir_prefix ^ name ^ ".sexp" in
  let sir_sexp = CCIO.File.read_exn (art_data_model_dir ^ "/" ^ sir_file) in
  Sir.Value_assignment.t_of_sexp (Sexplib.Sexp.of_string sir_sexp)

(*$
let data = [ ("primitive", "bool_list")
            ; ("primitive", "empty_list")
            ; ("primitive", "int")
            ] in
print_endline "";
List.iter (fun (sub_dir, name) ->
  let test_name = sub_dir ^ "/" ^ name in
  let code = [%string
    "let%expect_test \"%{test_name}\" =
let sir_val_assignment = load_sir_model \"%{sub_dir}\" \"%{name}\" in
let code = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
[%expect {||}];
;;
  " ] in
  print_endline code
) data;
*)

let%expect_test "primitive/bool_list" =
let sir_val_assignment = load_sir_model "primitive" "bool_list" in
let code = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
[%expect {| const w: boolean[] = [true, false]; |}];
;;


let%expect_test "primitive/empty_list" =
let sir_val_assignment = load_sir_model "primitive" "empty_list" in
let code = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
[%expect {| const w: _a_0[] = []; |}];
;;


let%expect_test "primitive/int" =
let sir_val_assignment = load_sir_model "primitive" "int" in
let code = Typescript_adapter.Emit.emit_value_assignment sir_val_assignment in
  print_endline code;
[%expect {| const w: number = 2; |}];
;;

(*$*)

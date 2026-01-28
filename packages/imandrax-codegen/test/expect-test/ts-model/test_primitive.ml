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


let%expect_test "bool list" =
  let bool_list = load_sir_model "primitive" "bool_list" in
  let code = Typescript_adapter.Emit.emit_value_assignment (bool_list) in
    print_endline code;
 [%expect {| const w: boolean[] = [true, false]; |}];

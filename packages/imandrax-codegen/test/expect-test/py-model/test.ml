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
    ; "composite", "multiset_empty"
    ]
  in
  print_endline "";
  List.iter
    (fun (sub_dir, name) ->
      let code =
        [%string
          "let%expect_test \"%{sub_dir}/%{name}\" =
  let sir_val_assignment = load_sir_model \"%{sub_dir}\" \"%{name}\" in
  let stmts = Python_adapter.Transform.ast_stmts_of_sir_value_assignment sir_val_assignment in
  List.iter (fun stmt -> print_endline (Python_adapter.Ast.sexp_of_stmt stmt |> Sexplib.Sexp.to_string_hum)) stmts;
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
  let stmts = Python_adapter.Transform.ast_stmts_of_sir_value_assignment sir_val_assignment in
  List.iter (fun stmt -> print_endline (Python_adapter.Ast.sexp_of_stmt stmt |> Sexplib.Sexp.to_string_hum)) stmts;
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation (Name ((id int) (ctx Load))))
       (value ((Constant ((value (Int 2)) (kind ()))))) (simple 1)))
     |}]
;;

let%expect_test "primitive/real" =
  let sir_val_assignment = load_sir_model "primitive" "real" in
  let stmts = Python_adapter.Transform.ast_stmts_of_sir_value_assignment sir_val_assignment in
  List.iter (fun stmt -> print_endline (Python_adapter.Ast.sexp_of_stmt stmt |> Sexplib.Sexp.to_string_hum)) stmts;
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation (Name ((id real) (ctx Load))))
       (value ((Constant ((value (Float 3.14)) (kind ()))))) (simple 1)))
     |}]
;;

let%expect_test "primitive/LChar" =
  let sir_val_assignment = load_sir_model "primitive" "LChar" in
  let stmts = Python_adapter.Transform.ast_stmts_of_sir_value_assignment sir_val_assignment in
  List.iter (fun stmt -> print_endline (Python_adapter.Ast.sexp_of_stmt stmt |> Sexplib.Sexp.to_string_hum)) stmts;
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation (Name ((id str) (ctx Load))))
       (value ((Constant ((value (String "\000")) (kind ()))))) (simple 1)))
     |}]
;;

let%expect_test "primitive/LString" =
  let sir_val_assignment = load_sir_model "primitive" "LString" in
  let stmts = Python_adapter.Transform.ast_stmts_of_sir_value_assignment sir_val_assignment in
  List.iter (fun stmt -> print_endline (Python_adapter.Ast.sexp_of_stmt stmt |> Sexplib.Sexp.to_string_hum)) stmts;
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation
        (Subscript
         ((value (Name ((id list) (ctx Load))))
          (slice (Name ((id str) (ctx Load)))) (ctx Load))))
       (value
        ((List
          ((elts
            ((Constant ((value (String h)) (kind ())))
             (Constant ((value (String i)) (kind ())))))
           (ctx Load)))))
       (simple 1)))
     |}]
;;

let%expect_test "primitive/tuple_of_bool_and_int" =
  let sir_val_assignment = load_sir_model "primitive" "tuple_of_bool_and_int" in
  let stmts = Python_adapter.Transform.ast_stmts_of_sir_value_assignment sir_val_assignment in
  List.iter (fun stmt -> print_endline (Python_adapter.Ast.sexp_of_stmt stmt |> Sexplib.Sexp.to_string_hum)) stmts;
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation
        (Subscript
         ((value (Name ((id tuple) (ctx Load))))
          (slice
           (Tuple
            ((elts ((Name ((id bool) (ctx Load))) (Name ((id int) (ctx Load)))))
             (ctx Load) (dims ()))))
          (ctx Load))))
       (value
        ((Tuple
          ((elts
            ((Constant ((value (Bool true)) (kind ())))
             (Constant ((value (Int 2)) (kind ())))))
           (ctx Load) (dims ())))))
       (simple 1)))
     |}]
;;

let%expect_test "primitive/record" =
  let sir_val_assignment = load_sir_model "primitive" "record" in
  let stmts = Python_adapter.Transform.ast_stmts_of_sir_value_assignment sir_val_assignment in
  List.iter (fun stmt -> print_endline (Python_adapter.Ast.sexp_of_stmt stmt |> Sexplib.Sexp.to_string_hum)) stmts;
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation (Name ((id user) (ctx Load))))
       (value
        ((Call
          ((func (Name ((id user) (ctx Load)))) (args ())
           (keywords
            (((arg (id)) (value (Constant ((value (Int 1)) (kind ())))))
             ((arg (active)) (value (Constant ((value (Bool true)) (kind ())))))))))))
       (simple 1)))
     |}]
;;

let%expect_test "primitive/variant1" =
  let sir_val_assignment = load_sir_model "primitive" "variant1" in
  let stmts = Python_adapter.Transform.ast_stmts_of_sir_value_assignment sir_val_assignment in
  List.iter (fun stmt -> print_endline (Python_adapter.Ast.sexp_of_stmt stmt |> Sexplib.Sexp.to_string_hum)) stmts;
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation (Name ((id status) (ctx Load))))
       (value
        ((Call ((func (Name ((id Active) (ctx Load)))) (args ()) (keywords ())))))
       (simple 1)))
     |}]
;;

let%expect_test "primitive/variant2" =
  let sir_val_assignment = load_sir_model "primitive" "variant2" in
  let stmts = Python_adapter.Transform.ast_stmts_of_sir_value_assignment sir_val_assignment in
  List.iter (fun stmt -> print_endline (Python_adapter.Ast.sexp_of_stmt stmt |> Sexplib.Sexp.to_string_hum)) stmts;
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation (Name ((id status) (ctx Load))))
       (value
        ((Call
          ((func (Name ((id Waitlist) (ctx Load))))
           (args ((Constant ((value (Int 1)) (kind ()))))) (keywords ())))))
       (simple 1)))
     |}]
;;

let%expect_test "primitive/variant3" =
  let sir_val_assignment = load_sir_model "primitive" "variant3" in
  let stmts = Python_adapter.Transform.ast_stmts_of_sir_value_assignment sir_val_assignment in
  List.iter (fun stmt -> print_endline (Python_adapter.Ast.sexp_of_stmt stmt |> Sexplib.Sexp.to_string_hum)) stmts;
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation (Name ((id status) (ctx Load))))
       (value
        ((Call
          ((func (Name ((id Waitlist) (ctx Load))))
           (args
            ((Constant ((value (Int 2)) (kind ())))
             (Constant ((value (Bool true)) (kind ())))))
           (keywords ())))))
       (simple 1)))
     |}]
;;

let%expect_test "primitive/bool_list" =
  let sir_val_assignment = load_sir_model "primitive" "bool_list" in
  let stmts = Python_adapter.Transform.ast_stmts_of_sir_value_assignment sir_val_assignment in
  List.iter (fun stmt -> print_endline (Python_adapter.Ast.sexp_of_stmt stmt |> Sexplib.Sexp.to_string_hum)) stmts;
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation
        (Subscript
         ((value (Name ((id list) (ctx Load))))
          (slice (Name ((id bool) (ctx Load)))) (ctx Load))))
       (value
        ((List
          ((elts
            ((Constant ((value (Bool true)) (kind ())))
             (Constant ((value (Bool false)) (kind ())))))
           (ctx Load)))))
       (simple 1)))
     |}]
;;

let%expect_test "primitive/single_element_int_list" =
  let sir_val_assignment = load_sir_model "primitive" "single_element_int_list" in
  let stmts = Python_adapter.Transform.ast_stmts_of_sir_value_assignment sir_val_assignment in
  List.iter (fun stmt -> print_endline (Python_adapter.Ast.sexp_of_stmt stmt |> Sexplib.Sexp.to_string_hum)) stmts;
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation
        (Subscript
         ((value (Name ((id list) (ctx Load))))
          (slice (Name ((id int) (ctx Load)))) (ctx Load))))
       (value
        ((List ((elts ((Constant ((value (Int 1)) (kind ()))))) (ctx Load)))))
       (simple 1)))
     |}]
;;

let%expect_test "primitive/empty_list" =
  let sir_val_assignment = load_sir_model "primitive" "empty_list" in
  let stmts = Python_adapter.Transform.ast_stmts_of_sir_value_assignment sir_val_assignment in
  List.iter (fun stmt -> print_endline (Python_adapter.Ast.sexp_of_stmt stmt |> Sexplib.Sexp.to_string_hum)) stmts;
   [%expect
     {|
     (Assign
      ((targets ((Name ((id a) (ctx Load)))))
       (value
        (Call
         ((func (Name ((id TypeVar) (ctx Load))))
          (args ((Constant ((value (String a)) (kind ()))))) (keywords ()))))
       (type_comment ())))
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation
        (Subscript
         ((value (Name ((id list) (ctx Load)))) (slice (Name ((id a) (ctx Load))))
          (ctx Load))))
       (value ((List ((elts ()) (ctx Load))))) (simple 1)))
     |}]
;;

let%expect_test "primitive/int_option" =
  let sir_val_assignment = load_sir_model "primitive" "int_option" in
  let stmts = Python_adapter.Transform.ast_stmts_of_sir_value_assignment sir_val_assignment in
  List.iter (fun stmt -> print_endline (Python_adapter.Ast.sexp_of_stmt stmt |> Sexplib.Sexp.to_string_hum)) stmts;
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation
        (Subscript
         ((value (Name ((id option) (ctx Load))))
          (slice (Name ((id int) (ctx Load)))) (ctx Load))))
       (value
        ((Call
          ((func (Name ((id Some) (ctx Load))))
           (args ((Constant ((value (Int 2)) (kind ()))))) (keywords ())))))
       (simple 1)))
     |}]
;;

let%expect_test "primitive/int_option_none" =
  let sir_val_assignment = load_sir_model "primitive" "int_option_none" in
  let stmts = Python_adapter.Transform.ast_stmts_of_sir_value_assignment sir_val_assignment in
  List.iter (fun stmt -> print_endline (Python_adapter.Ast.sexp_of_stmt stmt |> Sexplib.Sexp.to_string_hum)) stmts;
   [%expect
     {|
     (Assign
      ((targets ((Name ((id a) (ctx Load)))))
       (value
        (Call
         ((func (Name ((id TypeVar) (ctx Load))))
          (args ((Constant ((value (String a)) (kind ()))))) (keywords ()))))
       (type_comment ())))
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation
        (Subscript
         ((value (Name ((id option) (ctx Load))))
          (slice (Name ((id a) (ctx Load)))) (ctx Load))))
       (value ((Constant ((value Unit) (kind ()))))) (simple 1)))
     |}]
;;

let%expect_test "composite/multiset_empty" =
  let sir_val_assignment = load_sir_model "composite" "multiset_empty" in
  let stmts = Python_adapter.Transform.ast_stmts_of_sir_value_assignment sir_val_assignment in
  List.iter (fun stmt -> print_endline (Python_adapter.Ast.sexp_of_stmt stmt |> Sexplib.Sexp.to_string_hum)) stmts;
   [%expect
     {|
     (Assign
      ((targets ((Name ((id a) (ctx Load)))))
       (value
        (Call
         ((func (Name ((id TypeVar) (ctx Load))))
          (args ((Constant ((value (String a)) (kind ()))))) (keywords ()))))
       (type_comment ())))
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation
        (Subscript
         ((value (Name ((id defaultdict) (ctx Load))))
          (slice
           (Tuple
            ((elts ((Name ((id a) (ctx Load))) (Name ((id int) (ctx Load)))))
             (ctx Load) (dims ()))))
          (ctx Load))))
       (value
        ((Call
          ((func (Name ((id defaultdict) (ctx Load))))
           (args
            ((Lambda
              ((args
                ((posonlyargs ()) (args ()) (vararg ()) (kwonlyargs ())
                 (kw_defaults ()) (kwarg ()) (defaults ())))
               (body (Constant ((value (Int 0)) (kind ()))))))))
           (keywords ())))))
       (simple 1)))
     |}]
;;

(*$*)

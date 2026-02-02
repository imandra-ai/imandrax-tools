
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

let read_sir (name : string) : Sir.Value_assignment.t =
  let sir_file = {%string|%{sir_data_dir}/model/%{name}.sexp|} in
  let sir_sexp = CCIO.File.read_exn sir_file in
  Sir.Value_assignment.t_of_sexp (Sexplib.Sexp.of_string sir_sexp)
;;

let run_test : string -> unit = fun name ->
  let sir_val_assignment = read_sir name in
  let stmts = Python_adapter.Transform.ast_stmts_of_sir_value_assignment sir_val_assignment in
  List.iter (fun stmt -> print_endline (Python_adapter.Ast.sexp_of_stmt stmt |> Sexplib.Sexp.to_string_hum)) stmts;
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
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation (Name ((id int) (ctx Load))))
       (value ((Constant ((value (Int 2)) (kind ()))))) (simple 1)))
     |}]
;;

let%expect_test "primitive/real" =
   run_test "primitive/real";
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation (Name ((id real) (ctx Load))))
       (value ((Constant ((value (Float 3.14)) (kind ()))))) (simple 1)))
     |}]
;;

let%expect_test "primitive/LChar" =
   run_test "primitive/LChar";
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation (Name ((id str) (ctx Load))))
       (value ((Constant ((value (String "\000")) (kind ()))))) (simple 1)))
     |}]
;;

let%expect_test "primitive/LString" =
   run_test "primitive/LString";
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
   run_test "primitive/tuple_of_bool_and_int";
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
   run_test "primitive/record";
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
   run_test "primitive/variant1";
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
   run_test "primitive/variant2";
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
   run_test "primitive/variant3";
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
   run_test "primitive/bool_list";
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
   run_test "primitive/single_element_int_list";
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
   run_test "primitive/empty_list";
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
   run_test "primitive/int_option";
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
   run_test "primitive/int_option_none";
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

let%expect_test "composite/variant_and_record" =
   run_test "composite/variant_and_record";
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation (Name ((id movement) (ctx Load))))
       (value
        ((Call
          ((func (Name ((id Move) (ctx Load))))
           (args
            ((Call
              ((func (Name ((id position) (ctx Load)))) (args ())
               (keywords
                (((arg (x)) (value (Constant ((value (Int 1)) (kind ())))))
                 ((arg (y)) (value (Constant ((value (Int 2)) (kind ())))))
                 ((arg (z)) (value (Constant ((value (Float 3)) (kind ())))))))))
             (Call
              ((func (Name ((id North) (ctx Load)))) (args ()) (keywords ())))))
           (keywords ())))))
       (simple 1)))
     |}]
;;

let%expect_test "composite/inline_record" =
   run_test "composite/inline_record";
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation (Name ((id event) (ctx Load))))
       (value
        ((Call
          ((func (Name ((id Scroll) (ctx Load))))
           (args ((Constant ((value (Float 2)) (kind ()))))) (keywords ())))))
       (simple 1)))
     |}]
;;

let%expect_test "composite/map_default_value_only" =
   run_test "composite/map_default_value_only";
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
            ((elts ((Name ((id a) (ctx Load))) (Name ((id bool) (ctx Load)))))
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
               (body (Constant ((value (Bool false)) (kind ()))))))))
           (keywords ())))))
       (simple 1)))
     |}]
;;

let%expect_test "composite/map_int_bool_0" =
   run_test "composite/map_int_bool_0";
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation
        (Subscript
         ((value (Name ((id defaultdict) (ctx Load))))
          (slice
           (Tuple
            ((elts ((Name ((id int) (ctx Load))) (Name ((id bool) (ctx Load)))))
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
               (body (Constant ((value (Bool false)) (kind ()))))))))
           (keywords ())))))
       (simple 1)))
     |}]
;;

let%expect_test "composite/map_int_bool_1" =
   run_test "composite/map_int_bool_1";
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation
        (Subscript
         ((value (Name ((id defaultdict) (ctx Load))))
          (slice
           (Tuple
            ((elts ((Name ((id int) (ctx Load))) (Name ((id bool) (ctx Load)))))
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
               (body (Constant ((value (Bool false)) (kind ()))))))
             (Dict
              ((keys (((Constant ((value (Int 2)) (kind ()))))))
               (values ((Constant ((value (Bool true)) (kind ())))))))))
           (keywords ())))))
       (simple 1)))
     |}]
;;

let%expect_test "composite/map_int_bool_2" =
   run_test "composite/map_int_bool_2";
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation
        (Subscript
         ((value (Name ((id defaultdict) (ctx Load))))
          (slice
           (Tuple
            ((elts ((Name ((id int) (ctx Load))) (Name ((id bool) (ctx Load)))))
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
               (body (Constant ((value (Bool false)) (kind ()))))))
             (Dict
              ((keys
                (((Constant ((value (Int 2)) (kind ()))))
                 ((Constant ((value (Int 3)) (kind ()))))))
               (values
                ((Constant ((value (Bool true)) (kind ())))
                 (Constant ((value (Bool false)) (kind ())))))))))
           (keywords ())))))
       (simple 1)))
     |}]
;;

let%expect_test "composite/multiset_empty" =
   run_test "composite/multiset_empty";
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

let%expect_test "composite/multiset_nonempty" =
   run_test "composite/multiset_nonempty";
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation
        (Subscript
         ((value (Name ((id defaultdict) (ctx Load))))
          (slice
           (Tuple
            ((elts ((Name ((id int) (ctx Load))) (Name ((id int) (ctx Load)))))
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
               (body (Constant ((value (Int 0)) (kind ()))))))
             (Dict
              ((keys
                (((Constant ((value (Int 1)) (kind ()))))
                 ((Constant ((value (Int 3)) (kind ()))))
                 ((Constant ((value (Int 2)) (kind ()))))))
               (values
                ((Constant ((value (Int 2)) (kind ())))
                 (Constant ((value (Int 1)) (kind ())))
                 (Constant ((value (Int 2)) (kind ())))))))))
           (keywords ())))))
       (simple 1)))
     |}]
;;

let%expect_test "composite/set_empty" =
   run_test "composite/set_empty";
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
            ((elts ((Name ((id a) (ctx Load))) (Name ((id bool) (ctx Load)))))
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
               (body (Constant ((value (Bool false)) (kind ()))))))))
           (keywords ())))))
       (simple 1)))
     |}]
;;

let%expect_test "composite/set_nonempty" =
   run_test "composite/set_nonempty";
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation
        (Subscript
         ((value (Name ((id defaultdict) (ctx Load))))
          (slice
           (Tuple
            ((elts ((Name ((id int) (ctx Load))) (Name ((id bool) (ctx Load)))))
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
               (body (Constant ((value (Bool false)) (kind ()))))))
             (Dict
              ((keys
                (((Constant ((value (Int 1)) (kind ()))))
                 ((Constant ((value (Int 3)) (kind ()))))
                 ((Constant ((value (Int 2)) (kind ()))))))
               (values
                ((Constant ((value (Bool true)) (kind ())))
                 (Constant ((value (Bool true)) (kind ())))
                 (Constant ((value (Bool true)) (kind ())))))))))
           (keywords ())))))
       (simple 1)))
     |}]
;;

let%expect_test "polymorphic/annotated_polymorphic" =
   run_test "polymorphic/annotated_polymorphic";
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

let%expect_test "polymorphic/annotated_polymorphic_weird_type_name" =
   run_test "polymorphic/annotated_polymorphic_weird_type_name";
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation
        (Subscript
         ((value (Name ((id list) (ctx Load))))
          (slice (Name ((id _a_0) (ctx Load)))) (ctx Load))))
       (value ((List ((elts ()) (ctx Load))))) (simple 1)))
     |}]
;;

let%expect_test "polymorphic/nested_poly" =
   run_test "polymorphic/nested_poly";
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation
        (Subscript
         ((value (Name ((id container3) (ctx Load))))
          (slice
           (Tuple
            ((elts
              ((Name ((id int) (ctx Load))) (Name ((id int) (ctx Load)))
               (Subscript
                ((value (Name ((id container2) (ctx Load))))
                 (slice
                  (Tuple
                   ((elts
                     ((Name ((id int) (ctx Load))) (Name ((id int) (ctx Load)))))
                    (ctx Load) (dims ()))))
                 (ctx Load)))))
             (ctx Load) (dims ()))))
          (ctx Load))))
       (value
        ((Call
          ((func (Name ((id container3) (ctx Load)))) (args ())
           (keywords
            (((arg (v))
              (value
               (Tuple
                ((elts
                  ((Constant ((value (Int 1)) (kind ())))
                   (Constant ((value (Int 2)) (kind ())))
                   (Call
                    ((func (Name ((id container2) (ctx Load)))) (args ())
                     (keywords
                      (((arg (v))
                        (value
                         (Tuple
                          ((elts
                            ((Constant ((value (Int 3)) (kind ())))
                             (Constant ((value (Int 4)) (kind ())))))
                           (ctx Load) (dims ())))))))))))
                 (ctx Load) (dims ())))))))))))
       (simple 1)))
     |}]
;;

let%expect_test "polymorphic/poly_3" =
   run_test "polymorphic/poly_3";
   [%expect
     {|
     (AnnAssign
      ((target (Name ((id w) (ctx Load))))
       (annotation
        (Subscript
         ((value (Name ((id container3) (ctx Load))))
          (slice
           (Tuple
            ((elts
              ((Name ((id int) (ctx Load))) (Name ((id int) (ctx Load)))
               (Name ((id int) (ctx Load)))))
             (ctx Load) (dims ()))))
          (ctx Load))))
       (value
        ((Call
          ((func (Name ((id Three) (ctx Load))))
           (args
            ((Constant ((value (Int 1)) (kind ())))
             (Constant ((value (Int 2)) (kind ())))
             (Constant ((value (Int 3)) (kind ())))))
           (keywords ())))))
       (simple 1)))
     |}]
;;

(*$*)

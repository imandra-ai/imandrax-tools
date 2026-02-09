(** Transform Semantic IR to Python AST *)

open Ast
module Sir = Semantic_ir

val annot_of_sir_type_expr : Sir.type_expr -> Ast.expr
val stmts_of_type_params : string list -> stmt list
val ast_stmts_of_sir_value_assignment : Sir.Value_assignment.t -> Ast.stmt list
val stmts_of_sir_type_decl : Sir.type_decl -> Ast.stmt list
val test_func_def_of_test_decl : Sir.test_decl -> Ast.stmt
val test_data_dict_of_test_suite : Sir.test_suite -> Ast.stmt

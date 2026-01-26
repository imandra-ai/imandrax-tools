(** Transform Semantic IR to Python AST *)

open Ast
module Sir = Semantic_ir.Types

(* TODO: is this really needed to be exposed? *)
val annot_of_sir_type_expr : Sir.type_expr -> Ast.expr
val ast_expr_of_sir_value : Sir.value -> Ast.expr
val stmts_of_sir_type_decl : Sir.type_decl -> Ast.stmt list
val test_func_def_of_test_decl : Sir.test_decl -> Ast.stmt

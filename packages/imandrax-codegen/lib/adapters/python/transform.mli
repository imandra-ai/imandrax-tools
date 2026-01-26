(** Transform Semantic IR to Python AST *)

open Ast
module Sir = Semantic_ir.Types

(* Primitive *)
(* TODO: is this really needed to be exposed? *)
val ast_type_expr_of_sir : Sir.type_expr -> Ast.type_expr
val ast_expr_of_sir_value : Sir.value -> Ast.expr

(* Composite *)
val annot_of_sir_type_expr : Sir.type_expr -> Ast.expr
val stmts_of_sir_type_decl : Sir.type_decl -> Ast.stmt list

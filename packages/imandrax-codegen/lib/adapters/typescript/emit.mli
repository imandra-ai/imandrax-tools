(** Emit TypeScript source code from Semantic IR *)

module Sir = Semantic_ir
module Extra_imports = Config.Extra_imports

(** {1 Type Expressions} *)

(** Emit a SIR type expression as TypeScript type annotation *)
val emit_type_expr : Sir.type_expr -> string * Extra_imports.t

(** Emit type parameters as TypeScript generics: [<a, b>] *)
val emit_type_params : string list -> string

(** {1 Values} *)

(** Emit a SIR constant as TypeScript literal *)
val emit_const : Sir.const_value -> string * Extra_imports.t

(** Emit a SIR value as TypeScript expression *)
val emit_value : Sir.value -> string * Extra_imports.t

(** {1 Type Declarations} *)

(** Emit variant constructor fields as TypeScript payload type *)
val emit_variant_payload : Sir.Variant_field.t list -> string * Extra_imports.t

(** Emit a variant constructor as a TypeScript union member *)
val emit_variant_constructor : Sir.variant_constructor -> string * Extra_imports.t

(** Emit a SIR type declaration as TypeScript source *)
val emit_type_decl : Sir.type_decl -> string * Extra_imports.t

(** {1 Statements} *)

(** Emit a value assignment as TypeScript const declaration *)
val emit_value_assignment : Sir.Value_assignment.t -> string * Extra_imports.t

(** {1 Test Generation} *)

(** Emit a test suite as TypeScript test data object *)
val emit_test_suite_dict : Sir.test_suite -> string * Extra_imports.t

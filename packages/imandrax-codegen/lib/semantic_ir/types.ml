(** Language-neutral Semantic IR types for representing code-generation-related ImandraX MIR
  - lightweight, intuitive
*)

(* Type
==================== *)

(** Type expressions *)
type type_expr =
  | TBase of string  (** Base type: int, bool, MyType *)
  | TVar of string  (** Type variable: 'a, 'b *)
  | TApp of string * type_expr list
      (** Type application: list[int], option['a] *)
  | TTuple of type_expr list  (** Tuple type: (int, bool) *)
  | TArrow of type_expr * type_expr  (** Function type: int -> bool *)
[@@deriving show, eq, yojson]

(** Variant constructor field types *)
type variant_field =
  | Positional of type_expr  (** Positional field. came from inline record *)
  | Named of string * type_expr  (** Named field: name: type *)
[@@deriving show, eq, yojson]

let variant_field_type_expr (field : variant_field) : type_expr =
  match field with
  | Positional ty -> ty
  | Named (_, ty) -> ty

let type_var_names_of_type_expr (ty_expr : type_expr) : string list =
  let rec helper (acc: string list) (ty_expr : type_expr) : string list =
    match ty_expr with
    | TVar ty_name -> [ty_name] @ acc
    | TApp (_, args) -> CCList.fold_left helper acc args
    | TTuple args -> CCList.fold_left helper acc args
    | TArrow (arg, ret) -> helper acc arg @ helper acc ret
    | _ -> []
  in helper [] ty_expr

type variant_constructor = {
  vc_name : string;  (** Constructor name, aka tag *)
  vc_fields : variant_field list;  (** Constructor fields *)
}
[@@deriving show, eq, yojson]
(** Variant constructor definition *)

type record_field = {
  rf_name : string;  (** Field name *)
  rf_type : type_expr;  (** Field type *)
}
[@@deriving show, eq, yojson]
(** Record field definition *)

(** Type declarations *)
type type_decl =
  | Variant of {
      name : string;  (** Type name *)
      type_params : string list;  (** Type parameters: ['a, 'b] *)
      constructors : variant_constructor list;  (** Variant constructors *)
    }
  | Record of {
      name : string;  (** Type name *)
      type_params : string list;  (** Type parameters: ['a, 'b] *)
      fields : record_field list;  (** Record fields *)
    }
  | Alias of {
      name : string;  (** Type name *)
      type_params : string list;  (** Type parameters: ['a, 'b] *)
      target : type_expr;  (** Aliased type *)
    }
[@@deriving show, eq, yojson]

(** Constant values *)
type const_value =
  | CInt of int  (** Integer constant *)
  | CFloat of float  (** Float constant *)
  | CBool of bool  (** Boolean constant *)
  | CString of string  (** String constant *)
  | CChar of char  (** Character constant *)
  | CUnit  (** Unit constant *)
[@@deriving show, eq, yojson]

(** Binary operators *)
type bin_op =
  | Add  (** Addition: + *)
  | Sub  (** Subtraction: - *)
  | Mult  (** Multiplication: * *)
  | Div  (** Division: / *)
  | Eq  (** Equality: = *)
  | Lt  (** Less than: < *)
  | Gt  (** Greater than: > *)
  | And  (** Logical and: && *)
  | Or  (** Logical or: || *)
[@@deriving show, eq, yojson]

(* Value
==================== *)

(** Values *)
type value =
  | VConst of const_value  (** Constant value *)
  | VTuple of value list  (** Tuple value *)
  | VList of value list  (** List value *)
  | VRecord of {
      type_name : string;  (** Record type name *)
      fields : (string * value) list;  (** Field name-value pairs *)
    }
  | VConstruct of {
      constructor : string;  (** Constructor name *)
      args : value list;  (** Constructor arguments *)
    }
  | VName of string  (** Variable reference *)
  | VBinOp of value * bin_op * value  (** Binary operation *)
  | VIfThenElse of value * value * value  (** Conditional expression *)
  | VMap of {
      default : value;  (** Default value *)
      entries : (value * value) list;  (** Map entries: (key, value) pairs *)
    }
[@@deriving show, eq, yojson]

(* Test declaration
==================== *)

(** One test *)
type test_decl = {
  name : string;
  f_name : string;
  f_args : (string * type_expr * value) list;
  f_output : type_expr * value;
  docstr : string;
}

(** Test suite *)
type test_suite = test_decl list

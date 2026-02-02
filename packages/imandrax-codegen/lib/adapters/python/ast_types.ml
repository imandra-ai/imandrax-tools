open Sexplib.Std

let sexp_of_bytes b = Sexplib.Sexp.Atom (Bytes.to_string b)
let bytes_of_sexp = function
  | Sexplib.Sexp.Atom s -> Bytes.of_string s
  | sexp -> Sexplib.Conv.of_sexp_error "bytes_of_sexp: expected Atom" sexp

type location =
  { lineno : int
  ; col_offset : int
  ; end_lineno : int option
  ; end_col_offset : int option
  }
[@@deriving show]

type ast =
  | Keyword of keyword
  | Expr of expr
  | Module of module_ast
  | Arguments of arguments
  | Arg of arg
  | TypeParam of type_param
  | Alias of alias

and alias =
  { name : string
  ; asname : string option
  }

and module_ast = { body : stmt list (* type_ignores: list[TypeIgnore] *) }

and keyword =
  { (* location: location; *)
    arg : string option
  ; value : expr
  }

(* Expressions
==================== *)
and expr =
  | Constant of constant
  | BoolOp of bool_op
  | BinOp of bin_op
  | UnaryOp of unary_op
  | IfExp of (expr * expr * expr)
  | Set of expr list
  | ListComp of list_comp
  | List of list_expr
  | Tuple of tuple_expr
  | Name of name_expr
  | Attribute of attribute_expr
  | Subscript of subscript_expr
  | Call of call_expr
  | Lambda of lambda_expr
  | Dict of dict_expr
  | Compare of
      { left : expr
      ; ops : cmpop list
      ; comparators : expr list
      }

and constant =
  { value : constant_value
  ; kind : string option
  }

and constant_value =
  | String of string
  | Bytes of bytes
  | Bool of bool
  | Int of int
  | Float of float
  | Unit
(* TODO: complex and EllipsisType *)

and bool_op =
  | And
  | Or

and bin_op =
  { left : expr
  ; op : operator
  ; right : expr
  }

and operator =
  | Add
  | Sub
  | Mult
  | MatMult
  | Div
  | Mod
  | Pow
  | LShift
  | RShift
  | BitOr
  | BitXor
  | BitAnd
  | FloorDiv

and unary_op =
  | Invert
  | Not
  | UAdd
  | USub

and cmpop =
  | Eq
  | NotEq
  | Lt
  | LtE
  | Gt
  | GtE
  | Is
  | IsNot
  | In
  | NotIn

and comprehension =
  { target : expr
  ; iter : expr
  ; ifs : expr list
  ; is_async : int
  }

and list_comp =
  { elt : expr
  ; generators : comprehension list
  }

and expr_context =
  | Load
  | Store
  | Del

and list_expr =
  { elts : expr list
  ; ctx : expr_context
  }

and tuple_expr =
  { elts : expr list
  ; ctx : expr_context
  ; (* contains type parameter expressions (like T, U in generic syntax) *)
    dims : expr list
  }

and name_expr =
  { id : string
  ; ctx : expr_context
  }

and attribute_expr =
  { value : expr
  ; attr : string
  ; ctx : expr_context
  }

and subscript_expr =
  { value : expr
  ; slice : expr
  ; ctx : expr_context
  }

and call_expr =
  { func : expr
  ; args : expr list
  ; keywords : keyword list
  }

and lambda_expr =
  { args : arguments
  ; body : expr
  }

and dict_expr =
  { keys : expr option list
  ; values : expr list
  }

(* Statements
==================== *)
and stmt =
  | FunctionDef of function_def_stmt
  | Assign of assign_stmt
  | AnnAssign of ann_assign_stmt
  | ClassDef of class_def_stmt
  (* 801 *)
  | Pass
  | Assert of
      { test : expr
      ; msg : expr option
      }
  | Import of { names : alias list }
  | ImportFrom of { names : alias list }
  (* NOTE: the name is different from Python's `Expr :< stmt` statement *)
  | ExprStmt of { value : expr }

and function_def_stmt =
  { name : string
  ; args : arguments
  ; body : stmt list
  ; decorator_list : expr list
  ; returns : expr option
  ; type_comment : string option
  ; type_params : type_param list
  }

and assign_stmt =
  { targets : expr list
  ; value : expr
  ; type_comment : string option
  }

and ann_assign_stmt =
  { target : expr
  ; annotation : expr
  ; value : expr option
  ; simple : int
  }

and class_def_stmt =
  { name : string
  ; bases : expr list
  ; keywords : keyword list
  ; (* capture arguments like metaclass specification and other keyword arguments
     passed to `__init_sublcass__` or the metaclass
  *)
    body : stmt list
  ; decorator_list : expr list
  }

and arguments =
  { posonlyargs : arg list
  ; args : arg list
  ; vararg : arg option
  ; kwonlyargs : arg list
  ; kw_defaults : expr option list
  ; kwarg : arg option
  ; defaults : expr list
  }

and arg =
  { arg : string
  ; annotation : expr option
  ; type_comment : string option
  }

(* Type params
==================== *)
and type_param =
  | TypeVar of type_var
  | ParamSepc of
      { name : string
      ; default_value : expr option
      }
  | TypeVarTuple of
      { name : string
      ; default_value : expr option
      }

and type_var =
  { name : string
  ; bound : expr option
  ; default_value : expr option
  }
[@@deriving show, eq, yojson, sexp]

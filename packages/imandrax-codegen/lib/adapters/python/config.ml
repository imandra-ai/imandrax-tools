(** Python-specific configuration and mappings *)

module Sir = Semantic_ir

(** Mapping from language-neutral type names to Python type names *)
let type_name_mapping = [
  ("int", "int");
  ("bool", "bool");
  ("string", "str");
  ("real", "float");
  ("unit", "None");
]

(** Map a SIR type name to a Python type name *)
let map_type_name name =
  List.assoc_opt name type_name_mapping
  |> Option.value ~default:name

(** Binary operator mapping from SIR to Python *)
let map_bin_op (op : Sir.Types.bin_op) : Ast_types.operator =
  match op with
  | Add -> Add
  | Sub -> Sub
  | Mult -> Mult
  | Div -> Div
  | _ -> failwith "Unsupported binary operator"

(** Comparison operator mapping *)
let map_cmp_op (op : Sir.Types.bin_op) : Ast_types.cmpop =
  match op with
  | Eq -> Eq
  | Lt -> Lt
  | Gt -> Gt
  | _ -> failwith "Not a comparison operator"

(** Boolean operator mapping *)
let map_bool_op (op : Sir.Types.bin_op) : Ast_types.bool_op =
  match op with
  | And -> And
  | Or -> Or
  | _ -> failwith "Not a boolean operator"

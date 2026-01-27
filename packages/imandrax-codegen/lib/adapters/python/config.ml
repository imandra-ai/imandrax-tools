(** Python-specific configuration and mappings *)

module Sir = Semantic_ir

(** Mapping from language-neutral type names to Python type names *)
let (type_name_mapping : (string * string) list) =
  [
    ("int", "int");
    ("bool", "bool");
    ("string", "str");
    ("LChar.t", "str");
    ("char", "str");
    ("unit", "None");
    ("Map.t", "defaultdict");
    (* ("real", "float"); *)
  ]

(** Map a SIR type name to a Python type name *)
let map_type_name (name : string) : string =
  List.assoc_opt name type_name_mapping |> Option.value ~default:name

(** Binary operator mapping from SIR to Python *)
let map_bin_op (op : Sir.bin_op) : Ast.operator =
  match op with
  | Add -> Add
  | Sub -> Sub
  | Mult -> Mult
  | Div -> Div
  | _ -> failwith "Unsupported binary operator"

(** Comparison operator mapping *)
let map_cmp_op (op : Sir.bin_op) : Ast.cmpop =
  match op with
  | Eq -> Eq
  | Lt -> Lt
  | Gt -> Gt
  | _ -> failwith "Not a comparison operator"

(** Boolean operator mapping *)
let map_bool_op (op : Sir.bin_op) : Ast.bool_op =
  match op with And -> And | Or -> Or | _ -> failwith "Not a boolean operator"

(** TypeScript-specific configuration and mappings *)

module Sir = Semantic_ir

module Extra_imports = struct
  type t = {
    option : bool;
    default_map : bool;
  }

  let empty = { option = false; default_map = false }
  let option = { empty with option = true }
  let default_map = { empty with default_map = true }

  let union a b = {
    option = a.option || b.option;
    default_map = a.default_map || b.default_map;
  }

  let union_list xs = List.fold_left union empty xs

  let lib_content : t -> string = fun t ->
    let buf = Buffer.create 100 in
    if t.option then Buffer.add_string buf Helper_lib.option else ();
    if t.default_map then Buffer.add_string buf Helper_lib.default_map else ();
    if t.option || t.default_map then Buffer.add_char buf '\n' else ();
    Buffer.contents buf
end


(** Mapping from language-neutral type names to TypeScript type names *)
let type_name_mapping : (string * string) list =
  [ "int", "number"
  ; "real", "number"
  ; "bool", "boolean"
  ; "string", "string"
  ; "LChar.t", "string"
  ; "char", "string"
  ; "unit", "null"
  ; "list", "Array"
  ; "option", "Option"
  ; "Map.t", "DefaultMap"
  ]
;;

(** Map a SIR type name to a TypeScript type name *)
let map_type_name (name : string) : string =
  List.assoc_opt name type_name_mapping |> Option.value ~default:name
;;

(** Binary operator to TypeScript string *)
let string_of_bin_op (op : Sir.bin_op) : string =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Eq -> "==="
  | Lt -> "<"
  | Gt -> ">"
  | And -> "&&"
  | Or -> "||"
;;

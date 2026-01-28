(** TypeScript-specific configuration and mappings *)

module Sir = Semantic_ir

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
  ; "Map.t", "Map"
  ]
;;

(** Map a SIR type name to a TypeScript type name *)
let map_type_name (name : string) : string =
  List.assoc_opt name type_name_mapping |> Option.value ~default:name
;;

(** Convert snake_case or lowercase to PascalCase for type names *)
let to_pascal_case (name : string) : string =
  name
  |> String.split_on_char '_'
  |> List.map String.capitalize_ascii
  |> String.concat ""
;;

(** Convert type variable name from OCaml style ('a) to TypeScript style (A) *)
let map_type_var_name (name : string) : string =
  (* Remove leading apostrophe if present and uppercase *)
  let name = if String.length name > 0 && name.[0] = '\'' then
    String.sub name 1 (String.length name - 1)
  else name in
  String.uppercase_ascii name
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

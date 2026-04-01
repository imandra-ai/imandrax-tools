(** js_of_iml adapter configuration and type mappings *)

module Sir = Semantic_ir

(* OCaml type mappings
   ==================== *)

(** Map SIR base type names to OCaml type names as used in the prelude *)
let ocaml_type_name_mapping : (string * string) list =
  [ "int", "int"      (* Z.t via prelude *)
  ; "real", "real"     (* Q.t via prelude *)
  ; "bool", "bool"
  ; "string", "string"
  ; "unit", "unit"
  ]
;;

let map_ocaml_type_name (name : string) : string =
  List.assoc_opt name ocaml_type_name_mapping |> Option.value ~default:name
;;

(* Python type mappings
   ===================== *)

let python_type_name_mapping : (string * string) list =
  [ "int", "int"
  ; "real", "float"
  ; "bool", "bool"
  ; "string", "str"
  ; "unit", "None"
  ]
;;

let map_python_type_name (name : string) : string =
  List.assoc_opt name python_type_name_mapping |> Option.value ~default:name
;;

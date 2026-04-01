(** Emit Python types, JSON marshalling, and subprocess bindings for js_of_iml.

    For each SIR type declaration, produces:
    - Python dataclass / Enum definition
    - [_to_dict] function for JSON serialization
    - [_of_json] function for JSON deserialization (from subprocess output)

    Also produces the subprocess-based binding function.
*)

module Sir = Semantic_ir

let sprintf = Printf.sprintf
let join_lines lines = String.concat "\n" lines

(* Type expression emission
   ========================= *)

(** Emit a SIR type expression as a Python type annotation *)
let rec emit_type_annot (te : Sir.type_expr) : string =
  match te with
  | Sir.TBase name -> Config.map_python_type_name name
  | Sir.TVar name -> name
  | Sir.TApp ("list", [ elem ]) ->
    sprintf "list[%s]" (emit_type_annot elem)
  | Sir.TApp ("option", [ elem ]) ->
    sprintf "Optional[%s]" (emit_type_annot elem)
  | Sir.TApp (name, args) ->
    let args_str = List.map emit_type_annot args |> String.concat ", " in
    sprintf "%s[%s]" (Config.map_python_type_name name) args_str
  | Sir.TTuple exprs ->
    let parts = List.map emit_type_annot exprs in
    sprintf "tuple[%s]" (String.concat ", " parts)
  | Sir.TArrow (arg, ret) ->
    sprintf "Callable[[%s], %s]" (emit_type_annot arg) (emit_type_annot ret)
;;

(* Dataclass / Enum emission
   =========================== *)

(** Check if a variant is a simple enum (all constructors have no fields) *)
let is_simple_enum (constructors : Sir.variant_constructor list) : bool =
  List.for_all (fun (vc : Sir.variant_constructor) -> vc.vc_fields = []) constructors
;;

(** Emit a Python type definition for a SIR type declaration *)
let emit_type_def (decl : Sir.type_decl) : string =
  match decl with
  | Sir.Record { name; fields; _ } ->
    let field_lines =
      fields |> List.map (fun (rf : Sir.record_field) ->
        sprintf "    %s: %s" rf.rf_name (emit_type_annot rf.rf_type)
      )
    in
    join_lines
      ([ ""
       ; "@dataclass"
       ; sprintf "class %s:" name
       ] @ field_lines)
  | Sir.Variant { name; constructors; _ } when is_simple_enum constructors ->
    let member_lines =
      constructors |> List.map (fun (vc : Sir.variant_constructor) ->
        let lower = String.uppercase_ascii vc.vc_name in
        sprintf "    %s = \"%s\"" lower vc.vc_name
      )
    in
    join_lines
      ([ ""
       ; sprintf "class %s(Enum):" name
       ] @ member_lines)
  | Sir.Variant { name; constructors; _ } ->
    let class_defs =
      constructors |> List.map (fun (vc : Sir.variant_constructor) ->
        match vc.vc_fields with
        | [] ->
          join_lines
            [ ""
            ; "@dataclass"
            ; sprintf "class %s:" vc.vc_name
            ; "    pass"
            ]
        | [ Sir.Variant_field.Positional te ] ->
          join_lines
            [ ""
            ; "@dataclass"
            ; sprintf "class %s:" vc.vc_name
            ; sprintf "    value: %s" (emit_type_annot te)
            ]
        | fields ->
          let field_lines =
            fields |> List.mapi (fun i field ->
              match field with
              | Sir.Variant_field.Named (n, te) ->
                sprintf "    %s: %s" n (emit_type_annot te)
              | Sir.Variant_field.Positional te ->
                sprintf "    arg%d: %s" i (emit_type_annot te)
            )
          in
          join_lines
            ([ ""
             ; "@dataclass"
             ; sprintf "class %s:" vc.vc_name
             ] @ field_lines)
      )
    in
    let constructor_names =
      constructors |> List.map (fun (vc : Sir.variant_constructor) -> vc.vc_name)
    in
    join_lines class_defs
    ^ "\n\n"
    ^ sprintf "%s = %s" name (String.concat " | " constructor_names)
  | Sir.Alias { name; target; _ } ->
    sprintf "\n%s = %s" name (emit_type_annot target)
;;

(* _to_dict emission
   =================== *)

(** Emit expression to convert a Python value to a JSON-serializable dict/value *)
let rec emit_to_dict_expr (te : Sir.type_expr) (val_expr : string) : string =
  match te with
  | Sir.TBase "int" -> val_expr
  | Sir.TBase "real" -> val_expr
  | Sir.TBase "bool" -> val_expr
  | Sir.TBase "string" -> val_expr
  | Sir.TBase "unit" -> "None"
  | Sir.TBase name -> sprintf "%s_to_dict(%s)" name val_expr
  | Sir.TApp ("list", [ elem ]) ->
    let elem_conv = emit_to_dict_expr elem "x" in
    sprintf "[%s for x in %s]" elem_conv val_expr
  | Sir.TApp ("option", [ elem ]) ->
    let elem_conv = emit_to_dict_expr elem val_expr in
    sprintf "(%s if %s is not None else None)" elem_conv val_expr
  | Sir.TTuple exprs ->
    let n = List.length exprs in
    let elems =
      List.init n (fun i ->
        emit_to_dict_expr (List.nth exprs i) (sprintf "%s[%d]" val_expr i)
      )
    in
    sprintf "[%s]" (String.concat ", " elems)
  | _ -> val_expr
;;

(** Emit a [_to_dict] function for a SIR type declaration *)
let emit_to_dict_func (decl : Sir.type_decl) : string =
  match decl with
  | Sir.Record { name; fields; _ } ->
    let field_lines =
      fields |> List.map (fun (rf : Sir.record_field) ->
        let conv = emit_to_dict_expr rf.rf_type
          (sprintf "v.%s" rf.rf_name) in
        sprintf "        \"%s\": %s," rf.rf_name conv
      )
    in
    join_lines
      ([ ""
       ; sprintf "def %s_to_dict(v: %s) -> dict:" name name
       ; "    return {"
       ] @ field_lines
       @ [ "    }" ])
  | Sir.Variant { name; constructors; _ } when is_simple_enum constructors ->
    join_lines
      [ ""
      ; sprintf "def %s_to_dict(v: %s) -> str:" name name
      ; "    return v.value"
      ]
  | Sir.Variant { name; constructors; _ } ->
    let cases =
      constructors |> List.map (fun (vc : Sir.variant_constructor) ->
        match vc.vc_fields with
        | [] ->
          join_lines
            [ sprintf "    if isinstance(v, %s):" vc.vc_name
            ; sprintf "        return {\"tag\": \"%s\"}" vc.vc_name
            ]
        | [ Sir.Variant_field.Positional te ] ->
          let conv = emit_to_dict_expr te "v.value" in
          join_lines
            [ sprintf "    if isinstance(v, %s):" vc.vc_name
            ; sprintf "        return {\"tag\": \"%s\", \"value\": %s}" vc.vc_name conv
            ]
        | fields ->
          let field_strs =
            fields |> List.mapi (fun i field ->
              match field with
              | Sir.Variant_field.Named (n, te) ->
                let conv = emit_to_dict_expr te (sprintf "v.%s" n) in
                sprintf "\"%s\": %s" n conv
              | Sir.Variant_field.Positional te ->
                let conv = emit_to_dict_expr te (sprintf "v.arg%d" i) in
                sprintf "\"arg%d\": %s" i conv
            )
          in
          join_lines
            [ sprintf "    if isinstance(v, %s):" vc.vc_name
            ; sprintf "        return {\"tag\": \"%s\", %s}" vc.vc_name
                (String.concat ", " field_strs)
            ]
      )
    in
    join_lines
      ([ ""
       ; sprintf "def %s_to_dict(v: %s) -> dict:" name name
       ] @ cases
       @ [ sprintf "    raise ValueError(f\"unknown %s: {v}\")" name ])
  | Sir.Alias { name; target; _ } ->
    let conv = emit_to_dict_expr target "v" in
    join_lines
      [ ""
      ; sprintf "def %s_to_dict(v: %s):" name (emit_type_annot target)
      ; sprintf "    return %s" conv
      ]
;;

(* _of_json emission (for deserializing results from subprocess)
   ============================================================== *)

(** Emit expression to convert a JSON-parsed Python value to a typed Python value *)
let rec emit_of_json_expr (te : Sir.type_expr) (val_expr : string) : string =
  match te with
  | Sir.TBase "int" -> val_expr
  | Sir.TBase "real" -> val_expr
  | Sir.TBase "bool" -> val_expr
  | Sir.TBase "string" -> val_expr
  | Sir.TBase "unit" -> "None"
  | Sir.TBase name -> sprintf "%s_of_json(%s)" name val_expr
  | Sir.TApp ("list", [ elem ]) ->
    let elem_conv = emit_of_json_expr elem "x" in
    sprintf "[%s for x in %s]" elem_conv val_expr
  | Sir.TApp ("option", [ elem ]) ->
    let elem_conv = emit_of_json_expr elem val_expr in
    sprintf "(%s if %s is not None else None)" elem_conv val_expr
  | Sir.TTuple exprs ->
    let n = List.length exprs in
    let elems =
      List.init n (fun i ->
        emit_of_json_expr (List.nth exprs i) (sprintf "%s[%d]" val_expr i)
      )
    in
    sprintf "(%s,)" (String.concat ", " elems)
  | _ -> val_expr
;;

(* Binding function emission
   =========================== *)

(** Emit the Python binding function that calls the JS wrapper via subprocess.

    Args:
      func_name: Python function name (e.g. "match_price")
      args: list of (arg_name, arg_type)
      ret_type: return type
      js_wrapper_path: path to the .bc.js file
*)
let emit_binding_func
    ~(func_name : string)
    ~(args : (string * Sir.type_expr) list)
    ~(ret_type : Sir.type_expr)
    ~(js_wrapper_path : string)
  : string =
  let param_list =
    args |> List.map (fun (name, te) ->
      sprintf "%s: %s" name (emit_type_annot te)
    ) |> String.concat ", "
  in
  let ret_annot = emit_type_annot ret_type in
  let payload_fields =
    args |> List.map (fun (name, te) ->
      let conv = emit_to_dict_expr te name in
      sprintf "        \"%s\": %s," name conv
    )
  in
  let result_conv = emit_of_json_expr ret_type "parsed" in
  join_lines
    ([ ""
     ; sprintf "def %s(%s) -> %s:" func_name param_list ret_annot
     ; "    payload = json.dumps({"
     ] @ payload_fields
     @ [ "    })"
       ; "    result = subprocess.run("
       ; sprintf "        [\"node\", \"%s\"]," js_wrapper_path
       ; "        input=payload, capture_output=True, text=True, check=True,"
       ; "    )"
       ; "    parsed = json.loads(result.stdout)"
       ; sprintf "    return %s" result_conv
       ])
;;

(** Emit the full Python module: imports + types + to_dict + binding *)
let emit_module
    ~(type_decls : Sir.type_decl list)
    ~(func_name : string)
    ~(args : (string * Sir.type_expr) list)
    ~(ret_type : Sir.type_expr)
    ~(js_wrapper_path : string)
  : string =
  let imports = join_lines
    [ "\"\"\"Generated Python bindings for IML function via js_of_ocaml.\"\"\""
    ; ""
    ; "import json"
    ; "import subprocess"
    ; "from dataclasses import dataclass"
    ; "from enum import Enum"
    ; "from typing import Optional"
    ]
  in
  let type_defs = type_decls |> List.map emit_type_def |> String.concat "\n" in
  let to_dict_funcs = type_decls |> List.map emit_to_dict_func |> String.concat "\n" in
  let binding = emit_binding_func ~func_name ~args ~ret_type ~js_wrapper_path in
  join_lines [ imports; type_defs; to_dict_funcs; binding; "" ]
;;

(* Tests
   ====== *)

let%expect_test "emit_type_def: record" =
  let decl =
    Sir.Record
      { name = "order"
      ; type_params = []
      ; fields =
          [ { rf_name = "order_id"; rf_type = Sir.TBase "int" }
          ; { rf_name = "order_price"; rf_type = Sir.TBase "real" }
          ]
      }
  in
  print_string (emit_type_def decl);
  [%expect {|

    @dataclass
    class order:
        order_id: int
        order_price: float |}]
;;

let%expect_test "emit_type_def: simple enum" =
  let decl =
    Sir.Variant
      { name = "order_type"
      ; type_params = []
      ; constructors =
          [ { vc_name = "Market"; vc_fields = [] }
          ; { vc_name = "Limit"; vc_fields = [] }
          ; { vc_name = "Quote"; vc_fields = [] }
          ]
      }
  in
  print_string (emit_type_def decl);
  [%expect {|

    class order_type(Enum):
        MARKET = "Market"
        LIMIT = "Limit"
        QUOTE = "Quote" |}]
;;

let%expect_test "emit_to_dict_func: record" =
  let decl =
    Sir.Record
      { name = "order"
      ; type_params = []
      ; fields =
          [ { rf_name = "order_id"; rf_type = Sir.TBase "int" }
          ; { rf_name = "order_type"; rf_type = Sir.TBase "order_type" }
          ; { rf_name = "order_price"; rf_type = Sir.TBase "real" }
          ]
      }
  in
  print_string (emit_to_dict_func decl);
  [%expect {|

    def order_to_dict(v: order) -> dict:
        return {
            "order_id": v.order_id,
            "order_type": order_type_to_dict(v.order_type),
            "order_price": v.order_price,
        } |}]
;;

let%expect_test "emit_to_dict_func: simple enum" =
  let decl =
    Sir.Variant
      { name = "order_type"
      ; type_params = []
      ; constructors =
          [ { vc_name = "Market"; vc_fields = [] }
          ; { vc_name = "Limit"; vc_fields = [] }
          ]
      }
  in
  print_string (emit_to_dict_func decl);
  [%expect {|

    def order_type_to_dict(v: order_type) -> str:
        return v.value |}]
;;

let%expect_test "emit_binding_func" =
  let args =
    [ "ob", Sir.TBase "order_book"
    ; "ref_price", Sir.TBase "real"
    ]
  in
  let ret_type = Sir.TApp ("option", [ Sir.TBase "real" ]) in
  print_string (emit_binding_func
    ~func_name:"match_price"
    ~args ~ret_type
    ~js_wrapper_path:"six_swiss_wrapper.bc.js");
  [%expect {|

    def match_price(ob: order_book, ref_price: float) -> Optional[float]:
        payload = json.dumps({
            "ob": order_book_to_dict(ob),
            "ref_price": ref_price,
        })
        result = subprocess.run(
            ["node", "six_swiss_wrapper.bc.js"],
            input=payload, capture_output=True, text=True, check=True,
        )
        parsed = json.loads(result.stdout)
        return (parsed if parsed is not None else None) |}]
;;

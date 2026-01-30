(** Emit TypeScript source code from Semantic IR *)

module Sir = Semantic_ir
module Extra_imports = Config.Extra_imports

let sprintf = Printf.sprintf

(* Formatting helpers
==================== *)

let brackets s = "[" ^ s ^ "]"
let braces s = "{ " ^ s ^ " }"
let angles s = "<" ^ s ^ ">"
let parens s = "(" ^ s ^ ")"
let quote s = "\"" ^ s ^ "\""
let kv k v = k ^ ": " ^ v
let join_comma = String.concat ", "
let join_semi = String.concat "; "
let join_newline_bar = String.concat "\n  | "

(** Tagged object for type definitions (uses semicolon) *)
let tagged_type tag payload =
  braces (join_semi [ kv "tag" (quote tag); kv "payload" payload ])
;;

(** Tagged object for values (uses comma) *)
let tagged_value tag payload =
  braces (join_comma [ kv "tag" (quote tag); kv "payload" payload ])
;;

(* Emitters
==================== *)

(* Type
-------------------- *)

(** Emit a type expression as TypeScript type annotation *)
let rec emit_type_expr (te : Sir.type_expr) : string * Extra_imports.t =
  match te with
  | Sir.TBase name -> (Config.map_type_name name, Extra_imports.empty)
  | Sir.TVar name -> (name, Extra_imports.empty)
  | Sir.TApp (name, args) ->
      let mapped_name = Config.map_type_name name in
      (match mapped_name, args with
      | "Array", [ elem ] ->
          let code, imports = emit_type_expr elem in
          (code ^ "[]", imports)
      | "Option", [ elem ] ->
          let code, imports = emit_type_expr elem in
          ("Option" ^ angles code, Extra_imports.union Extra_imports.option imports)
      | "DefaultMap", args ->
          let codes, imports = emit_type_exprs args in
          ("DefaultMap" ^ angles (join_comma codes), Extra_imports.union Extra_imports.default_map imports)
      | _, [] -> (mapped_name, Extra_imports.empty)
      | _, _ ->
          let codes, imports = emit_type_exprs args in
          (mapped_name ^ angles (join_comma codes), imports))
  | Sir.TTuple exprs ->
      let codes, imports = emit_type_exprs exprs in
      (brackets (join_comma codes), imports)
  | Sir.TArrow (arg, ret) ->
      let arg_code, arg_imports = emit_type_expr arg in
      let ret_code, ret_imports = emit_type_expr ret in
      (parens arg_code ^ " => " ^ ret_code, Extra_imports.union arg_imports ret_imports)

and emit_type_exprs (exprs : Sir.type_expr list) : string list * Extra_imports.t =
  let results = List.map emit_type_expr exprs in
  let codes = List.map fst results in
  let imports = Extra_imports.union_list (List.map snd results) in
  (codes, imports)
;;

(** Emit type parameters as TypeScript generics: <a, b> *)
let emit_type_params (params : string list) : string =
  match params with [] -> "" | _ -> angles (join_comma params)
;;

(** Emit variant constructor fields as TypeScript type *)
let emit_variant_payload (fields : Sir.Variant_field.t list) : string * Extra_imports.t =
  match fields with
  | [] -> ("null", Extra_imports.empty)
  | [ Sir.Variant_field.Positional ty ] -> emit_type_expr ty
  | [ Sir.Variant_field.Named (name, ty) ] ->
      let code, imports = emit_type_expr ty in
      (braces (kv name code), imports)
  | multiple ->
      (* Check if all fields are named (inline record) or positional (tuple) *)
      let all_named =
        List.for_all
          (function Sir.Variant_field.Named _ -> true | _ -> false)
          multiple
      in
      if all_named
      then
        let results =
          multiple
          |> List.map (function
               | Sir.Variant_field.Named (name, ty) ->
                   let code, imports = emit_type_expr ty in
                   (kv name code, imports)
               | Sir.Variant_field.Positional _ ->
                   failwith "unexpected positional")
        in
        let codes = List.map fst results in
        let imports = Extra_imports.union_list (List.map snd results) in
        (braces (join_semi codes), imports)
      else
        let results =
          multiple
          |> List.map (fun field ->
                 emit_type_expr (Sir.Variant_field.type_expr field))
        in
        let codes = List.map fst results in
        let imports = Extra_imports.union_list (List.map snd results) in
        (brackets (join_comma codes), imports)
;;

(** Emit a variant constructor as a TypeScript union member *)
let emit_variant_constructor (vc : Sir.variant_constructor) : string * Extra_imports.t =
  let payload, imports = emit_variant_payload vc.vc_fields in
  (tagged_type vc.vc_name payload, imports)
;;

(** Emit a type declaration as TypeScript source *)
let emit_type_decl (decl : Sir.type_decl) : string * Extra_imports.t =
  match decl with
  | Sir.Variant { name; type_params; constructors } ->
      let generics = emit_type_params type_params in
      let results = List.map emit_variant_constructor constructors in
      let members = results |> List.map fst |> join_newline_bar in
      let imports = Extra_imports.union_list (List.map snd results) in
      ("type " ^ name ^ generics ^ " =\n  | " ^ members ^ ";", imports)
  | Sir.Record { name; type_params; fields } ->
      let generics = emit_type_params type_params in
      let results =
        fields
        |> List.map (fun (rf : Sir.record_field) ->
               let code, imports = emit_type_expr rf.rf_type in
               ("  " ^ kv rf.rf_name code ^ ";", imports))
      in
      let field_strs = results |> List.map fst |> String.concat "\n" in
      let imports = Extra_imports.union_list (List.map snd results) in
      ("type " ^ name ^ generics ^ " = {\n" ^ field_strs ^ "\n};", imports)
  | Sir.Alias { name; type_params; target } ->
      let generics = emit_type_params type_params in
      let code, imports = emit_type_expr target in
      ("type " ^ name ^ generics ^ " = " ^ code ^ ";", imports)
;;

(* Value
-------------------- *)

(** Escape a string for TypeScript using hex escapes (not octal) *)
let ts_escape_string s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 32 || Char.code c > 126 ->
          Buffer.add_string buf (sprintf "\\x%02x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf
;;

let emit_const (c : Sir.const_value) : string * Extra_imports.t =
  let code =
    match c with
    | Sir.CInt i -> string_of_int i
    | Sir.CFloat f -> string_of_float f
    | Sir.CBool b -> if b then "true" else "false"
    | Sir.CString s -> quote (ts_escape_string s)
    | Sir.CChar ch -> quote (ts_escape_string (String.make 1 ch))
    | Sir.CUnit -> "null"
  in
  (code, Extra_imports.empty)
;;

let rec emit_value (v : Sir.value) : string * Extra_imports.t =
  match v with
  | Sir.VConst c -> emit_const c
  | Sir.VTuple vs ->
      let codes, imports = emit_values vs in
      (brackets (join_comma codes), imports)
  | Sir.VList vs ->
      let codes, imports = emit_values vs in
      (brackets (join_comma codes), imports)
  | Sir.VRecord { type_name = _; fields } ->
      let results =
        fields |> List.map (fun (name, value) ->
            let code, imports = emit_value value in
            (kv name code, imports))
      in
      let codes = List.map fst results in
      let imports = Extra_imports.union_list (List.map snd results) in
      (braces (join_comma codes), imports)
  | Sir.VConstruct { constructor; args } ->
      let payload, imports =
        match args with
        | [] -> ("null", Extra_imports.empty)
        | [ single ] -> emit_value single
        | multiple ->
            let codes, imports = emit_values multiple in
            (brackets (join_comma codes), imports)
      in
      (tagged_value constructor payload, imports)
  | Sir.VName name -> (name, Extra_imports.empty)
  | Sir.VBinOp (left, op, right) ->
      let left_code, left_imports = emit_value left in
      let right_code, right_imports = emit_value right in
      ( left_code ^ " " ^ Config.string_of_bin_op op ^ " " ^ right_code,
        Extra_imports.union left_imports right_imports )
  | Sir.VIfThenElse (cond, then_val, else_val) ->
      let cond_code, cond_imports = emit_value cond in
      let then_code, then_imports = emit_value then_val in
      let else_code, else_imports = emit_value else_val in
      ( cond_code ^ " ? " ^ then_code ^ " : " ^ else_code,
        Extra_imports.union_list [ cond_imports; then_imports; else_imports ] )
  | Sir.VMap { default; entries } ->
      let default_code, default_imports = emit_value default in
      let entry_results =
        entries
        |> List.map (fun (k, v) ->
               let k_code, k_imports = emit_value k in
               let v_code, v_imports = emit_value v in
               (brackets (join_comma [ k_code; v_code ]) ^ " as const",
                Extra_imports.union k_imports v_imports))
      in
      let entries_str = entry_results |> List.map fst |> join_comma in
      let entries_imports = Extra_imports.union_list (List.map snd entry_results) in
      ( "new DefaultMap("
        ^ parens ("() => " ^ default_code)
        ^ ", "
        ^ brackets entries_str
        ^ ")",
        Extra_imports.union_list [ Extra_imports.default_map; default_imports; entries_imports ] )

and emit_values (vs : Sir.value list) : string list * Extra_imports.t =
  let results = List.map emit_value vs in
  let codes = List.map fst results in
  let imports = Extra_imports.union_list (List.map snd results) in
  (codes, imports)
;;

(** Emit a value assignment as TypeScript const declaration.
    When the type contains type variables, emit type aliases first:
    type a = unknown;
    const w: a[] = []; *)
let emit_value_assignment (va : Sir.Value_assignment.t) : string * Extra_imports.t =
  let type_vars = Sir.Value_assignment.type_var va in
  let ty_code, ty_imports = emit_type_expr va.ty in
  let tm_code, tm_imports = emit_value va.tm in
  let imports = Extra_imports.union ty_imports tm_imports in
  let type_aliases =
    type_vars
    |> List.map (fun tv -> "type " ^ tv ^ " = unknown;")
    |> String.concat "\n"
  in
  let const_decl = "const " ^ kv va.var_name ty_code ^ " = " ^ tm_code ^ ";" in
  match type_vars with
  | [] -> (const_decl, imports)
  | _ -> (type_aliases ^ "\n" ^ const_decl, imports)
;;

(* Test declaration
-------------------- *)

(** Emit a test suite as TypeScript test data object *)
let emit_test_suite_dict (tests : Sir.test_suite) : string * Extra_imports.t =
  let results =
    tests
    |> List.map (fun (test : Sir.test_decl) ->
           let arg_results =
             test.f_args
             |> List.map (fun (name, _ty, value) ->
                    let code, imports = emit_value value in
                    (kv name code, imports))
           in
           let args_obj = arg_results |> List.map fst |> join_comma in
           let args_imports = Extra_imports.union_list (List.map snd arg_results) in
           let expected_code, expected_imports = emit_value (snd test.f_output) in
           let entry =
             sprintf
               "  %s: {\n    input: { %s },\n    expected: %s\n  }"
               (quote test.name)
               args_obj
               expected_code
           in
           (entry, Extra_imports.union args_imports expected_imports))
  in
  let entries = results |> List.map fst |> String.concat ",\n" in
  let imports = Extra_imports.union_list (List.map snd results) in
  ("const tests = {\n" ^ entries ^ "\n};", imports)
;;

(* Tests
==================== *)

let%expect_test "emit_type_expr: base types" =
  print_endline (fst (emit_type_expr (Sir.TBase "int")));
  print_endline (fst (emit_type_expr (Sir.TBase "bool")));
  print_endline (fst (emit_type_expr (Sir.TBase "string")));
  [%expect {|
    number
    boolean
    string |}]
;;

let%expect_test "emit_type_expr: type application" =
  print_endline (fst (emit_type_expr (Sir.TApp ("list", [ Sir.TBase "int" ]))));
  print_endline (fst (emit_type_expr (Sir.TApp ("option", [ Sir.TBase "string" ]))));
  [%expect {|
    number[]
    Option<string> |}]
;;

let%expect_test "emit_type_expr: tuple" =
  print_endline
    (fst (emit_type_expr (Sir.TTuple [ Sir.TBase "int"; Sir.TBase "bool" ])));
  [%expect {| [number, boolean] |}]
;;

let%expect_test "emit_type_decl: simple enum" =
  let decl =
    Sir.Variant
      { name = "color"
      ; type_params = []
      ; constructors =
          [ { vc_name = "Red"; vc_fields = [] }
          ; { vc_name = "Green"; vc_fields = [] }
          ; { vc_name = "Blue"; vc_fields = [] }
          ]
      }
  in
  print_endline (fst (emit_type_decl decl));
  [%expect
    {|
    type color =
      | { tag: "Red"; payload: null }
      | { tag: "Green"; payload: null }
      | { tag: "Blue"; payload: null }; |}]
;;

let%expect_test "emit_type_decl: variant with payload" =
  let decl =
    Sir.Variant
      { name = "shape"
      ; type_params = []
      ; constructors =
          [ { vc_name = "Point"; vc_fields = [] }
          ; { vc_name = "Circle"
            ; vc_fields = [ Sir.Variant_field.Positional (Sir.TBase "int") ]
            }
          ; { vc_name = "Rectangle"
            ; vc_fields =
                [ Sir.Variant_field.Positional (Sir.TBase "int")
                ; Sir.Variant_field.Positional (Sir.TBase "int")
                ]
            }
          ]
      }
  in
  print_endline (fst (emit_type_decl decl));
  [%expect
    {|
    type shape =
      | { tag: "Point"; payload: null }
      | { tag: "Circle"; payload: number }
      | { tag: "Rectangle"; payload: [number, number] }; |}]
;;

let%expect_test "emit_type_decl: record" =
  let decl =
    Sir.Record
      { name = "point"
      ; type_params = []
      ; fields =
          [ { rf_name = "x"; rf_type = Sir.TBase "int" }
          ; { rf_name = "y"; rf_type = Sir.TBase "int" }
          ]
      }
  in
  print_endline (fst (emit_type_decl decl));
  [%expect {|
    type point = {
      x: number;
      y: number;
    }; |}]
;;

let%expect_test "emit_type_decl: generic variant" =
  let decl =
    Sir.Variant
      { name = "maybe"
      ; type_params = [ "a" ]
      ; constructors =
          [ { vc_name = "Just"
            ; vc_fields = [ Sir.Variant_field.Positional (Sir.TVar "a") ]
            }
          ; { vc_name = "Nothing"; vc_fields = [] }
          ]
      }
  in
  print_endline (fst (emit_type_decl decl));
  [%expect
    {|
    type maybe<a> =
      | { tag: "Just"; payload: a }
      | { tag: "Nothing"; payload: null };
    |}]
;;

let%expect_test "emit_value: constants" =
  print_endline (fst (emit_value (Sir.VConst (Sir.CInt 42))));
  print_endline (fst (emit_value (Sir.VConst (Sir.CBool true))));
  print_endline (fst (emit_value (Sir.VConst (Sir.CString "hello"))));
  [%expect {|
    42
    true
    "hello" |}]
;;

let%expect_test "emit_value: construct" =
  print_endline
    (fst (emit_value
       (Sir.VConstruct
          { constructor = "Circle"; args = [ Sir.VConst (Sir.CInt 5) ] })));
  print_endline
    (fst (emit_value (Sir.VConstruct { constructor = "Point"; args = [] })));
  [%expect
    {|
    { tag: "Circle", payload: 5 }
    { tag: "Point", payload: null }
    |}]
;;

let%expect_test "emit_value_assignment" =
  let va : Sir.Value_assignment.t =
    { var_name = "x"; ty = Sir.TBase "int"; tm = Sir.VConst (Sir.CInt 42) }
  in
  print_endline (fst (emit_value_assignment va));
  [%expect {| const x: number = 42; |}]
;;

let%expect_test "emit_value_assignment with type variables" =
  let va : Sir.Value_assignment.t =
    { var_name = "w"; ty = Sir.TApp ("list", [ Sir.TVar "a" ]); tm = Sir.VList [] }
  in
  print_endline (fst (emit_value_assignment va));
  [%expect {|
    type a = unknown;
    const w: a[] = []; |}]
;;

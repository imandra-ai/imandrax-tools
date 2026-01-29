(** Emit TypeScript source code from Semantic IR *)

module Sir = Semantic_ir

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
let rec emit_type_expr (te : Sir.type_expr) : string =
  match te with
  | Sir.TBase name -> Config.map_type_name name
  | Sir.TVar name -> name
  | Sir.TApp (name, args) ->
      let mapped_name = Config.map_type_name name in
      (match mapped_name, args with
      | "Array", [ elem ] -> emit_type_expr elem ^ "[]"
      | "Option", [ elem ] -> emit_type_expr elem ^ " | null"
      | _, [] -> mapped_name
      | _, _ ->
          mapped_name ^ angles (args |> List.map emit_type_expr |> join_comma))
  | Sir.TTuple exprs -> brackets (exprs |> List.map emit_type_expr |> join_comma)
  | Sir.TArrow (arg, ret) ->
      parens (emit_type_expr arg) ^ " => " ^ emit_type_expr ret
;;

(** Emit type parameters as TypeScript generics: <a, b> *)
let emit_type_params (params : string list) : string =
  match params with [] -> "" | _ -> angles (join_comma params)
;;

(** Emit variant constructor fields as TypeScript type *)
let emit_variant_payload (fields : Sir.Variant_field.t list) : string =
  match fields with
  | [] -> "null"
  | [ Sir.Variant_field.Positional ty ] -> emit_type_expr ty
  | [ Sir.Variant_field.Named (name, ty) ] ->
      braces (kv name (emit_type_expr ty))
  | multiple ->
      (* Check if all fields are named (inline record) or positional (tuple) *)
      let all_named =
        List.for_all
          (function Sir.Variant_field.Named _ -> true | _ -> false)
          multiple
      in
      if all_named
      then
        braces
          (multiple
          |> List.map (function
               | Sir.Variant_field.Named (name, ty) ->
                   kv name (emit_type_expr ty)
               | Sir.Variant_field.Positional _ ->
                   failwith "unexpected positional")
          |> join_semi)
      else
        brackets
          (multiple
          |> List.map (fun field ->
                 emit_type_expr (Sir.Variant_field.type_expr field))
          |> join_comma)
;;

(** Emit a variant constructor as a TypeScript union member *)
let emit_variant_constructor (vc : Sir.variant_constructor) : string =
  tagged_type vc.vc_name (emit_variant_payload vc.vc_fields)
;;

(** Emit a type declaration as TypeScript source *)
let emit_type_decl (decl : Sir.type_decl) : string =
  match decl with
  | Sir.Variant { name; type_params; constructors } ->
      let generics = emit_type_params type_params in
      let members =
        constructors |> List.map emit_variant_constructor |> join_newline_bar
      in
      "type " ^ name ^ generics ^ " =\n  | " ^ members ^ ";"
  | Sir.Record { name; type_params; fields } ->
      let generics = emit_type_params type_params in
      let field_strs =
        fields
        |> List.map (fun (rf : Sir.record_field) ->
               "  " ^ kv rf.rf_name (emit_type_expr rf.rf_type) ^ ";")
        |> String.concat "\n"
      in
      "type " ^ name ^ generics ^ " = {\n" ^ field_strs ^ "\n};"
  | Sir.Alias { name; type_params; target } ->
      let generics = emit_type_params type_params in
      "type " ^ name ^ generics ^ " = " ^ emit_type_expr target ^ ";"
;;

(* Value
-------------------- *)

let emit_const (c : Sir.const_value) : string =
  match c with
  | Sir.CInt i -> string_of_int i
  | Sir.CFloat f -> string_of_float f
  | Sir.CBool b -> if b then "true" else "false"
  | Sir.CString s -> quote (String.escaped s)
  | Sir.CChar ch -> quote (String.escaped (String.make 1 ch))
  | Sir.CUnit -> "null"
;;

let rec emit_value (v : Sir.value) : string =
  match v with
  | Sir.VConst c -> emit_const c
  | Sir.VTuple vs -> brackets (vs |> List.map emit_value |> join_comma)
  | Sir.VList vs -> brackets (vs |> List.map emit_value |> join_comma)
  | Sir.VRecord { type_name = _; fields } ->
      braces
        (fields
        |> List.map (fun (name, value) -> kv name (emit_value value))
        |> join_comma)
  | Sir.VConstruct { constructor; args } ->
      let payload =
        match args with
        | [] -> "null"
        | [ single ] -> emit_value single
        | multiple -> brackets (multiple |> List.map emit_value |> join_comma)
      in
      tagged_value constructor payload
  | Sir.VName name -> name
  | Sir.VBinOp (left, op, right) ->
      emit_value left
      ^ " "
      ^ Config.string_of_bin_op op
      ^ " "
      ^ emit_value right
  | Sir.VIfThenElse (cond, then_val, else_val) ->
      emit_value cond
      ^ " ? "
      ^ emit_value then_val
      ^ " : "
      ^ emit_value else_val
  | Sir.VMap { default; entries } ->
      let entries_str =
        entries
        |> List.map (fun (k, v) ->
               brackets (join_comma [ emit_value k; emit_value v ]))
        |> join_comma
      in
      "new DefaultMap("
      ^ parens ("() => " ^ emit_value default)
      ^ ", "
      ^ brackets entries_str
      ^ ")"
;;

(** Emit a value assignment as TypeScript const declaration *)
let emit_value_assignment (va : Sir.Value_assignment.t) : string =
  "const "
  ^ kv va.var_name (emit_type_expr va.ty)
  ^ " = "
  ^ emit_value va.tm
  ^ ";"
;;

(* Test declaration
-------------------- *)

(** Emit a test suite as TypeScript test data object *)
let emit_test_suite_dict (tests : Sir.test_suite) : string =
  let entries =
    tests
    |> List.map (fun (test : Sir.test_decl) ->
           let args_obj =
             test.f_args
             |> List.map (fun (name, _ty, value) -> kv name (emit_value value))
             |> join_comma
           in
           let expected = emit_value (snd test.f_output) in
           sprintf
             "  %s: {\n    input: { %s },\n    expected: %s\n  }"
             (quote test.name)
             args_obj
             expected)
    |> String.concat ",\n"
  in
  "const tests = {\n" ^ entries ^ "\n};"
;;

(* Tests
==================== *)

let%expect_test "emit_type_expr: base types" =
  print_endline (emit_type_expr (Sir.TBase "int"));
  print_endline (emit_type_expr (Sir.TBase "bool"));
  print_endline (emit_type_expr (Sir.TBase "string"));
  [%expect {|
    number
    boolean
    string |}]
;;

let%expect_test "emit_type_expr: type application" =
  print_endline (emit_type_expr (Sir.TApp ("list", [ Sir.TBase "int" ])));
  print_endline (emit_type_expr (Sir.TApp ("option", [ Sir.TBase "string" ])));
  [%expect {|
    number[]
    string | null |}]
;;

let%expect_test "emit_type_expr: tuple" =
  print_endline
    (emit_type_expr (Sir.TTuple [ Sir.TBase "int"; Sir.TBase "bool" ]));
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
  print_endline (emit_type_decl decl);
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
  print_endline (emit_type_decl decl);
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
  print_endline (emit_type_decl decl);
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
  print_endline (emit_type_decl decl);
  [%expect
    {|
    type maybe<a> =
      | { tag: "Just"; payload: a }
      | { tag: "Nothing"; payload: null };
    |}]
;;

let%expect_test "emit_value: constants" =
  print_endline (emit_value (Sir.VConst (Sir.CInt 42)));
  print_endline (emit_value (Sir.VConst (Sir.CBool true)));
  print_endline (emit_value (Sir.VConst (Sir.CString "hello")));
  [%expect {|
    42
    true
    "hello" |}]
;;

let%expect_test "emit_value: construct" =
  print_endline
    (emit_value
       (Sir.VConstruct
          { constructor = "Circle"; args = [ Sir.VConst (Sir.CInt 5) ] }));
  print_endline
    (emit_value (Sir.VConstruct { constructor = "Point"; args = [] }));
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
  print_endline (emit_value_assignment va);
  [%expect {| const x: number = 42; |}]
;;

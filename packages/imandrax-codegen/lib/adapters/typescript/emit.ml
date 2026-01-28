(** Emit TypeScript source code from Semantic IR *)

module Sir = Semantic_ir
let sprintf = Printf.sprintf

(** Emit a type expression as TypeScript type annotation *)
let rec emit_type_expr (te : Sir.type_expr) : string =
  match te with
  | Sir.TBase name -> Config.map_type_name name
  | Sir.TVar name -> Config.map_type_var_name name
  | Sir.TApp (name, args) ->
    let mapped_name = Config.map_type_name name in
    (match mapped_name, args with
     | "Array", [ elem ] -> emit_type_expr elem ^ "[]"
     | "Option", [ elem ] -> emit_type_expr elem ^ " | null"
     | _, [] -> mapped_name
     | _, _ ->
       let args_str = args |> List.map emit_type_expr |> String.concat ", " in
       mapped_name ^ "<" ^ args_str ^ ">")
  | Sir.TTuple exprs ->
    let elems = exprs |> List.map emit_type_expr |> String.concat ", " in
    "[" ^ elems ^ "]"
  | Sir.TArrow (arg, ret) ->
    "(" ^ emit_type_expr arg ^ ") => " ^ emit_type_expr ret
;;

(** Emit type parameters as TypeScript generics: <A, B> *)
let emit_type_params (params : string list) : string =
  match params with
  | [] -> ""
  | _ ->
    let params_str =
      params |> List.map Config.map_type_var_name |> String.concat ", "
    in
    "<" ^ params_str ^ ">"
;;

(** Emit a constant value as TypeScript literal *)
let emit_const (c : Sir.const_value) : string =
  match c with
  | Sir.CInt i -> string_of_int i
  | Sir.CFloat f -> string_of_float f
  | Sir.CBool b -> if b then "true" else "false"
  | Sir.CString s -> "\"" ^ String.escaped s ^ "\""
  | Sir.CChar ch -> "\"" ^ String.escaped (String.make 1 ch) ^ "\""
  | Sir.CUnit -> "null"
;;

(** Emit a value as TypeScript expression *)
let rec emit_value (v : Sir.value) : string =
  match v with
  | Sir.VConst c -> emit_const c
  | Sir.VTuple vs ->
    let elems = vs |> List.map emit_value |> String.concat ", " in
    "[" ^ elems ^ "]"
  | Sir.VList vs ->
    let elems = vs |> List.map emit_value |> String.concat ", " in
    "[" ^ elems ^ "]"
  | Sir.VRecord { type_name = _; fields } ->
    let field_strs =
      fields
      |> List.map (fun (name, value) -> name ^ ": " ^ emit_value value)
      |> String.concat ", "
    in
    "{ " ^ field_strs ^ " }"
  | Sir.VConstruct { constructor; args } ->
    (match args with
     | [] ->
       (* No-payload constructor: { tag: "Foo" } *)
       "{ tag: \"" ^ constructor ^ "\" }"
     | [ single ] ->
       (* Single payload: { tag: "Foo", payload: value } *)
       "{ tag: \"" ^ constructor ^ "\", payload: " ^ emit_value single ^ " }"
     | multiple ->
       (* Multiple payloads become tuple: { tag: "Foo", payload: [a, b] } *)
       let tuple_str = multiple |> List.map emit_value |> String.concat ", " in
       "{ tag: \"" ^ constructor ^ "\", payload: [" ^ tuple_str ^ "] }")
  | Sir.VName name -> name
  | Sir.VBinOp (left, op, right) ->
    emit_value left ^ " " ^ Config.string_of_bin_op op ^ " " ^ emit_value right
  | Sir.VIfThenElse (cond, then_val, else_val) ->
    emit_value cond ^ " ? " ^ emit_value then_val ^ " : " ^ emit_value else_val
  | Sir.VMap { default; entries } ->
    (* Emit as a Map with default handling via a wrapper or inline *)
    let entries_str =
      entries
      |> List.map (fun (k, v) -> "[" ^ emit_value k ^ ", " ^ emit_value v ^ "]")
      |> String.concat ", "
    in
    "new Map([" ^ entries_str ^ "]) /* default: " ^ emit_value default ^ " */"
;;

(** Check if a variant is a simple enum (all constructors have no payload) *)
let is_simple_enum (constructors : Sir.variant_constructor list) : bool =
  constructors
  |> List.for_all (fun (vc : Sir.variant_constructor) ->
    List.length vc.vc_fields = 0)
;;

(** Emit variant constructor fields as TypeScript type *)
let emit_variant_payload (fields : Sir.Variant_field.t list) : string =
  match fields with
  | [] -> "null"
  | [ Sir.Variant_field.Positional ty ] -> emit_type_expr ty
  | [ Sir.Variant_field.Named (name, ty) ] ->
    "{ " ^ name ^ ": " ^ emit_type_expr ty ^ " }"
  | multiple ->
    (* Check if all fields are named (inline record) or positional (tuple) *)
    let all_named =
      multiple
      |> List.for_all (function
        | Sir.Variant_field.Named _ -> true
        | _ -> false)
    in
    if all_named then (
      (* Emit as object type *)
      let field_strs =
        multiple
        |> List.map (function
          | Sir.Variant_field.Named (name, ty) ->
            name ^ ": " ^ emit_type_expr ty
          | Sir.Variant_field.Positional _ -> failwith "unexpected positional")
        |> String.concat "; "
      in
      "{ " ^ field_strs ^ " }")
    else (
      (* Emit as tuple type *)
      let elem_strs =
        multiple
        |> List.map (fun field -> emit_type_expr (Sir.Variant_field.type_expr field))
        |> String.concat ", "
      in
      "[" ^ elem_strs ^ "]")
;;

(** Emit a variant constructor as a TypeScript union member *)
let emit_variant_constructor (vc : Sir.variant_constructor) : string =
  match vc.vc_fields with
  | [] -> "{ tag: \"" ^ vc.vc_name ^ "\" }"
  | _ ->
    let payload = emit_variant_payload vc.vc_fields in
    "{ tag: \"" ^ vc.vc_name ^ "\"; payload: " ^ payload ^ " }"
;;

(** Emit a type declaration as TypeScript source *)
let emit_type_decl (decl : Sir.type_decl) : string =
  match decl with
  | Sir.Variant { name; type_params; constructors } ->
    let ts_name = Config.to_pascal_case name in
    let generics = emit_type_params type_params in
    if is_simple_enum constructors then (
      (* Simple enum: type Color = "Red" | "Green" | "Blue" *)
      let members =
        constructors
        |> List.map (fun (vc : Sir.variant_constructor) ->
          "\"" ^ vc.vc_name ^ "\"")
        |> String.concat " | "
      in
      "type " ^ ts_name ^ generics ^ " = " ^ members ^ ";")
    else (
      (* Discriminated union *)
      let members =
        constructors |> List.map emit_variant_constructor |> String.concat "\n  | "
      in
      "type " ^ ts_name ^ generics ^ " =\n  | " ^ members ^ ";")
  | Sir.Record { name; type_params; fields } ->
    let ts_name = Config.to_pascal_case name in
    let generics = emit_type_params type_params in
    let field_strs =
      fields
      |> List.map (fun (rf : Sir.record_field) ->
        "  " ^ rf.rf_name ^ ": " ^ emit_type_expr rf.rf_type ^ ";")
      |> String.concat "\n"
    in
    "type " ^ ts_name ^ generics ^ " = {\n" ^ field_strs ^ "\n};"
  | Sir.Alias { name; type_params; target } ->
    let ts_name = Config.to_pascal_case name in
    let generics = emit_type_params type_params in
    "type " ^ ts_name ^ generics ^ " = " ^ emit_type_expr target ^ ";"
;;

(** Emit a value assignment as TypeScript const declaration *)
let emit_value_assignment (va : Sir.Value_assignment.t) : string =
  let type_annot = emit_type_expr va.ty in
  let value_str = emit_value va.tm in
  "const " ^ va.var_name ^ ": " ^ type_annot ^ " = " ^ value_str ^ ";"
;;

(** Emit a test declaration as TypeScript test function *)
let emit_test_decl (test : Sir.test_decl) : string =
  let args_str =
    test.f_args
    |> List.map (fun (name, _ty, value) -> name ^ ": " ^ emit_value value)
    |> String.concat ", "
  in
  let expected_str = emit_value (snd test.f_output) in
  sprintf
    {|test("%s", () => {
  // %s
  const result = %s({ %s });
  const expected = %s;
  expect(result).toEqual(expected);
});|}
    test.name test.docstr test.f_name args_str expected_str
;;

(** Emit a test suite as TypeScript test data object *)
let emit_test_suite_dict (tests : Sir.test_suite) : string =
  let entries =
    tests
    |> List.map (fun (test : Sir.test_decl) ->
      let args_obj =
        test.f_args
        |> List.map (fun (name, _ty, value) -> name ^ ": " ^ emit_value value)
        |> String.concat ", "
      in
      let expected = emit_value (snd test.f_output) in
      sprintf "  \"%s\": {\n    input: { %s },\n    expected: %s\n  }"
        test.name args_obj expected)
    |> String.concat ",\n"
  in
  "const tests = {\n" ^ entries ^ "\n};"
;;

(* Tests *)

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
  print_endline (emit_type_expr (Sir.TTuple [ Sir.TBase "int"; Sir.TBase "bool" ]));
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
  [%expect {| type Color = "Red" | "Green" | "Blue"; |}]
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
    type Shape =
      | { tag: "Point" }
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
    type Point = {
      x: number;
      y: number;
    }; |}]
;;

let%expect_test "emit_type_decl: alias" =
  let decl =
    Sir.Alias
      { name = "two_int"
      ; type_params = []
      ; target = Sir.TTuple [ Sir.TBase "int"; Sir.TBase "int" ]
      }
  in
  print_endline (emit_type_decl decl);
  [%expect {| type TwoInt = [number, number]; |}]
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
    type Maybe<A> =
      | { tag: "Just"; payload: A }
      | { tag: "Nothing" }; |}]
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
       (Sir.VConstruct { constructor = "Circle"; args = [ Sir.VConst (Sir.CInt 5) ] }));
  print_endline (emit_value (Sir.VConstruct { constructor = "Point"; args = [] }));
  [%expect {|
    { tag: "Circle", payload: 5 }
    { tag: "Point" } |}]
;;

let%expect_test "emit_value_assignment" =
  let va : Sir.Value_assignment.t =
    { var_name = "x"; ty = Sir.TBase "int"; tm = Sir.VConst (Sir.CInt 42) }
  in
  print_endline (emit_value_assignment va);
  [%expect {| const x: number = 42; |}]
;;

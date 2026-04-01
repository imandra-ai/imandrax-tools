(** Emit OCaml JSON serializers/deserializers from SIR type declarations.

    For each type, produces:
    - [<type>_of_json : Json.t -> <type>] deserializer
    - [json_of_<type> : <type> -> Json.t] serializer

    Assumes the [Json] module from the six_swiss_js POC is available
    (pure-OCaml JSON parser/emitter with no deps).
*)

module Sir = Semantic_ir

let sprintf = Printf.sprintf
let indent n s = String.make (n * 2) ' ' ^ s
let join_lines lines = String.concat "\n" lines

(* Type expression emission
   ========================= *)

(** Emit OCaml type expression as string *)
let rec emit_type_expr (te : Sir.type_expr) : string =
  match te with
  | Sir.TBase name -> Config.map_ocaml_type_name name
  | Sir.TVar name -> "'" ^ name
  | Sir.TApp ("list", [ elem ]) -> emit_type_expr elem ^ " list"
  | Sir.TApp ("option", [ elem ]) -> emit_type_expr elem ^ " option"
  | Sir.TApp (name, args) ->
    let args_str = List.map emit_type_expr args |> String.concat ", " in
    sprintf "(%s) %s" args_str name
  | Sir.TTuple exprs ->
    let parts = List.map emit_type_expr exprs in
    "(" ^ String.concat " * " parts ^ ")"
  | Sir.TArrow (arg, ret) ->
    emit_type_expr arg ^ " -> " ^ emit_type_expr ret
;;

(* Deserializer emission (JSON -> OCaml)
   ======================================= *)

(** Emit expression to deserialize a JSON value into an OCaml value of the given type *)
let rec emit_of_json_expr (te : Sir.type_expr) (json_expr : string) : string =
  match te with
  | Sir.TBase "int" -> sprintf "Z.of_int (Json.to_int %s)" json_expr
  | Sir.TBase "real" -> sprintf "Q.of_float (Json.to_float %s)" json_expr
  | Sir.TBase "bool" -> sprintf "Json.to_bool %s" json_expr
  | Sir.TBase "string" -> sprintf "Json.to_string %s" json_expr
  | Sir.TBase "unit" -> "()"
  | Sir.TBase name -> sprintf "%s_of_json %s" name json_expr
  | Sir.TApp ("list", [ elem ]) ->
    let elem_deser = emit_of_json_expr elem "x" in
    sprintf "(Stdlib.List.map (fun x -> %s) (Json.to_list %s))" elem_deser json_expr
  | Sir.TApp ("option", [ elem ]) ->
    let elem_deser = emit_of_json_expr elem json_expr in
    sprintf "(match %s with Json.Null -> None | _ -> Some (%s))" json_expr elem_deser
  | Sir.TTuple exprs ->
    let arr = sprintf "(Json.to_list %s)" json_expr in
    let bindings =
      List.mapi (fun i te ->
        let elem = sprintf "(Stdlib.List.nth __arr %d)" i in
        emit_of_json_expr te elem
      ) exprs
    in
    sprintf "(let __arr = %s in (%s))" arr (String.concat ", " bindings)
  | _ -> sprintf "(failwith \"unsupported type for of_json\")"
;;

(** Emit a full [<type>_of_json] function for a type declaration *)
let emit_of_json_decl (decl : Sir.type_decl) : string =
  match decl with
  | Sir.Record { name; fields; _ } ->
    let field_lines =
      fields |> List.map (fun (rf : Sir.record_field) ->
        let deser = emit_of_json_expr rf.rf_type
          (sprintf "(Json.field j \"%s\")" rf.rf_name) in
        sprintf "    %s = %s;" rf.rf_name deser
      )
    in
    join_lines
      [ sprintf "let %s_of_json (j : Json.t) : %s =" name name
      ; "  {"
      ]
    ^ "\n"
    ^ join_lines field_lines
    ^ "\n  }\n"
  | Sir.Variant { name; constructors; _ } ->
    let is_enum = List.for_all
      (fun (vc : Sir.variant_constructor) -> vc.vc_fields = []) constructors
    in
    if is_enum then
      (* Simple enum: deserialize from flat string *)
      let cases =
        constructors |> List.map (fun (vc : Sir.variant_constructor) ->
          sprintf "  | \"%s\" -> %s" vc.vc_name vc.vc_name
        )
      in
      join_lines
        ([ sprintf "let %s_of_json (j : Json.t) : %s =" name name
         ; "  match Json.to_string j with"
         ] @ cases
         @ [ sprintf "  | s -> raise (Json.Json_error (\"unknown %s: \" ^ s))" name ])
      ^ "\n"
    else
      (* Tagged variant: deserialize from {"tag": ..., ...} *)
      let cases =
        constructors |> List.map (fun (vc : Sir.variant_constructor) ->
          match vc.vc_fields with
          | [] ->
            sprintf "  | \"%s\" -> %s" vc.vc_name vc.vc_name
          | [ Sir.Variant_field.Positional te ] ->
            let deser = emit_of_json_expr te "(Json.field j \"value\")" in
            sprintf "  | \"%s\" -> %s (%s)" vc.vc_name vc.vc_name deser
          | fields ->
            let named =
              fields |> List.map (fun field ->
                match field with
                | Sir.Variant_field.Named (n, te) ->
                  let deser = emit_of_json_expr te
                    (sprintf "(Json.field j \"%s\")" n) in
                  sprintf "%s = %s" n deser
                | Sir.Variant_field.Positional _ ->
                  "(failwith \"unsupported positional variant field\")"
              )
            in
            sprintf "  | \"%s\" -> %s { %s }" vc.vc_name vc.vc_name
              (String.concat "; " named)
        )
      in
      join_lines
        ([ sprintf "let %s_of_json (j : Json.t) : %s =" name name
         ; sprintf "  match Json.to_string (Json.field j \"tag\") with"
         ] @ cases
         @ [ sprintf "  | s -> raise (Json.Json_error (\"unknown %s: \" ^ s))" name ])
      ^ "\n"
  | Sir.Alias { name; target; _ } ->
    sprintf "let %s_of_json (j : Json.t) : %s =\n  %s\n"
      name name (emit_of_json_expr target "j")
;;

(* Serializer emission (OCaml -> JSON)
   ===================================== *)

(** Emit expression to serialize an OCaml value into JSON *)
let rec emit_to_json_expr (te : Sir.type_expr) (val_expr : string) : string =
  match te with
  | Sir.TBase "int" -> sprintf "Json.Number (Stdlib.Float.of_int (Z.to_int %s))" val_expr
  | Sir.TBase "real" -> sprintf "Json.Number (Q.to_float %s)" val_expr
  | Sir.TBase "bool" ->
    sprintf "Json.Bool %s" val_expr
  | Sir.TBase "string" ->
    sprintf "Json.String %s" val_expr
  | Sir.TBase "unit" -> "Json.Null"
  | Sir.TBase name -> sprintf "json_of_%s %s" name val_expr
  | Sir.TApp ("list", [ elem ]) ->
    let elem_ser = emit_to_json_expr elem "x" in
    sprintf "Json.Array (Stdlib.List.map (fun x -> %s) %s)" elem_ser val_expr
  | Sir.TApp ("option", [ elem ]) ->
    let elem_ser = emit_to_json_expr elem "v" in
    sprintf "(match %s with None -> Json.Null | Some v -> %s)" val_expr elem_ser
  | Sir.TTuple exprs ->
    let n = List.length exprs in
    let vars = List.init n (fun i -> sprintf "__v%d" i) in
    let pat = "(" ^ String.concat ", " vars ^ ")" in
    let elems =
      List.map2 (fun var te -> emit_to_json_expr te var) vars exprs
    in
    sprintf "(let %s = %s in Json.Array [%s])"
      pat val_expr (String.concat "; " elems)
  | _ -> sprintf "(failwith \"unsupported type for to_json\")"
;;

(** Emit a full [json_of_<type>] function for a type declaration *)
let emit_to_json_decl (decl : Sir.type_decl) : string =
  match decl with
  | Sir.Record { name; fields; _ } ->
    let field_lines =
      fields |> List.map (fun (rf : Sir.record_field) ->
        let ser = emit_to_json_expr rf.rf_type
          (sprintf "v.%s" rf.rf_name) in
        sprintf "    \"%s\", %s;" rf.rf_name ser
      )
    in
    join_lines
      [ sprintf "let json_of_%s (v : %s) : Json.t =" name name
      ; "  Json.Object ["
      ]
    ^ "\n"
    ^ join_lines field_lines
    ^ "\n  ]\n"
  | Sir.Variant { name; constructors; _ } ->
    let is_enum = List.for_all
      (fun (vc : Sir.variant_constructor) -> vc.vc_fields = []) constructors
    in
    if is_enum then
      (* Simple enum: serialize to flat string *)
      let cases =
        constructors |> List.map (fun (vc : Sir.variant_constructor) ->
          sprintf "  | %s -> Json.String \"%s\"" vc.vc_name vc.vc_name
        )
      in
      join_lines
        ([ sprintf "let json_of_%s (v : %s) : Json.t =" name name
         ; sprintf "  match v with"
         ] @ cases)
      ^ "\n"
    else
      (* Tagged variant: serialize to {"tag": ..., ...} *)
      let cases =
        constructors |> List.map (fun (vc : Sir.variant_constructor) ->
          match vc.vc_fields with
          | [] ->
            sprintf "  | %s ->\n    Json.Object [\"tag\", Json.String \"%s\"]"
              vc.vc_name vc.vc_name
          | [ Sir.Variant_field.Positional te ] ->
            let ser = emit_to_json_expr te "v" in
            sprintf "  | %s v ->\n    Json.Object [\"tag\", Json.String \"%s\"; \"value\", %s]"
              vc.vc_name vc.vc_name ser
          | fields ->
            let named_fields =
              fields |> CCList.filter_map (fun field ->
                match field with
                | Sir.Variant_field.Named (n, te) ->
                  let ser = emit_to_json_expr te (sprintf "r.%s" n) in
                  Some (sprintf "\"%s\", %s" n ser)
                | Sir.Variant_field.Positional _ -> None
              )
            in
            sprintf "  | %s r ->\n    Json.Object [\"tag\", Json.String \"%s\"; %s]"
              vc.vc_name vc.vc_name (String.concat "; " named_fields)
        )
      in
      join_lines
        ([ sprintf "let json_of_%s (v : %s) : Json.t =" name name
         ; sprintf "  match v with"
         ] @ cases)
      ^ "\n"
  | Sir.Alias { name; target; _ } ->
    sprintf "let json_of_%s (v : %s) : Json.t =\n  %s\n"
      name name (emit_to_json_expr target "v")
;;

(** Emit both serializer and deserializer for a type declaration *)
let emit_type_codec (decl : Sir.type_decl) : string =
  emit_of_json_decl decl ^ "\n" ^ emit_to_json_decl decl
;;

(** Emit codecs for a list of type declarations *)
let emit_type_codecs (decls : Sir.type_decl list) : string =
  decls |> List.map emit_type_codec |> String.concat "\n"
;;

(* Wrapper main emission
   ======================= *)

(** Emit the OCaml wrapper [main] function.

    Args:
      module_name: the OCaml module containing the target function (e.g. "Six_swiss")
      func_name: the function to call (e.g. "match_price")
      args: list of (arg_name, arg_type) for the function's parameters
      ret_type: the return type
*)
let emit_wrapper_main
    ~(module_name : string)
    ~(func_name : string)
    ~(args : (string * Sir.type_expr) list)
    ~(ret_type : Sir.type_expr)
  : string =
  let deser_lines =
    args |> List.map (fun (name, te) ->
      let deser = emit_of_json_expr te
        (sprintf "(Json.field json \"%s\")" name) in
      sprintf "  let %s = %s in" name deser
    )
  in
  let arg_names = args |> List.map fst |> String.concat " " in
  let ser = emit_to_json_expr ret_type "result" in
  join_lines
    ([ "let () ="
     ; "  let buf = Stdlib.Buffer.create 4096 in"
     ; "  (try while true do"
     ; "     Stdlib.Buffer.add_char buf (Stdlib.input_char Stdlib.stdin)"
     ; "   done with Stdlib.End_of_file -> ());"
     ; "  let input = Stdlib.Buffer.contents buf in"
     ; "  let json = Json.parse input in"
     ] @ deser_lines
     @ [ sprintf "  let result = %s.%s %s in" module_name func_name arg_names
       ; sprintf "  Stdlib.print_string (Json.emit (%s))" ser
       ])
  ^ "\n"
;;

(* Tests
   ====== *)

let%expect_test "emit_of_json_decl: record" =
  let decl =
    Sir.Record
      { name = "order"
      ; type_params = []
      ; fields =
          [ { rf_name = "order_id"; rf_type = Sir.TBase "int" }
          ; { rf_name = "order_price"; rf_type = Sir.TBase "real" }
          ; { rf_name = "order_type"; rf_type = Sir.TBase "order_type" }
          ]
      }
  in
  print_string (emit_of_json_decl decl);
  [%expect {|
    let order_of_json (j : Json.t) : order =
      {
        order_id = Z.of_int (Json.to_int (Json.field j "order_id"));
        order_price = Q.of_float (Json.to_float (Json.field j "order_price"));
        order_type = order_type_of_json (Json.field j "order_type");
      } |}]
;;

let%expect_test "emit_to_json_decl: record" =
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
  print_string (emit_to_json_decl decl);
  [%expect {|
    let json_of_order (v : order) : Json.t =
      Json.Object [
        "order_id", Json.Number (Stdlib.Float.of_int (Z.to_int v.order_id));
        "order_price", Json.Number (Q.to_float v.order_price);
      ] |}]
;;

let%expect_test "emit_of_json_decl: simple variant (enum)" =
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
  print_string (emit_of_json_decl decl);
  [%expect {|
    let order_type_of_json (j : Json.t) : order_type =
      match Json.to_string j with
      | "Market" -> Market
      | "Limit" -> Limit
      | "Quote" -> Quote
      | s -> raise (Json.Json_error ("unknown order_type: " ^ s)) |}]
;;

let%expect_test "emit_to_json_decl: simple variant (enum)" =
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
  print_string (emit_to_json_decl decl);
  [%expect {|
    let json_of_order_type (v : order_type) : Json.t =
      match v with
      | Market -> Json.String "Market"
      | Limit -> Json.String "Limit"
      | Quote -> Json.String "Quote" |}]
;;

let%expect_test "emit_wrapper_main" =
  let args =
    [ "ob", Sir.TBase "order_book"
    ; "ref_price", Sir.TBase "real"
    ]
  in
  let ret_type = Sir.TApp ("option", [ Sir.TBase "real" ]) in
  print_string (emit_wrapper_main
    ~module_name:"Six_swiss"
    ~func_name:"match_price"
    ~args ~ret_type);
  [%expect {|
    let () =
      let buf = Stdlib.Buffer.create 4096 in
      (try while true do
         Stdlib.Buffer.add_char buf (Stdlib.input_char Stdlib.stdin)
       done with Stdlib.End_of_file -> ());
      let input = Stdlib.Buffer.contents buf in
      let json = Json.parse input in
      let ob = order_book_of_json (Json.field json "ob") in
      let ref_price = Q.of_float (Json.to_float (Json.field json "ref_price")) in
      let result = Six_swiss.match_price ob ref_price in
      Stdlib.print_string (Json.emit ((match result with None -> Json.Null | Some v -> Json.Number (Q.to_float v)))) |}]
;;

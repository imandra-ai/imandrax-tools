open Printf
open Parse_common

include Parse_term
include Parse_model
include Parse_fun_decomp

(* Expect tests
==================== *)

let sep : string = "\n" ^ CCString.repeat "<>" 10 ^ "\n"

(* Decl
-------------------- *)

let%expect_test "parse decl art" =
  (* let yaml_str = CCIO.File.read_exn "../test/data/decl/record.yaml" in *)
  (* let yaml_str = CCIO.File.read_exn "../test/data/decl/variant_simple.yaml" in *)
  let yaml_str = CCIO.File.read_exn "../test/data/decl/variant_with_data.yaml" in
  let (yaml : Yaml.value) = Yaml.of_string_exn yaml_str in
  let name, code, arts =
    match yaml with
    | `O fields ->
        let name =
          List.assoc "name" fields |> function
          | `String s -> s
          | _ -> failwith "name must be string"
        in
        let code =
          List.assoc "iml" fields |> function
          | `String s -> s
          | _ -> failwith "iml must be string"
        in
        let arts =
          match List.assoc_opt "get_decls_res" fields with
          | Some (`O [ ("decls", `A decls); ("not_found", `A _) ]) ->
              decls
              |> List.map (function
                   | `O delc_assoc -> List.assoc "artifact" delc_assoc
                   | _ -> failwith "decls must be an array of objects")
          | None -> failwith "get_decls_res is missing"
          | _ -> failwith "get_decls_res must be an array"
        in
        (name, code, arts)
    | _ -> failwith "Invalid yaml: top level must be object"
  in

  let _ = arts in

  let decls = arts |> List.map Art_utils.yaml_to_decl in

  printf "name: %s\n" name;
  printf "code:\n %s\n" code;
  printf "<><><><><><><><><>\n";

  let fmt = Format.str_formatter in
  List.iter
    (fun decl -> Format.fprintf fmt "%a@?" Pretty_print.pp_decl decl)
    decls;
  print_endline (Format.flush_str_formatter ());
  [%expect
    {|
    name: variant_with_data
    code:
     type shape =
      | Circle of int
      | Rectangle of int * int

    let area = fun s ->
      match s with
      | Circle r -> r * r
      | Rectangle (w, h) -> w * h

    <><><><><><><><><>
    Ty
      {
      name = shape/v_i3bLLE-uDuzDsq0PP6ZlOd3BpxRihJWXKha-SQ5ZE;
      params = [];
      decl =
        Algebraic
          [{
             c = Circle/S_Cmpwoi8d1foHX9rOJm3zWVQipeHLWH5HjimJ6oDt8;
             labels = None;
             args = [{ view = (Constr (int,[]));
                       generation = 1 }];
             doc = None
             };
           {
             c = Rectangle/rkX3Qq3NXp4EFP5Js2YdOCcAKeXLgAMN6gmGAE4C7Xk;
             labels = None;
             args =
               [{ view = (Constr (int,[]));
                  generation = 1 };
                { view = (Constr (int,[]));
                  generation = 1 }];
             doc = None
             }];
      clique = None;
      timeout = None
      }
    |}]

(* Decomp
-------------------- *)

let%expect_test "parse fun decomp art" =
  let yaml_str = CCIO.File.read_exn "../test/data/fun_decomp/basic.yaml" in
  let (yaml : Yaml.value) = Yaml.of_string_exn yaml_str in
  let name, code, art =
    match yaml with
    | `O fields ->
        let name =
          List.assoc "name" fields |> function
          | `String s -> s
          | _ -> failwith "name must be string"
        in
        let code =
          List.assoc "iml" fields |> function
          | `String s -> s
          | _ -> failwith "iml must be string"
        in
        let art =
          match List.assoc_opt "decomp_res" fields with
          | Some (`O res_fields) -> (
              match List.assoc_opt "artifact" res_fields with
              | Some art -> art
              | None -> failwith "artifact not found in decomp_res")
          | Some `Null | None -> failwith "decomp_res is empty or missing"
          | _ -> failwith "decomp_res must be object"
        in
        (name, code, art)
    | _ -> failwith "Invalid yaml: top level must be object"
  in

  printf "name: %s\n" name;
  printf "code:\n %s\n" code;

  let (fun_decomp : Mir.Fun_decomp.t) = Art_utils.yaml_to_fun_decomp art in

  printf "Fun decomp:\n";
  let fmt = Format.str_formatter in
  Format.fprintf fmt "%a@?" Pretty_print.pp_fun_decomp fun_decomp;
  print_endline (Format.flush_str_formatter ());

  printf "Parsed AST:\n";

  let parsed = parse_fun_decomp fun_decomp in
  List.iter (fun stmt -> print_endline (Ast.show_stmt stmt)) parsed;

  ();
  [%expect
    {|
    name: basic
    code:
     let g = fun x -> x + 1

    let f = fun x -> if x > 0 then x + 2 else g x

    Fun decomp:
    {
      f_id = f/u-V_2hDBsgPLnBVARN3d7lwMjeshy0JEtJSUqjWmJj8;
      f_args =
        [{ id = x/116441; ty = { view = (Constr (int,[]));
                                 generation = 3 } }];
      regions =
        [(2 elements)
         {
           constraints =
             [{ view =
                  Apply {f = { view =
                                 (Sym
                                   (>= : { view =
                                             (Arrow ((),
                                                     { view = (Constr (int,[]));
                                                       generation = 3 },
                                                     { view =
                                                         (Arrow ((),
                                                                 { view =
                                                                     (Constr
                                                                       (int,[]));
                                                                   generation = 3 },
                                                                 { view =
                                                                     (Constr
                                                                       (bool,[]));
                                                                   generation = 3 }));
                                                       generation = 3 }));
                                           generation = 3 }));
                               ty =
                                 { view =
                                     (Arrow ((),
                                             { view = (Constr (int,[]));
                                               generation = 3 },
                                             { view =
                                                 (Arrow ((),
                                                         { view =
                                                             (Constr (int,[]));
                                                           generation = 3 },
                                                         { view =
                                                             (Constr (bool,[]));
                                                           generation = 3 }));
                                               generation = 3 }));
                                   generation = 3 };
                               generation = 1;
                               sub_anchor = None };
                         l =
                           [{ view =
                                (Var
                                  { id = x/116441;
                                    ty =
                                    { view = (Constr (int,[]));
                                      generation = 3 }
                                    });
                              ty = { view = (Constr (int,[]));
                                     generation = 3 };
                              generation = 1;
                              sub_anchor = None };
                            { view = (Const 1);
                              ty = { view = (Constr (int,[]));
                                     generation = 3 };
                              generation = 1;
                              sub_anchor = None }]
                         };
                ty = { view = (Constr (bool,[]));
                       generation = 3 };
                generation = 1;
                sub_anchor = None }];
           invariant =
             { view =
                 Apply {f = { view =
                                (Sym
                                  (+ : { view =
                                           (Arrow ((),
                                                   { view = (Constr (int,[]));
                                                     generation = 3 },
                                                   { view =
                                                       (Arrow ((),
                                                               { view =
                                                                   (Constr
                                                                     (int,[]));
                                                                 generation = 3 },
                                                               { view =
                                                                   (Constr
                                                                     (int,[]));
                                                                 generation = 3 }));
                                                     generation = 3 }));
                                         generation = 3 }));
                              ty =
                                { view =
                                    (Arrow ((),
                                            { view = (Constr (int,[]));
                                              generation = 3 },
                                            { view =
                                                (Arrow ((),
                                                        { view =
                                                            (Constr (int,[]));
                                                          generation = 3 },
                                                        { view =
                                                            (Constr (int,[]));
                                                          generation = 3 }));
                                              generation = 3 }));
                                  generation = 3 };
                              generation = 1;
                              sub_anchor = None };
                        l =
                          [{ view =
                               (Var
                                 { id = x/116441;
                                   ty =
                                   { view = (Constr (int,[]));
                                     generation = 3 }
                                   });
                             ty = { view = (Constr (int,[]));
                                    generation = 3 };
                             generation = 1;
                             sub_anchor = None };
                           { view = (Const 2);
                             ty = { view = (Constr (int,[]));
                                    generation = 3 };
                             generation = 1;
                             sub_anchor = None }]
                        };
               ty = { view = (Constr (int,[]));
                      generation = 3 };
               generation = 1;
               sub_anchor = None };
           meta =
             ["str":
                Assoc
                  {"model_eval": String "3";
                   "invariant": String "x + 2";
                   "constraints": List [String "x >= 1"];
                   "model": Assoc {"x": String "1"}};
              "model":
                Assoc
                  {"x":
                     Term
                       { view = (Const 1);
                         ty = { view = (Constr (int,[]));
                                generation = 3 };
                         generation = 1;
                         sub_anchor = None }};
              "model_eval":
                Term
                  { view = (Const 3);
                    ty = { view = (Constr (int,[]));
                           generation = 3 };
                    generation = 1;
                    sub_anchor = None };
              "id": String "403df3c1-816f-4d4a-9414-877a974b6548"];
           status =
             Feasible
               { tys = [];
                 consts =
                 [((x/116441 : { view = (Constr (int,[]));
                                 generation = 3 }),
                   { view = (Const 1);
                     ty = { view = (Constr (int,[]));
                            generation = 3 };
                     generation = 1;
                     sub_anchor = None })
                   ];
                 funs = []; representable = true; completed = false;
                 ty_subst = [] }
           };
         {
           constraints =
             [{ view =
                  Apply {f = { view =
                                 (Sym
                                   (<= : { view =
                                             (Arrow ((),
                                                     { view = (Constr (int,[]));
                                                       generation = 3 },
                                                     { view =
                                                         (Arrow ((),
                                                                 { view =
                                                                     (Constr
                                                                       (int,[]));
                                                                   generation = 3 },
                                                                 { view =
                                                                     (Constr
                                                                       (bool,[]));
                                                                   generation = 3 }));
                                                       generation = 3 }));
                                           generation = 3 }));
                               ty =
                                 { view =
                                     (Arrow ((),
                                             { view = (Constr (int,[]));
                                               generation = 3 },
                                             { view =
                                                 (Arrow ((),
                                                         { view =
                                                             (Constr (int,[]));
                                                           generation = 3 },
                                                         { view =
                                                             (Constr (bool,[]));
                                                           generation = 3 }));
                                               generation = 3 }));
                                   generation = 3 };
                               generation = 1;
                               sub_anchor = None };
                         l =
                           [{ view =
                                (Var
                                  { id = x/116441;
                                    ty =
                                    { view = (Constr (int,[]));
                                      generation = 3 }
                                    });
                              ty = { view = (Constr (int,[]));
                                     generation = 3 };
                              generation = 1;
                              sub_anchor = None };
                            { view = (Const 0);
                              ty = { view = (Constr (int,[]));
                                     generation = 3 };
                              generation = 1;
                              sub_anchor = None }]
                         };
                ty = { view = (Constr (bool,[]));
                       generation = 3 };
                generation = 1;
                sub_anchor = None }];
           invariant =
             { view =
                 Apply {f = { view =
                                (Sym
                                  (+ : { view =
                                           (Arrow ((),
                                                   { view = (Constr (int,[]));
                                                     generation = 3 },
                                                   { view =
                                                       (Arrow ((),
                                                               { view =
                                                                   (Constr
                                                                     (int,[]));
                                                                 generation = 3 },
                                                               { view =
                                                                   (Constr
                                                                     (int,[]));
                                                                 generation = 3 }));
                                                     generation = 3 }));
                                         generation = 3 }));
                              ty =
                                { view =
                                    (Arrow ((),
                                            { view = (Constr (int,[]));
                                              generation = 3 },
                                            { view =
                                                (Arrow ((),
                                                        { view =
                                                            (Constr (int,[]));
                                                          generation = 3 },
                                                        { view =
                                                            (Constr (int,[]));
                                                          generation = 3 }));
                                              generation = 3 }));
                                  generation = 3 };
                              generation = 1;
                              sub_anchor = None };
                        l =
                          [{ view = (Const 1);
                             ty = { view = (Constr (int,[]));
                                    generation = 3 };
                             generation = 1;
                             sub_anchor = None };
                           { view =
                               (Var
                                 { id = x/116441;
                                   ty =
                                   { view = (Constr (int,[]));
                                     generation = 3 }
                                   });
                             ty = { view = (Constr (int,[]));
                                    generation = 3 };
                             generation = 1;
                             sub_anchor = None }]
                        };
               ty = { view = (Constr (int,[]));
                      generation = 3 };
               generation = 1;
               sub_anchor = None };
           meta =
             ["str":
                Assoc
                  {"model_eval": String "1";
                   "invariant": String "1 + x";
                   "constraints": List [String "x <= 0"];
                   "model": Assoc {"x": String "0"}};
              "model":
                Assoc
                  {"x":
                     Term
                       { view = (Const 0);
                         ty = { view = (Constr (int,[]));
                                generation = 3 };
                         generation = 1;
                         sub_anchor = None }};
              "model_eval":
                Term
                  { view = (Const 1);
                    ty = { view = (Constr (int,[]));
                           generation = 3 };
                    generation = 1;
                    sub_anchor = None };
              "id": String "7c9c52f3-519f-47a5-934c-d7dede67083d"];
           status =
             Feasible
               { tys = [];
                 consts =
                 [((x/116441 : { view = (Constr (int,[]));
                                 generation = 3 }),
                   { view = (Const 0);
                     ty = { view = (Constr (int,[]));
                            generation = 3 };
                     generation = 1;
                     sub_anchor = None })
                   ];
                 funs = []; representable = true; completed = false;
                 ty_subst = [] }
           }]
      }
    Parsed AST:
    (Ast.FunctionDef
       { Ast.name = "test_1";
         args =
         { Ast.posonlyargs = []; args = []; vararg = None; kwonlyargs = [];
           kw_defaults = []; kwarg = None; defaults = [] };
         body =
         [Ast.ExprStmt {
            value =
            (Ast.Constant
               { Ast.value =
                 (Ast.String
                    "test_1\n\n- invariant: x + 2\n- constraints:\n    - x >= 1\n");
                 kind = None })};
           (Ast.AnnAssign
              { Ast.target = (Ast.Name { Ast.id = "result"; ctx = Ast.Load });
                annotation = (Ast.Name { Ast.id = "int"; ctx = Ast.Load });
                value =
                (Some (Ast.Call
                         { Ast.func = (Ast.Name { Ast.id = "f"; ctx = Ast.Load });
                           args = [];
                           keywords =
                           [{ Ast.arg = (Some "x");
                              value =
                              (Ast.Constant
                                 { Ast.value = (Ast.Int 1); kind = None })
                              }
                             ]
                           }));
                simple = 1 });
           (Ast.AnnAssign
              { Ast.target = (Ast.Name { Ast.id = "expected"; ctx = Ast.Load });
                annotation = (Ast.Name { Ast.id = "int"; ctx = Ast.Load });
                value =
                (Some (Ast.Constant { Ast.value = (Ast.Int 3); kind = None }));
                simple = 1 });
           Ast.Assert {
             test =
             Ast.Compare {
               left = (Ast.Name { Ast.id = "result"; ctx = Ast.Load });
               ops = [Ast.Eq];
               comparators = [(Ast.Name { Ast.id = "expected"; ctx = Ast.Load })]};
             msg = None}
           ];
         decorator_list = []; returns = None; type_comment = None;
         type_params = [] })
    (Ast.FunctionDef
       { Ast.name = "test_2";
         args =
         { Ast.posonlyargs = []; args = []; vararg = None; kwonlyargs = [];
           kw_defaults = []; kwarg = None; defaults = [] };
         body =
         [Ast.ExprStmt {
            value =
            (Ast.Constant
               { Ast.value =
                 (Ast.String
                    "test_2\n\n- invariant: 1 + x\n- constraints:\n    - x <= 0\n");
                 kind = None })};
           (Ast.AnnAssign
              { Ast.target = (Ast.Name { Ast.id = "result"; ctx = Ast.Load });
                annotation = (Ast.Name { Ast.id = "int"; ctx = Ast.Load });
                value =
                (Some (Ast.Call
                         { Ast.func = (Ast.Name { Ast.id = "f"; ctx = Ast.Load });
                           args = [];
                           keywords =
                           [{ Ast.arg = (Some "x");
                              value =
                              (Ast.Constant
                                 { Ast.value = (Ast.Int 0); kind = None })
                              }
                             ]
                           }));
                simple = 1 });
           (Ast.AnnAssign
              { Ast.target = (Ast.Name { Ast.id = "expected"; ctx = Ast.Load });
                annotation = (Ast.Name { Ast.id = "int"; ctx = Ast.Load });
                value =
                (Some (Ast.Constant { Ast.value = (Ast.Int 1); kind = None }));
                simple = 1 });
           Ast.Assert {
             test =
             Ast.Compare {
               left = (Ast.Name { Ast.id = "result"; ctx = Ast.Load });
               ops = [Ast.Eq];
               comparators = [(Ast.Name { Ast.id = "expected"; ctx = Ast.Load })]};
             msg = None}
           ];
         decorator_list = []; returns = None; type_comment = None;
         type_params = [] })
    |}]

(* Model
-------------------- *)

let%expect_test "parse model art" =
  (* let yaml_str = CCIO.File.read_exn "../examples/art/art.yaml" in *)
  let yaml_str =
    (* CCIO.File.read_exn "../test/data/model/composite/map_default_value_only.yaml" *)
    (* CCIO.File.read_exn "../test/data/model/primitive/empty_list.yaml" *)
    CCIO.File.read_exn
      "../test/data/model/polymorphic/annotated_polymorphic.yaml"
    (* CCIO.File.read_exn "../test/data/model/polymorphic/ambiguous_type_name.yaml" *)
  in
  let yaml = Yaml.of_string_exn yaml_str in

  let name, code =
    match yaml with
    | `O assoc ->
        let name =
          match List.assoc_opt "name" assoc with
          | Some (`String s) -> s
          | _ -> "unknown"
        in
        let code =
          match List.assoc_opt "iml" assoc with
          | Some (`String s) -> s
          | _ -> "unknown"
        in
        (name, code)
    | _ -> ("unknown name", "unknown value")
  in
  printf "name: %s\n" name;
  printf "code: %s\n" code;

  let model = Art_utils.yaml_to_model yaml in
  let app_sym, term = unpack_model model in

  (* Create a custom formatter with wider margin *)
  let fmt = Format.str_formatter in
  Format.pp_set_margin fmt 400;
  Format.pp_set_max_indent fmt 390;

  let _term_view = term.view in

  print_endline sep;

  print_endline "Applied symbol:";
  Format.fprintf fmt "%a@?"
    (Imandrax_api_common.Applied_symbol.pp_t_poly Mir.Type.pp)
    app_sym;
  print_endline (Format.flush_str_formatter ());
  printf "%s\n" sep;

  print_endline "Term:";
  Format.fprintf fmt "%a@?" Pretty_print.pp_term term;
  print_endline (Format.flush_str_formatter ());
  printf "%s\n" sep;

  (* print_endline "Term view:";
  Format.fprintf fmt "%a@?" (Term.pp_view Type.pp) term_view;
  print_endline (Format.flush_str_formatter ());
  printf "%s\n" sep; *)
  print_endline "Parsing term:\n";
  let ty_defs, type_annot, expr =
    match parse_term term with
    | Ok (ty_defs, type_annot, expr) -> (ty_defs, type_annot, expr)
    | Error msg -> failwith msg
  in

  printf "Type defs:\n";
  List.iter (fun ty_def -> print_endline (Ast.show_stmt ty_def)) ty_defs;

  printf "Type annot: ";
  (match type_annot with
  | None -> print_endline "None"
  | Some ty_annot -> print_endline (Ast.show_expr ty_annot));

  printf "\n";
  printf "Expr:\n";
  print_endline (Ast.show_expr expr);

  print_endline sep;
  print_endline "Json:\n";

  let stmts = parse_model model in

  (* Convert each statement to JSON and collect them in a list *)
  let json_out : Yojson.Safe.t = `List (List.map Ast.stmt_to_yojson stmts) in

  (* Also print to stdout for the test *)
  Yojson.Safe.pretty_to_string json_out |> print_endline;

  [%expect
    {|
    name: annotated_polymorphic
    code: let v =  (fun (w: _ list) -> if w = [] then true else false)

    <><><><><><><><><><>

    Applied symbol:
    (w/97332 : { view = (Constr (list, [{ view = (Var a/97331); generation = 5 }])); generation = 5 })

    <><><><><><><><><><>

    Term:
    { view = Construct {c = ([] : { view = (Constr (list,[{ view = (Constr (_a_0/6fPTIEu6GifOtjqDBtCi54oXN93v3kMRHpj650evVOI,[]));
                                                            generation = 5 }]));
                                    generation = 5 });args = []};
      ty = { view = (Constr (list,[{ view = (Constr (_a_0/6fPTIEu6GifOtjqDBtCi54oXN93v3kMRHpj650evVOI,[]));
                                     generation = 5 }]));
             generation = 5 };
      generation = 2; sub_anchor = None }

    <><><><><><><><><><>

    Parsing term:

    Type defs:
    Type annot: None

    Expr:
    (Ast.List { Ast.elts = []; ctx = Ast.Load })

    <><><><><><><><><><>

    Json:

    [
      [
        "Assign",
        {
          "targets": [ [ "Name", { "id": "w", "ctx": [ "Load" ] } ] ],
          "value": [ "List", { "elts": [], "ctx": [ "Load" ] } ],
          "type_comment": null
        }
      ]
    ]
    |}]

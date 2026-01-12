open Printf
open Parse_common

include Parse_term
include Parse_model
include Parse_fun_decomp
include Parse_decl

(* Expect tests
==================== *)

let sep : string = "\n" ^ CCString.repeat "<>" 10 ^ "\n"

(* Decl
-------------------- *)

(* TODO: move from parse_decl *)

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
  [%expect {|
    name: basic
    code:
     let g = fun x -> x + 1

    let f = fun x -> if x > 0 then x + 2 else g x
    |}];

  let (fun_decomp : Mir.Fun_decomp.t) = Art_utils.yaml_to_fun_decomp art in

  printf "Fun decomp:\n";
  let fmt = Format.str_formatter in
  Format.fprintf fmt "%a@?" Pretty_print.pp_fun_decomp fun_decomp;
  print_endline (Format.flush_str_formatter ());

  printf "Parsed AST:\n";

  let parsed = parse_fun_decomp fun_decomp in
  List.iter (fun stmt -> print_endline (Ast.show_stmt stmt)) parsed;

  ();
  [%expect {|
    Fun decomp:
    {
      f_id = f/u-V_2hDBsgPLnBVARN3d7lwMjeshy0JEtJSUqjWmJj8;
      f_args =
        [{ id = x/116441; ty = { view = (Constr (int,[]));
                                 generation = 1 } }];
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
                                                       generation = 1 },
                                                     { view =
                                                         (Arrow ((),
                                                                 { view =
                                                                     (Constr
                                                                       (int,[]));
                                                                   generation = 1 },
                                                                 { view =
                                                                     (Constr
                                                                       (bool,[]));
                                                                   generation = 1 }));
                                                       generation = 1 }));
                                           generation = 1 }));
                               ty =
                                 { view =
                                     (Arrow ((),
                                             { view = (Constr (int,[]));
                                               generation = 1 },
                                             { view =
                                                 (Arrow ((),
                                                         { view =
                                                             (Constr (int,[]));
                                                           generation = 1 },
                                                         { view =
                                                             (Constr (bool,[]));
                                                           generation = 1 }));
                                               generation = 1 }));
                                   generation = 1 };
                               generation = 0;
                               sub_anchor = None };
                         l =
                           [{ view =
                                (Var
                                  { id = x/116441;
                                    ty =
                                    { view = (Constr (int,[]));
                                      generation = 1 }
                                    });
                              ty = { view = (Constr (int,[]));
                                     generation = 1 };
                              generation = 0;
                              sub_anchor = None };
                            { view = (Const 1);
                              ty = { view = (Constr (int,[]));
                                     generation = 1 };
                              generation = 0;
                              sub_anchor = None }]
                         };
                ty = { view = (Constr (bool,[]));
                       generation = 1 };
                generation = 0;
                sub_anchor = None }];
           invariant =
             { view =
                 Apply {f = { view =
                                (Sym
                                  (+ : { view =
                                           (Arrow ((),
                                                   { view = (Constr (int,[]));
                                                     generation = 1 },
                                                   { view =
                                                       (Arrow ((),
                                                               { view =
                                                                   (Constr
                                                                     (int,[]));
                                                                 generation = 1 },
                                                               { view =
                                                                   (Constr
                                                                     (int,[]));
                                                                 generation = 1 }));
                                                     generation = 1 }));
                                         generation = 1 }));
                              ty =
                                { view =
                                    (Arrow ((),
                                            { view = (Constr (int,[]));
                                              generation = 1 },
                                            { view =
                                                (Arrow ((),
                                                        { view =
                                                            (Constr (int,[]));
                                                          generation = 1 },
                                                        { view =
                                                            (Constr (int,[]));
                                                          generation = 1 }));
                                              generation = 1 }));
                                  generation = 1 };
                              generation = 0;
                              sub_anchor = None };
                        l =
                          [{ view =
                               (Var
                                 { id = x/116441;
                                   ty =
                                   { view = (Constr (int,[]));
                                     generation = 1 }
                                   });
                             ty = { view = (Constr (int,[]));
                                    generation = 1 };
                             generation = 0;
                             sub_anchor = None };
                           { view = (Const 2);
                             ty = { view = (Constr (int,[]));
                                    generation = 1 };
                             generation = 0;
                             sub_anchor = None }]
                        };
               ty = { view = (Constr (int,[]));
                      generation = 1 };
               generation = 0;
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
                                generation = 1 };
                         generation = 0;
                         sub_anchor = None }};
              "model_eval":
                Term
                  { view = (Const 3);
                    ty = { view = (Constr (int,[]));
                           generation = 1 };
                    generation = 0;
                    sub_anchor = None };
              "id": String "403df3c1-816f-4d4a-9414-877a974b6548"];
           status =
             Feasible
               { tys = [];
                 consts =
                 [((x/116441 : { view = (Constr (int,[]));
                                 generation = 1 }),
                   { view = (Const 1);
                     ty = { view = (Constr (int,[]));
                            generation = 1 };
                     generation = 0;
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
                                                       generation = 1 },
                                                     { view =
                                                         (Arrow ((),
                                                                 { view =
                                                                     (Constr
                                                                       (int,[]));
                                                                   generation = 1 },
                                                                 { view =
                                                                     (Constr
                                                                       (bool,[]));
                                                                   generation = 1 }));
                                                       generation = 1 }));
                                           generation = 1 }));
                               ty =
                                 { view =
                                     (Arrow ((),
                                             { view = (Constr (int,[]));
                                               generation = 1 },
                                             { view =
                                                 (Arrow ((),
                                                         { view =
                                                             (Constr (int,[]));
                                                           generation = 1 },
                                                         { view =
                                                             (Constr (bool,[]));
                                                           generation = 1 }));
                                               generation = 1 }));
                                   generation = 1 };
                               generation = 0;
                               sub_anchor = None };
                         l =
                           [{ view =
                                (Var
                                  { id = x/116441;
                                    ty =
                                    { view = (Constr (int,[]));
                                      generation = 1 }
                                    });
                              ty = { view = (Constr (int,[]));
                                     generation = 1 };
                              generation = 0;
                              sub_anchor = None };
                            { view = (Const 0);
                              ty = { view = (Constr (int,[]));
                                     generation = 1 };
                              generation = 0;
                              sub_anchor = None }]
                         };
                ty = { view = (Constr (bool,[]));
                       generation = 1 };
                generation = 0;
                sub_anchor = None }];
           invariant =
             { view =
                 Apply {f = { view =
                                (Sym
                                  (+ : { view =
                                           (Arrow ((),
                                                   { view = (Constr (int,[]));
                                                     generation = 1 },
                                                   { view =
                                                       (Arrow ((),
                                                               { view =
                                                                   (Constr
                                                                     (int,[]));
                                                                 generation = 1 },
                                                               { view =
                                                                   (Constr
                                                                     (int,[]));
                                                                 generation = 1 }));
                                                     generation = 1 }));
                                         generation = 1 }));
                              ty =
                                { view =
                                    (Arrow ((),
                                            { view = (Constr (int,[]));
                                              generation = 1 },
                                            { view =
                                                (Arrow ((),
                                                        { view =
                                                            (Constr (int,[]));
                                                          generation = 1 },
                                                        { view =
                                                            (Constr (int,[]));
                                                          generation = 1 }));
                                              generation = 1 }));
                                  generation = 1 };
                              generation = 0;
                              sub_anchor = None };
                        l =
                          [{ view = (Const 1);
                             ty = { view = (Constr (int,[]));
                                    generation = 1 };
                             generation = 0;
                             sub_anchor = None };
                           { view =
                               (Var
                                 { id = x/116441;
                                   ty =
                                   { view = (Constr (int,[]));
                                     generation = 1 }
                                   });
                             ty = { view = (Constr (int,[]));
                                    generation = 1 };
                             generation = 0;
                             sub_anchor = None }]
                        };
               ty = { view = (Constr (int,[]));
                      generation = 1 };
               generation = 0;
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
                                generation = 1 };
                         generation = 0;
                         sub_anchor = None }};
              "model_eval":
                Term
                  { view = (Const 1);
                    ty = { view = (Constr (int,[]));
                           generation = 1 };
                    generation = 0;
                    sub_anchor = None };
              "id": String "7c9c52f3-519f-47a5-934c-d7dede67083d"];
           status =
             Feasible
               { tys = [];
                 consts =
                 [((x/116441 : { view = (Constr (int,[]));
                                 generation = 1 }),
                   { view = (Const 0);
                     ty = { view = (Constr (int,[]));
                            generation = 1 };
                     generation = 0;
                     sub_anchor = None })
                   ];
                 funs = []; representable = true; completed = false;
                 ty_subst = [] }
           }]
      }
    Parsed AST:
    (Ast_types.FunctionDef
       { Ast_types.name = "test_1";
         args =
         { Ast_types.posonlyargs = []; args = []; vararg = None; kwonlyargs = [];
           kw_defaults = []; kwarg = None; defaults = [] };
         body =
         [Ast_types.ExprStmt {
            value =
            (Ast_types.Constant
               { Ast_types.value =
                 (Ast_types.String
                    "test_1\n\n- invariant: x + 2\n- constraints:\n    - x >= 1\n");
                 kind = None })};
           (Ast_types.AnnAssign
              { Ast_types.target =
                (Ast_types.Name { Ast_types.id = "result"; ctx = Ast_types.Load });
                annotation =
                (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
                value =
                (Some (Ast_types.Call
                         { Ast_types.func =
                           (Ast_types.Name
                              { Ast_types.id = "f"; ctx = Ast_types.Load });
                           args = [];
                           keywords =
                           [{ Ast_types.arg = (Some "x");
                              value =
                              (Ast_types.Constant
                                 { Ast_types.value = (Ast_types.Int 1);
                                   kind = None })
                              }
                             ]
                           }));
                simple = 1 });
           (Ast_types.AnnAssign
              { Ast_types.target =
                (Ast_types.Name
                   { Ast_types.id = "expected"; ctx = Ast_types.Load });
                annotation =
                (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
                value =
                (Some (Ast_types.Constant
                         { Ast_types.value = (Ast_types.Int 3); kind = None }));
                simple = 1 });
           Ast_types.Assert {
             test =
             Ast_types.Compare {
               left =
               (Ast_types.Name { Ast_types.id = "result"; ctx = Ast_types.Load });
               ops = [Ast_types.Eq];
               comparators =
               [(Ast_types.Name
                   { Ast_types.id = "expected"; ctx = Ast_types.Load })
                 ]};
             msg = None}
           ];
         decorator_list = []; returns = None; type_comment = None;
         type_params = [] })
    (Ast_types.FunctionDef
       { Ast_types.name = "test_2";
         args =
         { Ast_types.posonlyargs = []; args = []; vararg = None; kwonlyargs = [];
           kw_defaults = []; kwarg = None; defaults = [] };
         body =
         [Ast_types.ExprStmt {
            value =
            (Ast_types.Constant
               { Ast_types.value =
                 (Ast_types.String
                    "test_2\n\n- invariant: 1 + x\n- constraints:\n    - x <= 0\n");
                 kind = None })};
           (Ast_types.AnnAssign
              { Ast_types.target =
                (Ast_types.Name { Ast_types.id = "result"; ctx = Ast_types.Load });
                annotation =
                (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
                value =
                (Some (Ast_types.Call
                         { Ast_types.func =
                           (Ast_types.Name
                              { Ast_types.id = "f"; ctx = Ast_types.Load });
                           args = [];
                           keywords =
                           [{ Ast_types.arg = (Some "x");
                              value =
                              (Ast_types.Constant
                                 { Ast_types.value = (Ast_types.Int 0);
                                   kind = None })
                              }
                             ]
                           }));
                simple = 1 });
           (Ast_types.AnnAssign
              { Ast_types.target =
                (Ast_types.Name
                   { Ast_types.id = "expected"; ctx = Ast_types.Load });
                annotation =
                (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
                value =
                (Some (Ast_types.Constant
                         { Ast_types.value = (Ast_types.Int 1); kind = None }));
                simple = 1 });
           Ast_types.Assert {
             test =
             Ast_types.Compare {
               left =
               (Ast_types.Name { Ast_types.id = "result"; ctx = Ast_types.Load });
               ops = [Ast_types.Eq];
               comparators =
               [(Ast_types.Name
                   { Ast_types.id = "expected"; ctx = Ast_types.Load })
                 ]};
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
      "../test/data/model/polymorphic/nested_poly.yaml"
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
  let type_annot, expr =
    match parse_term term with
    | Ok (type_annot, expr) -> (type_annot, expr)
    | Error msg -> failwith msg
  in

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

  [%expect {|
    name: nested_poly
    code: type ('a, 'b) container2 = { v : ('a * 'b) }

    type ('a, 'b, 'c) container3 = { v : ('a * 'b * 'c) }

    let v : ((int, int, (int, int) container2) container3) = { v = (1, 2, ({ v = (3, 4) } : (int, int) container2)) }

    let v = fun w -> if w = v then true else false

    <><><><><><><><><><>

    Applied symbol:
    (w/93078 : { view = (Constr (container3/GNcgqNwFwYW5tzhyaY6CeGAIPd_yt-388M9oBK-oCu0, [{ view = (Constr (int, [])); generation = 3 }; { view = (Constr (int, [])); generation = 3 }; { view = (Constr (container2/-DqmX3SSi5K8O3dHQmWmCpQMR7Z0rThdokr2J_d8SsM, [{ view = (Constr (int, [])); generation = 3 }; { view = (Constr (int, [])); generation = 3 }])); generation = 3 }])); generation = 3 })

    <><><><><><><><><><>

    Term:
    { view =
        Record
          {
          rows =
            [(
               (v/ntugG7n5lh6HDSFygAl3k5zU06F3KZPD_Ya-6G5al2E : { view =
                                                                    (Arrow ((),
                                                                            { view =
                                                                                (Constr
                                                                                  (container3/GNcgqNwFwYW5tzhyaY6CeGAIPd_yt-388M9oBK-oCu0,
                                                                                   [{ view = (Constr (int,[]));
                                                                                      generation = 3 };
                                                                                    { view = (Constr (int,[]));
                                                                                      generation = 3 };
                                                                                    { view = (Constr (container2/-DqmX3SSi5K8O3dHQmWmCpQMR7Z0rThdokr2J_d8SsM,[{ view = (Constr (int,[]));
                                                                                                                                                                generation = 3 };
                                                                                                                                                              { view = (Constr (int,[]));
                                                                                                                                                                generation = 3 }]));
                                                                                      generation = 3 }]));
                                                                              generation = 3 },
                                                                            { view = (Tuple [{ view = (Constr (int,[]));
                                                                                               generation = 3 }; { view = (Constr (int,[]));
                                                                                                                   generation = 3 }; { view = (Constr (container2/-DqmX3SSi5K8O3dHQmWmCpQMR7Z0rThdokr2J_d8SsM,[{ view = (Constr (int,[]));
                                                                                                                                                                                                                 generation = 3 };
                                                                                                                                                                                                               { view = (Constr (int,[]));
                                                                                                                                                                                                                 generation = 3 }]));
                                                                                                                                       generation = 3 }]);
                                                                              generation = 3 }));
                                                                  generation = 3 }),
               { view =
                   Tuple
                     {
                     l =
                       [{ view = (Const 1);
                          ty = { view = (Constr (int,[]));
                                 generation = 3 };
                          generation = 1; sub_anchor = None };
                         { view = (Const 2);
                           ty = { view = (Constr (int,[]));
                                  generation = 3 };
                           generation = 1; sub_anchor = None };
                         { view =
                             Record
                               {
                               rows =
                                 [(
                                    (v/qy3HK0-_EulKK7NKYEdLqhsx6_nCFYnpclHGLlIf9J8 : { view =
                                                                                         (Arrow ((),
                                                                                                 { view = (Constr (container2/-DqmX3SSi5K8O3dHQmWmCpQMR7Z0rThdokr2J_d8SsM,[{ view = (Constr (int,[]));
                                                                                                                                                                             generation = 3 };
                                                                                                                                                                           { view = (Constr (int,[]));
                                                                                                                                                                             generation = 3 }]));
                                                                                                   generation = 3 },
                                                                                                 { view = (Tuple [{ view = (Constr (int,[]));
                                                                                                                    generation = 3 }; { view = (Constr (int,[]));
                                                                                                                                        generation = 3 }]);
                                                                                                   generation = 3 }));
                                                                                       generation = 3 }),
                                    { view = Tuple {l = [{ view = (Const 3);
                                                           ty = { view = (Constr (int,[]));
                                                                  generation = 3 };
                                                           generation = 1; sub_anchor = None }; { view = (Const 4);
                                                                                                  ty = { view = (Constr (int,[]));
                                                                                                         generation = 3 };
                                                                                                  generation = 1; sub_anchor = None }]};
                                      ty = { view = (Tuple [{ view = (Constr (int,[]));
                                                              generation = 3 }; { view = (Constr (int,[]));
                                                                                  generation = 3 }]);
                                             generation = 3 };
                                      generation = 1;
                                      sub_anchor = None }
                                    )];
                               rest = None
                               };
                           ty = { view = (Constr (container2/-DqmX3SSi5K8O3dHQmWmCpQMR7Z0rThdokr2J_d8SsM,[{ view = (Constr (int,[]));
                                                                                                            generation = 3 };
                                                                                                          { view = (Constr (int,[]));
                                                                                                            generation = 3 }]));
                                  generation = 3 };
                           generation = 1;
                           sub_anchor = None }]
                     };
                 ty = { view = (Tuple [{ view = (Constr (int,[]));
                                         generation = 3 }; { view = (Constr (int,[]));
                                                             generation = 3 }; { view = (Constr (container2/-DqmX3SSi5K8O3dHQmWmCpQMR7Z0rThdokr2J_d8SsM,[{ view = (Constr (int,[]));
                                                                                                                                                           generation = 3 };
                                                                                                                                                         { view = (Constr (int,[]));
                                                                                                                                                           generation = 3 }]));
                                                                                 generation = 3 }]);
                        generation = 3 };
                 generation = 1;
                 sub_anchor = None }
               )];
          rest = None
          };
      ty = { view = (Constr (container3/GNcgqNwFwYW5tzhyaY6CeGAIPd_yt-388M9oBK-oCu0,[{ view = (Constr (int,[]));
                                                                                       generation = 3 };
                                                                                     { view = (Constr (int,[]));
                                                                                       generation = 3 };
                                                                                     { view = (Constr (container2/-DqmX3SSi5K8O3dHQmWmCpQMR7Z0rThdokr2J_d8SsM,[{ view = (Constr (int,[]));
                                                                                                                                                                 generation = 3 };
                                                                                                                                                               { view = (Constr (int,[]));
                                                                                                                                                                 generation = 3 }]));
                                                                                       generation = 3 }]));
             generation = 3 };
      generation = 1;
      sub_anchor = None }

    <><><><><><><><><><>

    Parsing term:

    Type annot: (Ast_types.Subscript
       { Ast_types.value =
         (Ast_types.Name { Ast_types.id = "container3"; ctx = Ast_types.Load });
         slice =
         (Ast_types.Tuple
            { Ast_types.elts =
              [(Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
                (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
                (Ast_types.Subscript
                   { Ast_types.value =
                     (Ast_types.Name
                        { Ast_types.id = "container2"; ctx = Ast_types.Load });
                     slice =
                     (Ast_types.Tuple
                        { Ast_types.elts =
                          [(Ast_types.Name
                              { Ast_types.id = "int"; ctx = Ast_types.Load });
                            (Ast_types.Name
                               { Ast_types.id = "int"; ctx = Ast_types.Load })
                            ];
                          ctx = Ast_types.Load; dims = [] });
                     ctx = Ast_types.Load })
                ];
              ctx = Ast_types.Load; dims = [] });
         ctx = Ast_types.Load })

    Expr:
    (Ast_types.Call
       { Ast_types.func =
         (Ast_types.Name { Ast_types.id = "container3"; ctx = Ast_types.Load });
         args =
         [(Ast_types.Tuple
             { Ast_types.elts =
               [(Ast_types.Constant
                   { Ast_types.value = (Ast_types.Int 1); kind = None });
                 (Ast_types.Constant
                    { Ast_types.value = (Ast_types.Int 2); kind = None });
                 (Ast_types.Call
                    { Ast_types.func =
                      (Ast_types.Name
                         { Ast_types.id = "container2"; ctx = Ast_types.Load });
                      args =
                      [(Ast_types.Tuple
                          { Ast_types.elts =
                            [(Ast_types.Constant
                                { Ast_types.value = (Ast_types.Int 3);
                                  kind = None });
                              (Ast_types.Constant
                                 { Ast_types.value = (Ast_types.Int 4);
                                   kind = None })
                              ];
                            ctx = Ast_types.Load; dims = [] })
                        ];
                      keywords = [] })
                 ];
               ctx = Ast_types.Load; dims = [] })
           ];
         keywords = [] })

    <><><><><><><><><><>

    Json:

    [
      [
        "AnnAssign",
        {
          "target": [ "Name", { "id": "w", "ctx": [ "Load" ] } ],
          "annotation": [
            "Subscript",
            {
              "value": [ "Name", { "id": "container3", "ctx": [ "Load" ] } ],
              "slice": [
                "Tuple",
                {
                  "elts": [
                    [ "Name", { "id": "int", "ctx": [ "Load" ] } ],
                    [ "Name", { "id": "int", "ctx": [ "Load" ] } ],
                    [
                      "Subscript",
                      {
                        "value": [
                          "Name", { "id": "container2", "ctx": [ "Load" ] }
                        ],
                        "slice": [
                          "Tuple",
                          {
                            "elts": [
                              [ "Name", { "id": "int", "ctx": [ "Load" ] } ],
                              [ "Name", { "id": "int", "ctx": [ "Load" ] } ]
                            ],
                            "ctx": [ "Load" ],
                            "dims": []
                          }
                        ],
                        "ctx": [ "Load" ]
                      }
                    ]
                  ],
                  "ctx": [ "Load" ],
                  "dims": []
                }
              ],
              "ctx": [ "Load" ]
            }
          ],
          "value": [
            "Call",
            {
              "func": [ "Name", { "id": "container3", "ctx": [ "Load" ] } ],
              "args": [
                [
                  "Tuple",
                  {
                    "elts": [
                      [ "Constant", { "value": [ "Int", 1 ], "kind": null } ],
                      [ "Constant", { "value": [ "Int", 2 ], "kind": null } ],
                      [
                        "Call",
                        {
                          "func": [
                            "Name", { "id": "container2", "ctx": [ "Load" ] }
                          ],
                          "args": [
                            [
                              "Tuple",
                              {
                                "elts": [
                                  [
                                    "Constant",
                                    { "value": [ "Int", 3 ], "kind": null }
                                  ],
                                  [
                                    "Constant",
                                    { "value": [ "Int", 4 ], "kind": null }
                                  ]
                                ],
                                "ctx": [ "Load" ],
                                "dims": []
                              }
                            ]
                          ],
                          "keywords": []
                        }
                      ]
                    ],
                    "ctx": [ "Load" ],
                    "dims": []
                  }
                ]
              ],
              "keywords": []
            }
          ],
          "simple": 1
        }
      ]
    ]
    |}]

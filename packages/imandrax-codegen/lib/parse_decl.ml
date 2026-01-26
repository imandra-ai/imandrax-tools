open Printf
open Parse_common

let wip s = failwith (sprintf "WIP: %s" s)
(**
Parse one row of a Record type declaration

Return a tuple of
- field name :: string
- field type names :: string list (chain of nested types)

Example:
- (x, int) will be mapped to
```python
@dataclass
class SomeClass:
    ...
    x: int
    ...
```
*)
let parse_rec_row_to_dataclass_row (rec_row : (Uid.t, Type.t) Ty_view.rec_row) :
    string * type_expr =
  let Ty_view.{ f : Uid.t; ty : Type.t; doc = _ } = rec_row in

  let arg_name = f.name in
  let Type.{ view = arg_ty_view; generation = _ } = ty in

  let arg_type_names, _params = parse_constr_to_type_annot arg_ty_view in

  (arg_name, arg_type_names)

(** Parse one row of an Algebraic type declaration

    Return:
    - dataclass definition statement :: Ast.stmt
    - type annotation of the dataclass :: Ast.expr
        - for non-generic types, this is just the dataclass name
        - for generic types, this is a subscript
        - this will be used in the final union definition

    - Handle the Algebraic case of Delc.t where payload is a list of adt_row
    - Each adt_row should maps to a dataclass definition in Python (a list of
      statements)

    Mapping
    - Each adt_row contains
    - constructor id (name) :: Uid.t
    - labels :: Uid.t list option: a list of labels for inline records
    - when this exists, we should generate a dataclass with non-anonymous fields
    - args: Type.t list
    - the type annotation for a row
    - doc :: string option

    In Python
    - Each row of dataclass is a type annotation, `arg0: int` or `a: int`

    Example:
    ```iml
    type 'a shape_poly =
    | Point
    | Circle of 'a
    | Square of int
    ```

    gets parsed to

    ```python
    a = TypeVar('a')
    @dataclass
    class Point:
        pass
    @dataclass
    class Circle[Generic[a]]:
        arg0: a
    @dataclass
    class Square:
        arg0: int
    shape_poly: TypeAlias = Point | Circle[a] | Square
    ```
    This function handles the adt rows (the dataclasses definition in Python)
    *)
let parse_adt_row_to_dataclass_def (adt_row : (Uid.t, Type.t) Ty_view.adt_row) :
    Ast.stmt * Ast.expr =
  let Ty_view.{ c; labels; args; doc = _ } = adt_row in
  let class_name = c.name in

  (* labels -> dataclass field names *)
  let (field_names : string list) =
    match labels with
    | None -> Ast.anonymous_arg_names (List.length args)
    | Some id_list -> List.map (fun (id : Uid.t) -> id.name) id_list
  in

  (* constructor names, i.e., the type annotation for each field
    Eg: a, int
    type params used
    Eg: a
  *)
  let ( (constr_names_by_field : type_expr list),
        (type_params_used : Uid.t list) ) =
    (args : Mir.Type.t list)
    |> List.map (fun (arg : Type.t) ->
           let Type.{ view = arg_ty_view; generation = _ } = arg in

           let types, params = parse_constr_to_type_annot arg_ty_view in

           (types, params))
    |> List.split
    |> fun (dc_arg_constr_names, dc_arg_type_params_by_arg) ->
    (* Flatten and dedup type params *)
    let (dc_arg_type_params : Uid.t list) =
      dc_arg_type_params_by_arg |> List.flatten
      |> CCList.sort_uniq ~cmp:Uid.compare
    in
    (dc_arg_constr_names, dc_arg_type_params)
  in

  (* Ast statement for defining the dataclass *)
  let ast_stmt_dataclass_def =
    Ast.mk_dataclass_def class_name
      (type_params_used |> List.map (fun (uid : Uid.t) -> uid.name))
      (List.combine field_names constr_names_by_field)
  in

  (* Ast expression for the type annotation of the dataclass
  Eg: Point, Circle[a], Square
  *)
  let ast_expr_dataclass_annot =
    Ast.mk_generic_type_annot class_name (type_params_used |> List.map (fun (uid : Uid.t) -> uid.name))
  in

  (ast_stmt_dataclass_def, ast_expr_dataclass_annot)

(** NEW: Parse declaration to Semantic IR *)
let parse_decl_to_semantic (decl : (Term.t, Type.t) Decl.t_poly) :
    (Semantic_ir.Types.type_decl, string) result =
  match decl with
  | Ty (ty_view_def : Type.t Ty_view.def_poly) ->
      let {
        Ty_view.name = decl_name_uid;
        params : Uid.t list;
        decl = ty_view_decl;
        clique = _;
        timeout = _;
      } =
        ty_view_def
      in

      let { Uid.name = decl_name; view = _ } = decl_name_uid in
      let type_params = List.map (fun (uid : Uid.t) -> uid.name) params in

      (match ty_view_decl with
       | Algebraic (adt_rows : (Uid.t, Type.t) Ty_view.adt_row list) ->
           let constructors = List.map (fun (adt_row : (Uid.t, Type.t) Ty_view.adt_row) ->
             let Ty_view.{ c; labels; args; doc = _ } = adt_row in
             let constructor_name = c.name in

             let field_names = match labels with
               | None -> Ast.anonymous_arg_names (List.length args)
               | Some id_list -> List.map (fun (id : Uid.t) -> id.name) id_list
             in

             let field_types = List.map (fun (arg : Type.t) ->
               let Type.{ view = arg_ty_view; generation = _ } = arg in
               let ty_expr, _params = parse_constr_to_sir_type_expr arg_ty_view in
               ty_expr
             ) args in

             let fields = List.map2 (fun name ty ->
               match labels with
               | None -> Semantic_ir.Types.Positional ty
               | Some _ -> Semantic_ir.Types.Named (name, ty)
             ) field_names field_types in

             { Semantic_ir.Types.vc_name = constructor_name;
               vc_fields = fields }
           ) adt_rows in

           Ok (Semantic_ir.Types.Variant {
             name = decl_name;
             type_params;
             constructors;
           })

       | Record (rec_rows : (Uid.t, Type.t) Ty_view.rec_row list) ->
           let fields = List.map (fun (rec_row : (Uid.t, Type.t) Ty_view.rec_row) ->
             let Ty_view.{ f : Uid.t; ty : Type.t; doc = _ } = rec_row in
             let field_name = f.name in
             let Type.{ view = arg_ty_view; generation = _ } = ty in
             let ty_expr, _params = parse_constr_to_sir_type_expr arg_ty_view in

             { Semantic_ir.Types.rf_name = field_name;
               rf_type = ty_expr }
           ) rec_rows in

           Ok (Semantic_ir.Types.Record {
             name = decl_name;
             type_params;
             fields;
           })

       | _ -> Error "WIP: not Algebraic and not Record")
  | Fun _ -> Error "WIP: Fun"
  | _ -> Error "parse_decl_to_semantic: expected Ty | Fun"

(** OLD: Parse declaration to Python AST *)
let parse_decl (decl : (Term.t, Type.t) Decl.t_poly) :
    (Ast.stmt list, string) result =
  match decl with
  | Ty (ty_view_def : Type.t Ty_view.def_poly) ->
      (* Unpack ty view *)
      let {
        Ty_view.name = decl_name_uid;
        params : Uid.t list;
        decl = ty_view_decl;
        clique = _;
        timeout = _;
      } =
        ty_view_def
      in

      let (type_var_defs : Ast.stmt list) =
        params |> List.map type_var_def_of_uid
      in

      (* Root name of the decl *)
      let { Uid.name = decl_name; view = _ } = decl_name_uid in

      (* TODO(refactor): move above extraction to single pattern match *)

      (* Handle rows *)
      let (decl_body : Ast_types.stmt list) =
        match ty_view_decl with
        | Algebraic (adt_rows : (Uid.t, Type.t) Ty_view.adt_row list) ->
            let (dc_defs : Ast.stmt list), (dc_annot : Ast.expr list) =
              adt_rows |> List.map parse_adt_row_to_dataclass_def |> List.split
            in
            let union_def = Ast.mk_union_def decl_name dc_annot in
            let dc_and_union_defs = dc_defs @ [ union_def ] in
            dc_and_union_defs
        | Record (rec_rows : (Uid.t, Type.t) Ty_view.rec_row list) ->
            let dc_args : (string * type_expr) list =
              rec_rows |> List.map parse_rec_row_to_dataclass_row
            in
            let dc_def = Ast.mk_dataclass_def decl_name [] dc_args in
            [ dc_def ]
        | _ -> failwith "WIP: not Algebraic and not Tuple"
      in
      Ok (type_var_defs @ decl_body)
  | Fun _ -> failwith "WIP: Fun"
  | _ -> invalid_arg "parse_decl: expected Ty | Fun"

(* Expect tests
==================== *)

let sep : string = "\n" ^ CCString.repeat "<>" 10 ^ "\n"

(* Decl
-------------------- *)

let%expect_test "parse decl art" =
  let file_name = "variant_poly_two_var" in
  (* let file_name = "nested_generics" in *)
  (* let file_name = "real_and_option" in *)
  let yaml_str =
    CCIO.File.read_exn (sprintf "../test/data/decl/%s.yaml" file_name)
  in
  (* let yaml_str = CCIO.File.read_exn "../test/data/decl/variant_two.yaml" in *)
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

  let (decls : Decl.t list) = arts |> List.map Art_utils.yaml_to_decl in

  (* Print source
  -------------------- *)
  printf "name: %s\n" name;
  printf "code:\n %s\n" code;
  printf "<><><><><><><><><>\n";

  (* Print decls
  -------------------- *)
  let fmt = Format.str_formatter in
  List.iter
    (fun decl -> Format.fprintf fmt "%a@?" Pretty_print.pp_decl decl)
    decls;
  print_endline (Format.flush_str_formatter ());

  [%expect
    {|
    name: variant_poly_two_var
    code:
     type ('a, 'b) container =
      | Empty
      | Single of 'a
      | Pair of 'a * 'b
      | Labeled of { key: 'a; value: 'b }
      | Multi of 'a list * 'b list

    <><><><><><><><><>
    Ty
      {
      name = container/oxiYljEm9mLVWkkYURTWoWGNfupScANrN7SpMxUoULo;
      params = [a/176702; b/176703];
      decl =
        Algebraic
          [{
             c = Empty/zaoimvUmhw4ew6X_Jt2YwcRT3Xmk2fdS0-c_jFJhJxQ;
             labels = None;
             args = [];
             doc = None
             };
           {
             c = Single/VX6Zwtm7dZgJKoLgh9TNOahjhFhEv3ERVolUa0HF8OQ;
             labels = None;
             args = [{ view = (Var a/176702);
                       generation = 1 }];
             doc = None
             };
           {
             c = Pair/B4x1x4rxXHCSWnsOlIPrDcyh6TbWcJB8Gg_-cZtmoF4;
             labels = None;
             args =
               [{ view = (Var a/176702);
                  generation = 1 };
                { view = (Var b/176703);
                  generation = 1 }];
             doc = None
             };
           {
             c = Labeled/iWpeHJGeoERz6FurcG-2PRVcIVyovCyN0ApH6xnsh3o;
             labels =
               (Some
                 [key/cwwWl6JtTLFzaHQ1pe7U_iVJJf6NEw6GdOArOnc1ebo;
                  value/FRnzhPcUFqym3uZnzXwwtdzgXuV3vrAi_1qaiZ2Te54]);
             args =
               [{ view = (Var a/176702);
                  generation = 1 };
                { view = (Var b/176703);
                  generation = 1 }];
             doc = None
             };
           {
             c = Multi/pHB4Isq3zq0g3pg2KaAB7IAKGm336YEoe1ws7w1MzD8;
             labels = None;
             args =
               [{ view =
                    (Constr (list,[{ view = (Var a/176702);
                                     generation = 1 }]));
                  generation = 1 };
                { view =
                    (Constr (list,[{ view = (Var b/176703);
                                     generation = 1 }]));
                  generation = 1 }];
             doc = None
             }];
      clique = None;
      timeout = None
      }
    |}];

  (* Print parsed
  -------------------- *)
  let decl =
    match decls with [ decl ] -> decl | _ -> failwith "Expected one decl"
  in
  let parsed = decl |> parse_decl |> unwrap in

  printf "<><><><><><><><><>\n";

  List.iter (fun stmt -> print_endline (Ast.show_stmt stmt)) parsed;

  printf "<><><><><><><><><>\n";
  [%expect
    {|
    <><><><><><><><><>
    (Ast_types.Assign
       { Ast_types.targets =
         [(Ast_types.Name { Ast_types.id = "a"; ctx = Ast_types.Load })];
         value =
         (Ast_types.Call
            { Ast_types.func =
              (Ast_types.Name { Ast_types.id = "TypeVar"; ctx = Ast_types.Load });
              args =
              [(Ast_types.Constant
                  { Ast_types.value = (Ast_types.String "a"); kind = None })
                ];
              keywords = [] });
         type_comment = None })
    (Ast_types.Assign
       { Ast_types.targets =
         [(Ast_types.Name { Ast_types.id = "b"; ctx = Ast_types.Load })];
         value =
         (Ast_types.Call
            { Ast_types.func =
              (Ast_types.Name { Ast_types.id = "TypeVar"; ctx = Ast_types.Load });
              args =
              [(Ast_types.Constant
                  { Ast_types.value = (Ast_types.String "b"); kind = None })
                ];
              keywords = [] });
         type_comment = None })
    (Ast_types.ClassDef
       { Ast_types.name = "Empty"; bases = []; keywords = [];
         body = [Ast_types.Pass];
         decorator_list =
         [(Ast_types.Name { Ast_types.id = "dataclass"; ctx = Ast_types.Load })]
         })
    (Ast_types.ClassDef
       { Ast_types.name = "Single";
         bases =
         [(Ast_types.Subscript
             { Ast_types.value =
               (Ast_types.Name { Ast_types.id = "Generic"; ctx = Ast_types.Load });
               slice =
               (Ast_types.Name { Ast_types.id = "a"; ctx = Ast_types.Load });
               ctx = Ast_types.Load })
           ];
         keywords = [];
         body =
         [(Ast_types.AnnAssign
             { Ast_types.target =
               (Ast_types.Name { Ast_types.id = "arg0"; ctx = Ast_types.Load });
               annotation =
               (Ast_types.Name { Ast_types.id = "a"; ctx = Ast_types.Load });
               value = None; simple = 1 })
           ];
         decorator_list =
         [(Ast_types.Name { Ast_types.id = "dataclass"; ctx = Ast_types.Load })]
         })
    (Ast_types.ClassDef
       { Ast_types.name = "Pair";
         bases =
         [(Ast_types.Subscript
             { Ast_types.value =
               (Ast_types.Name { Ast_types.id = "Generic"; ctx = Ast_types.Load });
               slice =
               (Ast_types.Tuple
                  { Ast_types.elts =
                    [(Ast_types.Name { Ast_types.id = "a"; ctx = Ast_types.Load });
                      (Ast_types.Name
                         { Ast_types.id = "b"; ctx = Ast_types.Load })
                      ];
                    ctx = Ast_types.Load; dims = [] });
               ctx = Ast_types.Load })
           ];
         keywords = [];
         body =
         [(Ast_types.AnnAssign
             { Ast_types.target =
               (Ast_types.Name { Ast_types.id = "arg0"; ctx = Ast_types.Load });
               annotation =
               (Ast_types.Name { Ast_types.id = "a"; ctx = Ast_types.Load });
               value = None; simple = 1 });
           (Ast_types.AnnAssign
              { Ast_types.target =
                (Ast_types.Name { Ast_types.id = "arg1"; ctx = Ast_types.Load });
                annotation =
                (Ast_types.Name { Ast_types.id = "b"; ctx = Ast_types.Load });
                value = None; simple = 1 })
           ];
         decorator_list =
         [(Ast_types.Name { Ast_types.id = "dataclass"; ctx = Ast_types.Load })]
         })
    (Ast_types.ClassDef
       { Ast_types.name = "Labeled";
         bases =
         [(Ast_types.Subscript
             { Ast_types.value =
               (Ast_types.Name { Ast_types.id = "Generic"; ctx = Ast_types.Load });
               slice =
               (Ast_types.Tuple
                  { Ast_types.elts =
                    [(Ast_types.Name { Ast_types.id = "a"; ctx = Ast_types.Load });
                      (Ast_types.Name
                         { Ast_types.id = "b"; ctx = Ast_types.Load })
                      ];
                    ctx = Ast_types.Load; dims = [] });
               ctx = Ast_types.Load })
           ];
         keywords = [];
         body =
         [(Ast_types.AnnAssign
             { Ast_types.target =
               (Ast_types.Name { Ast_types.id = "key"; ctx = Ast_types.Load });
               annotation =
               (Ast_types.Name { Ast_types.id = "a"; ctx = Ast_types.Load });
               value = None; simple = 1 });
           (Ast_types.AnnAssign
              { Ast_types.target =
                (Ast_types.Name { Ast_types.id = "value"; ctx = Ast_types.Load });
                annotation =
                (Ast_types.Name { Ast_types.id = "b"; ctx = Ast_types.Load });
                value = None; simple = 1 })
           ];
         decorator_list =
         [(Ast_types.Name { Ast_types.id = "dataclass"; ctx = Ast_types.Load })]
         })
    (Ast_types.ClassDef
       { Ast_types.name = "Multi";
         bases =
         [(Ast_types.Subscript
             { Ast_types.value =
               (Ast_types.Name { Ast_types.id = "Generic"; ctx = Ast_types.Load });
               slice =
               (Ast_types.Tuple
                  { Ast_types.elts =
                    [(Ast_types.Name { Ast_types.id = "a"; ctx = Ast_types.Load });
                      (Ast_types.Name
                         { Ast_types.id = "b"; ctx = Ast_types.Load })
                      ];
                    ctx = Ast_types.Load; dims = [] });
               ctx = Ast_types.Load })
           ];
         keywords = [];
         body =
         [(Ast_types.AnnAssign
             { Ast_types.target =
               (Ast_types.Name { Ast_types.id = "arg0"; ctx = Ast_types.Load });
               annotation =
               (Ast_types.Subscript
                  { Ast_types.value =
                    (Ast_types.Name
                       { Ast_types.id = "list"; ctx = Ast_types.Load });
                    slice =
                    (Ast_types.Tuple
                       { Ast_types.elts =
                         [(Ast_types.Name
                             { Ast_types.id = "a"; ctx = Ast_types.Load })
                           ];
                         ctx = Ast_types.Load; dims = [] });
                    ctx = Ast_types.Load });
               value = None; simple = 1 });
           (Ast_types.AnnAssign
              { Ast_types.target =
                (Ast_types.Name { Ast_types.id = "arg1"; ctx = Ast_types.Load });
                annotation =
                (Ast_types.Subscript
                   { Ast_types.value =
                     (Ast_types.Name
                        { Ast_types.id = "list"; ctx = Ast_types.Load });
                     slice =
                     (Ast_types.Tuple
                        { Ast_types.elts =
                          [(Ast_types.Name
                              { Ast_types.id = "b"; ctx = Ast_types.Load })
                            ];
                          ctx = Ast_types.Load; dims = [] });
                     ctx = Ast_types.Load });
                value = None; simple = 1 })
           ];
         decorator_list =
         [(Ast_types.Name { Ast_types.id = "dataclass"; ctx = Ast_types.Load })]
         })
    (Ast_types.Assign
       { Ast_types.targets =
         [(Ast_types.Name { Ast_types.id = "container"; ctx = Ast_types.Load })];
         value =
         (Ast_types.BinOp
            { Ast_types.left =
              (Ast_types.BinOp
                 { Ast_types.left =
                   (Ast_types.BinOp
                      { Ast_types.left =
                        (Ast_types.BinOp
                           { Ast_types.left =
                             (Ast_types.Name
                                { Ast_types.id = "Empty"; ctx = Ast_types.Load });
                             op = Ast_types.BitOr;
                             right =
                             (Ast_types.Subscript
                                { Ast_types.value =
                                  (Ast_types.Name
                                     { Ast_types.id = "Single";
                                       ctx = Ast_types.Load });
                                  slice =
                                  (Ast_types.Name
                                     { Ast_types.id = "a"; ctx = Ast_types.Load });
                                  ctx = Ast_types.Load })
                             });
                        op = Ast_types.BitOr;
                        right =
                        (Ast_types.Subscript
                           { Ast_types.value =
                             (Ast_types.Name
                                { Ast_types.id = "Pair"; ctx = Ast_types.Load });
                             slice =
                             (Ast_types.Tuple
                                { Ast_types.elts =
                                  [(Ast_types.Name
                                      { Ast_types.id = "a"; ctx = Ast_types.Load
                                        });
                                    (Ast_types.Name
                                       { Ast_types.id = "b"; ctx = Ast_types.Load
                                         })
                                    ];
                                  ctx = Ast_types.Load; dims = [] });
                             ctx = Ast_types.Load })
                        });
                   op = Ast_types.BitOr;
                   right =
                   (Ast_types.Subscript
                      { Ast_types.value =
                        (Ast_types.Name
                           { Ast_types.id = "Labeled"; ctx = Ast_types.Load });
                        slice =
                        (Ast_types.Tuple
                           { Ast_types.elts =
                             [(Ast_types.Name
                                 { Ast_types.id = "a"; ctx = Ast_types.Load });
                               (Ast_types.Name
                                  { Ast_types.id = "b"; ctx = Ast_types.Load })
                               ];
                             ctx = Ast_types.Load; dims = [] });
                        ctx = Ast_types.Load })
                   });
              op = Ast_types.BitOr;
              right =
              (Ast_types.Subscript
                 { Ast_types.value =
                   (Ast_types.Name
                      { Ast_types.id = "Multi"; ctx = Ast_types.Load });
                   slice =
                   (Ast_types.Tuple
                      { Ast_types.elts =
                        [(Ast_types.Name
                            { Ast_types.id = "a"; ctx = Ast_types.Load });
                          (Ast_types.Name
                             { Ast_types.id = "b"; ctx = Ast_types.Load })
                          ];
                        ctx = Ast_types.Load; dims = [] });
                   ctx = Ast_types.Load })
              });
         type_comment = None })
    <><><><><><><><><>
    |}]

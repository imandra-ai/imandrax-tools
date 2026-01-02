open Printf
open Parse_common


let wip s = failwith (sprintf "WIP: %s" s)

(** Parse Constr variant of Ty_view.view to type annotation

Return:
  0: a chained sequence of applied types
  1: generic type parameters used
*)
let parse_constr_to_type_annot
  (ty_view: (unit, Uid.t, Type.t) Ty_view.view)
  : (string list * Uid.t list) =

  let rec helper
  (ty_view: (unit, Uid.t, Type.t) Ty_view.view)
  (ty_acc: string list)
  (params_acc: Uid.t list)
  : (string list * Uid.t list) =
    match ty_view with
    | Constr ((constr_uid : Uid.t), (constr_args : Type.t list)) ->
      let constr_name = constr_uid.name in
      begin match constr_args with
      | [] -> (constr_name :: ty_acc, params_acc)
      | [next_ty] ->
        let next_view = next_ty.view in
        helper next_view (constr_name :: ty_acc) params_acc
      | _next_ty :: _next_tys ->
        failwith "Never(parse_constr_to_type_annot): expected Constr with 0 or 1 args"
      end
    | Var _ -> wip "Var"
    | _ -> failwith "parse_constr_to_type_annot: expected Constr or Var"
  in

  helper ty_view [] []

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
    string * string list =
  let Ty_view.{ f : Uid.t; ty : Type.t; doc = _ } = rec_row in

  let arg_name = f.name in
  let Type.{ view = arg_ty_view; generation = _ } = ty in

  let (arg_type_names, _params) = parse_constr_to_type_annot arg_ty_view in

  (arg_name, arg_type_names)

(** Parse one row of an Algebraic type declaration

    Return:
    - dataclass name :: string
    - dataclass definition statement :: Ast.stmt

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
    - Each row of dataclass is a type annotation, `arg0: int` or `a: int` *)
let parse_adt_row_to_dataclass_def (adt_row : (Uid.t, Type.t) Ty_view.adt_row) :
    (*
  - dev
    - what about polymorphic types? `args` field?
    - what if Type.t list in args is another ADT?
    - how should we handle `doc` field? (it's ignored at the moment)
   *)
    string * Ast.stmt =
  let Ty_view.{ c; labels; args; doc = _ } = adt_row in
  let dc_name = c.name in

  (* labels -> dataclass field names *)
  let (dc_arg_names : string list) =
    match labels with
    | None -> Ast.anonymous_arg_names (List.length args)
    | Some id_list -> List.map (fun (id : Uid.t) -> id.name) id_list
  in

  (* constructor names, i.e., the type annotation for each field *)
  let (dc_arg_constr_names : string list list) =
    args
    |> List.map (fun (arg : Type.t) ->
           let Type.{ view = arg_ty_view; generation = _ } = arg in

           let (types, _params) = parse_constr_to_type_annot arg_ty_view in
           printf "types: %s\n" (String.concat ", " types);

           types)
  in

  let dataclass_def_stmt =
    Ast.mk_dataclass_def dc_name (List.combine dc_arg_names dc_arg_constr_names)
  in

  (* printf "Dataclass name: %s\n" dc_name;
  print_endline "Arg names:";
  List.iter (fun n -> print_endline n) dc_arg_names;
  print_endline "Arg constr names:";
  List.iter (fun n -> print_endline n) dc_arg_constr_names;
  print_endline "";
  print_endline (Ast.show_stmt dataclass_def_stmt); *)
  (dc_name, dataclass_def_stmt)

let parse_decl (decl : (Term.t, Type.t) Decl.t_poly) :
    (Ast.stmt list, string) result =
  match decl with
  | Ty (ty_view_def : Type.t Ty_view.def_poly) -> (
      (* Unpack ty view *)
      let {
        Ty_view.name = decl_name_uid;
        params = _;
        decl = ty_view_decl;
        clique = _;
        timeout = _;
      } =
        ty_view_def
      in

      (* Root name of the decl *)
      let { Uid.name = decl_name; view = _ } = decl_name_uid in
      (* TODO: move above extraction to single pattern match *)

      (* printf "Root name: %s\n" decl_name; *)

      (* Handle rows *)
      match ty_view_decl with
      | Algebraic (adt_rows : (Uid.t, Type.t) Ty_view.adt_row list) ->
          let (dc_names : string list), (dc_defs : Ast.stmt list) =
            adt_rows |> List.map parse_adt_row_to_dataclass_def |> List.split
          in
          let union_def = Ast.mk_union_def decl_name dc_names in
          let dc_and_union_defs = dc_defs @ [ union_def ] in
          Ok dc_and_union_defs
      | Record (rec_rows : (Uid.t, Type.t) Ty_view.rec_row list) ->
          let dc_args : (string * string list) list =
            rec_rows |> List.map parse_rec_row_to_dataclass_row
          in
          let dc_def = Ast.mk_dataclass_def decl_name dc_args in
          Ok [ dc_def ]
      | _ -> failwith "WIP: not Algebraic and not Tuple")
  | Fun _ -> failwith "WIP: Fun"
  | _ -> invalid_arg "parse_decl: expected Ty | Fun"

(* Expect tests
==================== *)

let sep : string = "\n" ^ CCString.repeat "<>" 10 ^ "\n"

(* Decl
-------------------- *)

let%expect_test "parse decl art" =
  (* let file_name = "variant_poly_two_var" in *)
  let file_name = "nested_generics" in
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

  [%expect {|
    name: nested_generics
    code:
     type 'a identity = Identity of 'a

    type 'a maybe =
      | Just of 'a
      | Nothing

    type 'a validated =
      | Valid of 'a
      | Invalid of string

    type 'a tagged = {
      value : 'a;
      tag : string
    }

    type my_ty =
      | My_ty of int identity maybe validated tagged

    <><><><><><><><><>
    Ty
      {
      name = my_ty/K4n0AuCEz3XMvL6LS1WaKVkIil-9a8rQZR_saAqxiFI;
      params = [];
      decl =
        Algebraic
          [{
             c = My_ty/qlujE9k3C2C0TL-8Mzf_oEHJ9T5i4Y9ntjvl-NCkxa0;
             labels = None;
             args =
               [{ view =
                    (Constr
                      (tagged/I_WZIRhH76rALxpX_YhaelRmb4LqKVzs6ywNGiWMgdY,
                       [{ view =
                            (Constr
                              (validated/PxYEY49NXkjiR4aKBofz7rfhE6xXP_VLvNuiAIWJAcM,
                               [{ view =
                                    (Constr
                                      (maybe/Cc1rj0QipyuO7yl3jKSzWyL5D22dfxaHNBb9ATN70Js,
                                       [{ view =
                                            (Constr
                                              (identity/4AMLniQ-sMxTldiYfhr9PsBGq2-v0HfXToWpp68DlAQ,
                                               [{ view = (Constr (int,[]));
                                                  generation = 1 }]));
                                          generation = 1 }]));
                                  generation = 1 }]));
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
  [%expect {|
    types: int, identity, maybe, validated, tagged
    <><><><><><><><><>
    (Ast_types.ClassDef
       { Ast_types.name = "My_ty"; bases = []; keywords = [];
         body =
         [(Ast_types.AnnAssign
             { Ast_types.target =
               (Ast_types.Name { Ast_types.id = "arg0"; ctx = Ast_types.Load });
               annotation =
               (Ast_types.Name { Ast_types.id = "tagged"; ctx = Ast_types.Load });
               value = None; simple = 1 })
           ];
         decorator_list =
         [(Ast_types.Name { Ast_types.id = "dataclass"; ctx = Ast_types.Load })]
         })
    (Ast_types.Assign
       { Ast_types.targets =
         [(Ast_types.Name { Ast_types.id = "my_ty"; ctx = Ast_types.Load })];
         value =
         (Ast_types.Name { Ast_types.id = "My_ty"; ctx = Ast_types.Load });
         type_comment = None })
    <><><><><><><><><>
    |}]

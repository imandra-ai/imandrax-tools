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
    | Var (var_uid : Uid.t) ->
      let type_var_name = var_uid.name in
      (type_var_name :: ty_acc, var_uid :: params_acc)
    | _ -> failwith "parse_constr_to_type_annot: expected Constr or Var"
  in

  let (ty_acc, params_acc) = helper ty_view [] [] in
  (List.rev ty_acc, params_acc)


(** Define type variable

Example:
  - 'a/92728' -> `a = TypeVar('a')`
*)
let type_var_def_of_uid (uid : Uid.t) : Ast.stmt =
  let name = uid.name in
  Assign {
    targets = [
      Ast.mk_name_expr name
    ];
    value = Ast.Call {
      func = Ast.mk_name_expr "TypeVar";
      args = [
        Ast.Constant {
          value = String name;
          kind = None;
        }
      ];
      keywords = [];
    };
    type_comment = None;
  }

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
  let (dc_arg_constr_names : string list list), (dc_arg_type_params : Uid.t list) =
    (args : Mir.Type.t list)
    |> List.map (
      fun (arg : Type.t) ->
      let Type.{ view = arg_ty_view; generation = _ } = arg in

      let (types, params) = parse_constr_to_type_annot arg_ty_view in

      (types, params)
    )
    |> List.split
    |> fun (dc_arg_constr_names, dc_arg_type_params_by_arg) ->
      (* Flatten and dedup type params *)
      let (dc_arg_type_params: Uid.t list) =
        dc_arg_type_params_by_arg
        |> List.flatten
        |> CCList.sort_uniq ~cmp:Uid.compare
      in
      (dc_arg_constr_names, dc_arg_type_params)
  in

  let dataclass_def_stmt =
    Ast.mk_dataclass_def
      dc_name
      (Some (dc_arg_type_params |> List.map (fun (uid : Uid.t) -> uid.name)))
      (List.combine dc_arg_names dc_arg_constr_names)
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
        params = (params : Uid.t list);
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
      let (decl_body : Ast_types.stmt list) = begin match ty_view_decl with
      | Algebraic (adt_rows : (Uid.t, Type.t) Ty_view.adt_row list) ->
          let (dc_names : string list), (dc_defs : Ast.stmt list) =
            adt_rows |> List.map parse_adt_row_to_dataclass_def |> List.split
          in
          let union_def = Ast.mk_union_def decl_name dc_names in
          let dc_and_union_defs = dc_defs @ [ union_def ] in
          dc_and_union_defs
      | Record (rec_rows : (Uid.t, Type.t) Ty_view.rec_row list) ->
          let dc_args : (string * string list) list =
            rec_rows |> List.map parse_rec_row_to_dataclass_row
          in
          let dc_def = Ast.mk_dataclass_def decl_name None dc_args in
          [ dc_def ]
      | _ -> failwith "WIP: not Algebraic and not Tuple"
        end
      in
      Ok (type_var_defs @ decl_body)

  )
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

  [%expect {|
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
      params = [a/92728; b/92729];
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
             args = [{ view = (Var a/92728);
                       generation = 1 }];
             doc = None
             };
           {
             c = Pair/B4x1x4rxXHCSWnsOlIPrDcyh6TbWcJB8Gg_-cZtmoF4;
             labels = None;
             args =
               [{ view = (Var a/92728);
                  generation = 1 };
                { view = (Var b/92729);
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
               [{ view = (Var a/92728);
                  generation = 1 };
                { view = (Var b/92729);
                  generation = 1 }];
             doc = None
             };
           {
             c = Multi/pHB4Isq3zq0g3pg2KaAB7IAKGm336YEoe1ws7w1MzD8;
             labels = None;
             args =
               [{ view =
                    (Constr (list,[{ view = (Var a/92728);
                                     generation = 1 }]));
                  generation = 1 };
                { view =
                    (Constr (list,[{ view = (Var b/92729);
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
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Failure "WIP: Var")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Imandrax_codegen__Parse_decl.parse_constr_to_type_annot in file "packages/imandrax-codegen/lib/parse_decl.ml", line 37, characters 29-49
  Called from Imandrax_codegen__Parse_decl.parse_adt_row_to_dataclass_def.(fun) in file "packages/imandrax-codegen/lib/parse_decl.ml", line 113, characters 34-72
  Called from Stdlib__List.map in file "list.ml", line 83, characters 15-19
  Called from Imandrax_codegen__Parse_decl.parse_adt_row_to_dataclass_def in file "packages/imandrax-codegen/lib/parse_decl.ml", lines 109-115, characters 4-17
  Called from Stdlib__List.map in file "list.ml", line 87, characters 15-19
  Called from Imandrax_codegen__Parse_decl.parse_decl in file "packages/imandrax-codegen/lib/parse_decl.ml", line 156, characters 12-63
  Called from Imandrax_codegen__Parse_decl.(fun) in file "packages/imandrax-codegen/lib/parse_decl.ml", line 281, characters 15-33
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]

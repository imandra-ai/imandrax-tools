open Printf
open Parse_common

(* let parse_term = Parse_term.parse_term *)

(*
Parse one row of an Algebraic type declaration


Return:
- dataclass name :: string
- dataclass definition statement :: Ast.stmt

- Handle the Algebraic case of Delc.t where payload is a list of adt_row
- Each adt_row should maps to a dataclass definition in Python (a list of statements)

Mapping
- Each adt_row contains
  - constructor id (name) :: Uid.t
  - labels :: Uid.t list option: a list of labels for inline records
    - when this exists, we should generate a dataclass with non-anonymous fields
  - args: Type.t list
    - the type annotation for a row
    - Q: what if this is another ADT?
  - doc :: string option: ?

In Python
- Each row of dataclass is a type annotation, `arg0: int` or `a: int`

- dev
  - Q
    - what about polymorphic types? `args` field?
*)
let parse_adt_row_to_dataclass_def (adt_row : (Uid.t, Type.t) Ty_view.adt_row) :
    string * Ast.stmt =
  let Ty_view.{ c; labels; args; doc = _ } = adt_row in
  let dc_name = c.name in

  (* labels -> dataclass field names *)
  let dc_arg_names =
    match labels with
    | None -> Ast.anonymous_arg_names (List.length args)
    | Some id_list -> List.map (fun (id : Uid.t) -> id.name) id_list
  in

  let dc_arg_constr_names =
    args
    |> List.map (fun (arg : Type.t) ->
           let Type.{ view = arg_ty_view; generation = _ } = arg in
           match arg_ty_view with
           | Constr ((constr_uid : Uid.t), (constr_args : Type.t list)) ->
               let constr_name = constr_uid.name in
               let _todo = constr_args in
               constr_name
           | _ ->
               let msg =
                 "parse_adt_row_to_dataclass_def: expected Constr for \
                  adt_row.args.[] | view"
               in
               failwith msg)
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
  let parsed_decl =
    match decl with
    | Ty (ty_view_def : Type.t Ty_view.def_poly) ->
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

        printf "Root name: %s\n" decl_name;

        (* TODO: move above extraction to single pattern match *)

        (* Handle rows *)
        let (dc_names : string list), (dc_defs : Ast.stmt list) =
          match ty_view_decl with
          | Algebraic (adt_rows : (Uid.t, Type.t) Ty_view.adt_row list) ->
              let dc_names, dc_defs =
                adt_rows
                |> List.map parse_adt_row_to_dataclass_def
                |> List.split
              in
              (dc_names, dc_defs)
          | Record (rec_rows : (Uid.t, Type.t) Ty_view.rec_row list) ->
              let _ = rec_rows in

              failwith "WIP"
          | _ -> failwith "WIP: not Algebraic and not Tuple"
        in

        let union_def = Ast.mk_union_def decl_name dc_names in
        let all_defs = dc_defs @ [ union_def ] in
        all_defs
    | _ -> invalid_arg "parse_decl: expected Ty"
  in

  Ok parsed_decl

(* Expect tests
==================== *)

let sep : string = "\n" ^ CCString.repeat "<>" 10 ^ "\n"

(* Decl
-------------------- *)

let%expect_test "parse decl art" =
  let yaml_str =
    (* CCIO.File.read_exn "../test/data/decl/variant_simple.yaml" *)
    (* CCIO.File.read_exn "../test/data/decl/variant_with_payload.yaml" *)
    (* CCIO.File.read_exn "../test/data/decl/variant_recursive.yaml" *)
    CCIO.File.read_exn "../test/data/decl/record.yaml"
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
  (Failure WIP)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Imandrax_codegen__Parse_decl.parse_decl in file "packages/imandrax-codegen/lib/parse_decl.ml", line 112, characters 14-28
  Called from Imandrax_codegen__Parse_decl.(fun) in file "packages/imandrax-codegen/lib/parse_decl.ml", line 189, characters 15-33
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28

  Trailing output
  ---------------
  name: record
  code:
   type point = { x: int; y: int }

  let distance_category = fun p ->
    let sum = p.x + p.y in
    if sum < 0 then "negative"
    else if sum = 0 then "origin"
    else "positive"

  <><><><><><><><><>
  Ty
    {
    name = point/4N82zNFBEhrlrmHNKK_4jDYtpXt8hvmRWC4-N437xp8;
    params = [];
    decl =
      Record
        [{
           f = x/40plBYPa-nKP5TMortqhgELkBB1Ekmxx-T83mnr6Rlk;
           ty = { view = (Constr (int,[]));
                  generation = 1 };
           doc = None
           };
         {
           f = y/2q8uB6Fb8Uwf-NuF89IsRU8PWpxeLPDxZB2Kk_W6oqE;
           ty = { view = (Constr (int,[]));
                  generation = 1 };
           doc = None
           }];
    clique = None;
    timeout = None
    }
  Root name: point
  |}]

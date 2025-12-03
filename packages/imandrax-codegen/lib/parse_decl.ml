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
    (string * Ast.stmt) =
  let Ty_view.{ c; labels; args; doc = _ } = adt_row in
  let dc_name = c.name in

  (* labels -> dataclass field names *)
  let dc_arg_names =
    match labels with
    | None -> Ast.anonymous_arg_names (List.length args)
    | Some id_list -> List.map (fun (id : Uid.t) -> id.name) id_list
  in

  (* args ::  *)
  let dc_arg_constr_names =
    args
    |> List.map (fun (arg : Type.t) ->
           let Type.{ view = arg_ty_view; generation = _ } = arg in
           match arg_ty_view with
           | Constr ((constr_uid: Uid.t), (constr_args: Type.t list)) -> (
               let constr_name = constr_uid.name in
               let _todo = constr_args in
               constr_name
           )
           | _ ->
               let msg =
                 "parse_adt_row_to_dataclass_def: expected Constr for \
                  adt_row.args.[] | view"
               in
               failwith msg)
  in

  let dataclass_def_stmt = Ast.mk_dataclass_def dc_name (List.combine dc_arg_names dc_arg_constr_names) in

  printf "Dataclass name: %s\n" dc_name;
  print_endline "Arg names:";
  List.iter (fun n -> print_endline n) dc_arg_names;
  print_endline "Arg constr names:";
  List.iter (fun n -> print_endline n) dc_arg_constr_names;
  print_endline "";
  print_endline (Ast.show_stmt dataclass_def_stmt);

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
        let ((dc_names: string list), (dc_defs: Ast.stmt list)) =
          match ty_view_decl with
          | Algebraic adt_rows ->
              let (dc_names, dc_defs) = adt_rows |> List.map parse_adt_row_to_dataclass_def |> List.split in
              (dc_names, dc_defs)
          | _ -> failwith "WIP: not Algebraic"
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
  (* let yaml_str = CCIO.File.read_exn "../test/data/decl/record.yaml" in *)
  (* let yaml_str = CCIO.File.read_exn "../test/data/decl/variant_simple.yaml" in *)
  let yaml_str =
    CCIO.File.read_exn "../test/data/decl/variant_with_payload.yaml"
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
  (* Print parsed
  -------------------- *)
  let decl =
    match decls with [ decl ] -> decl | _ -> failwith "Expected one decl"
  in
  let parsed = decl |> parse_decl |> unwrap in

  printf "<><><><><><><><><>\n";

  List.iter (fun stmt -> print_endline (Ast.show_stmt stmt)) parsed;

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
    name: variant_with_payload
    code:
     type shape =
    | Point
    | Circle of int
    | Rectangle of int * int
    | Triangle of {a: int; b: int; c: int}

    <><><><><><><><><>
    Root name: shape
    Dataclass name: Point
    Arg names:
    Arg constr names:

    (Ast_types.ClassDef
       { Ast_types.name = "Point"; bases = []; keywords = [];
         body = [Ast_types.Pass];
         decorator_list =
         [(Ast_types.Name { Ast_types.id = "dataclass"; ctx = Ast_types.Load })]
         })
    Dataclass name: Circle
    Arg names:
    arg0
    Arg constr names:
    int

    (Ast_types.ClassDef
       { Ast_types.name = "Circle"; bases = []; keywords = [];
         body =
         [(Ast_types.AnnAssign
             { Ast_types.target =
               (Ast_types.Name { Ast_types.id = "arg0"; ctx = Ast_types.Load });
               annotation =
               (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
               value = None; simple = 1 })
           ];
         decorator_list =
         [(Ast_types.Name { Ast_types.id = "dataclass"; ctx = Ast_types.Load })]
         })
    Dataclass name: Rectangle
    Arg names:
    arg0
    arg1
    Arg constr names:
    int
    int

    (Ast_types.ClassDef
       { Ast_types.name = "Rectangle"; bases = []; keywords = [];
         body =
         [(Ast_types.AnnAssign
             { Ast_types.target =
               (Ast_types.Name { Ast_types.id = "arg0"; ctx = Ast_types.Load });
               annotation =
               (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
               value = None; simple = 1 });
           (Ast_types.AnnAssign
              { Ast_types.target =
                (Ast_types.Name { Ast_types.id = "arg1"; ctx = Ast_types.Load });
                annotation =
                (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
                value = None; simple = 1 })
           ];
         decorator_list =
         [(Ast_types.Name { Ast_types.id = "dataclass"; ctx = Ast_types.Load })]
         })
    Dataclass name: Triangle
    Arg names:
    a
    b
    c
    Arg constr names:
    int
    int
    int

    (Ast_types.ClassDef
       { Ast_types.name = "Triangle"; bases = []; keywords = [];
         body =
         [(Ast_types.AnnAssign
             { Ast_types.target =
               (Ast_types.Name { Ast_types.id = "a"; ctx = Ast_types.Load });
               annotation =
               (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
               value = None; simple = 1 });
           (Ast_types.AnnAssign
              { Ast_types.target =
                (Ast_types.Name { Ast_types.id = "b"; ctx = Ast_types.Load });
                annotation =
                (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
                value = None; simple = 1 });
           (Ast_types.AnnAssign
              { Ast_types.target =
                (Ast_types.Name { Ast_types.id = "c"; ctx = Ast_types.Load });
                annotation =
                (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
                value = None; simple = 1 })
           ];
         decorator_list =
         [(Ast_types.Name { Ast_types.id = "dataclass"; ctx = Ast_types.Load })]
         })
    <><><><><><><><><>
    (Ast_types.ClassDef
       { Ast_types.name = "Point"; bases = []; keywords = [];
         body = [Ast_types.Pass];
         decorator_list =
         [(Ast_types.Name { Ast_types.id = "dataclass"; ctx = Ast_types.Load })]
         })
    (Ast_types.ClassDef
       { Ast_types.name = "Circle"; bases = []; keywords = [];
         body =
         [(Ast_types.AnnAssign
             { Ast_types.target =
               (Ast_types.Name { Ast_types.id = "arg0"; ctx = Ast_types.Load });
               annotation =
               (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
               value = None; simple = 1 })
           ];
         decorator_list =
         [(Ast_types.Name { Ast_types.id = "dataclass"; ctx = Ast_types.Load })]
         })
    (Ast_types.ClassDef
       { Ast_types.name = "Rectangle"; bases = []; keywords = [];
         body =
         [(Ast_types.AnnAssign
             { Ast_types.target =
               (Ast_types.Name { Ast_types.id = "arg0"; ctx = Ast_types.Load });
               annotation =
               (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
               value = None; simple = 1 });
           (Ast_types.AnnAssign
              { Ast_types.target =
                (Ast_types.Name { Ast_types.id = "arg1"; ctx = Ast_types.Load });
                annotation =
                (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
                value = None; simple = 1 })
           ];
         decorator_list =
         [(Ast_types.Name { Ast_types.id = "dataclass"; ctx = Ast_types.Load })]
         })
    (Ast_types.ClassDef
       { Ast_types.name = "Triangle"; bases = []; keywords = [];
         body =
         [(Ast_types.AnnAssign
             { Ast_types.target =
               (Ast_types.Name { Ast_types.id = "a"; ctx = Ast_types.Load });
               annotation =
               (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
               value = None; simple = 1 });
           (Ast_types.AnnAssign
              { Ast_types.target =
                (Ast_types.Name { Ast_types.id = "b"; ctx = Ast_types.Load });
                annotation =
                (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
                value = None; simple = 1 });
           (Ast_types.AnnAssign
              { Ast_types.target =
                (Ast_types.Name { Ast_types.id = "c"; ctx = Ast_types.Load });
                annotation =
                (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
                value = None; simple = 1 })
           ];
         decorator_list =
         [(Ast_types.Name { Ast_types.id = "dataclass"; ctx = Ast_types.Load })]
         })
    (Ast_types.Assign
       { Ast_types.targets =
         [(Ast_types.Name { Ast_types.id = "shape"; ctx = Ast_types.Load })];
         value =
         (Ast_types.BinOp
            { Ast_types.left =
              (Ast_types.BinOp
                 { Ast_types.left =
                   (Ast_types.BinOp
                      { Ast_types.left =
                        (Ast_types.Name
                           { Ast_types.id = "Point"; ctx = Ast_types.Load });
                        op = Ast_types.BitOr;
                        right =
                        (Ast_types.Name
                           { Ast_types.id = "Circle"; ctx = Ast_types.Load })
                        });
                   op = Ast_types.BitOr;
                   right =
                   (Ast_types.Name
                      { Ast_types.id = "Rectangle"; ctx = Ast_types.Load })
                   });
              op = Ast_types.BitOr;
              right =
              (Ast_types.Name { Ast_types.id = "Triangle"; ctx = Ast_types.Load })
              });
         type_comment = None })
    <><><><><><><><><>
    Ty
      {
      name = shape/LRvMhhEHMdTSx76rtFLbuIIlQvd6pf-4f4moqBZzlLk;
      params = [];
      decl =
        Algebraic
          [{
             c = Point/zmf0aqMC_axCmK7HHULw_vLRRPsZcYSY0rDnEfsNBH4;
             labels = None;
             args = [];
             doc = None
             };
           {
             c = Circle/SMNwkTSaN2qWlbF6hzAjvEZgnq8GAChMuxuLVuuQ0CI;
             labels = None;
             args = [{ view = (Constr (int,[]));
                       generation = 1 }];
             doc = None
             };
           {
             c = Rectangle/MhV0gfbrP5fVL3HBRcv_kIpiDQpIDYUqTG5xZPeKBFE;
             labels = None;
             args =
               [{ view = (Constr (int,[]));
                  generation = 1 };
                { view = (Constr (int,[]));
                  generation = 1 }];
             doc = None
             };
           {
             c = Triangle/m10MbB_wDF6lzCDu31fUAid6KYjDtZfaoKdwopYRWMs;
             labels =
               (Some
                 [a/LW4xXxrPKQVl9T6AgQwQ0vT7qFQVSzZ9_croJJv7CPs;
                  b/XYqiqHuwZfky9w3RIahW_9PWastTQo_ux3y-7BJ5V6s;
                  c/s7ipAMnTq9Um_H-PZpC39hw_wYNr2F5pDCJBknvEX4k]);
             args =
               [{ view = (Constr (int,[]));
                  generation = 1 };
                { view = (Constr (int,[]));
                  generation = 1 };
                { view = (Constr (int,[]));
                  generation = 1 }];
             doc = None
             }];
      clique = None;
      timeout = None
      }
    |}]

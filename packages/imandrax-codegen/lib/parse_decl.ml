open Printf
(* open Parse_common *)

(* let parse_term = Parse_term.parse_term *)


(* let parse_decl (decl: Decl.t) : (Ast.stmt list, string) result =
  _ *)


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

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
  let yaml_str = CCIO.File.read_exn "../test/data/decl/variant_with_payload.yaml" in
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
  [%expect {|
    name: variant_with_payload
    code:
     type shape =
    | Point
    | Circle of int
    | Rectangle of int * int
    | Triangle of {a: int; b: int; c: int}

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

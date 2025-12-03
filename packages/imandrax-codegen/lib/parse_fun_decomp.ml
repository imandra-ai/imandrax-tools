open Printf
open Imandrax_api
open CCFun
open Parse_common

let parse_term = Parse_term.parse_term

(* Return a tuple of
  - a list of model terms, which are the model "inputs"
  - the model eval term
  - invariant string
  - constraints string list
*)
let parse_region (region : (Term.term, Type.t) Mir.Region.t_poly) :
    (string * Term.term) list * Term.term * (string * string list) =
  let (model : (string * Term.term) list) =
    region.meta |> List.assoc "model" |> function
    | Assoc assoc ->
        assoc
        |> List.map (function
             | arg_name, Region.Term t -> (arg_name, t)
             | _ -> failwith "Never: values of model assoc should be a term")
    | _ -> failwith "Never: model should be an Assoc"
  in
  let model_eval = region.meta |> List.assoc "model_eval" in
  let model_eval_term =
    match model_eval with
    | Term t -> t
    | _ -> failwith "Never: model_eval should be a term"
  in

  let meta_str = region.meta |> List.assoc "str" in
  let (invariant : string), (constraints : string list) =
    match meta_str with
    | Assoc assoc ->
        let extract_string key = function
          | Region.String s -> s
          | _ ->
              failwith (Printf.sprintf "Never: %s should be a meta string" key)
        in
        let extract_list key = function
          | Region.List l -> l
          | _ -> failwith (Printf.sprintf "Never: %s should be a meta list" key)
        in
        let invariant =
          List.assoc "invariant" assoc |> extract_string "invariant"
        in
        let constraints_meta =
          List.assoc "constraints" assoc |> extract_list "constraints"
        in
        let constraints =
          List.map (extract_string "constraint") constraints_meta
        in
        (invariant, constraints)
    | _ -> failwith "Never: meta_str should be an Assoc"
  in

  (model, model_eval_term, (invariant, constraints))

let uniq_stmts (stmts : Ast.stmt list) : Ast.stmt list =
  let tbl = Hashtbl.create (List.length stmts) in
  List.filter
    (fun x ->
      if Hashtbl.mem tbl x then false
      else (
        Hashtbl.add tbl x ();
        true))
    stmts

let format_invariant_constraints_as_yaml_string
    ?(with_fence : bool = false)
    (invariant : string)
    (constraints : string list) : string =
  let yaml_meta : Yaml.value =
    `O
      [
        ("invariant", `String invariant);
        ("constraints", `A (List.map (fun s -> `String s) constraints));
      ]
  in
  let yaml_str = Yaml.to_string_exn yaml_meta in
  if with_fence then
    let fence_yaml = String.trim %> sprintf "```yaml\n%s\n```" in
    fence_yaml yaml_str
  else yaml_str

let indent i s =
  let indent_str = String.make i ' ' in
  indent_str ^ CCString.ltrim s

let format_invariant_constraints_as_markdown_list
    (invariant : string)
    (constraints : string list) : string =
  let invariant_str = sprintf "- invariant: %s" invariant in
  let constraints_strs = constraints |> List.map (fun s -> sprintf "- %s" s) in
  invariant_str :: "- constraints:" :: (constraints_strs |> List.map (indent 4))
  |> String.concat "\n"
  |> fun s -> s ^ "\n"

(*

Arguments:
  - output_as_dict: whether to output the test data as a dictionary
    instead of test function definitions
  - ...
*)
let parse_fun_decomp
    ?(output_as_dict : bool = false)
    (fun_decomp : Mir.Fun_decomp.t) : Ast.stmt list =
  (* Ast.stmt list  *)
  match fun_decomp with
  | { f_id = Uid.{ name = f_id_name; view = _ }; f_args = _; regions } ->
      let n_region = regions |> List.length in

      (* The last list is the dimension of region
       The second to last list is the dimension of f_args
    *)
      let ( (model_by_arg_by_region : (string * Term.term) list list),
            (model_evals : Term.term list),
            (invariant_and_constraints_by_region : (string * string list) list)
          ) =
        regions |> List.map parse_region |> unzip3
      in

      let ( (models_type_defs : (string * Ast.stmt list) list list),
            (* We have no place to use type annots for input args*)
            (_models_type_annots : (string * Ast.expr option) list list),
            (models_terms : (string * Ast.expr) list list) ) =
        model_by_arg_by_region
        |> List.map (fun (model_by_arg : (string * Term.term) list) ->
               List.map
                 (fun (arg_name, model) ->
                   let type_defs, type_annot, term_expr =
                     parse_term model |> unwrap
                   in
                   (* bind arg name again *)
                   ( (arg_name, type_defs),
                     (arg_name, type_annot),
                     (arg_name, term_expr) ))
                 model_by_arg)
        |> List.map unzip3 |> unzip3
      in

      let test_names =
        List.map (fun i -> Printf.sprintf "test_%d" i) CCList.(1 -- n_region)
      in

      let docstr_body_by_region : string list =
        invariant_and_constraints_by_region
        |> List.map (fun (invariant, constraints) ->
               format_invariant_constraints_as_markdown_list invariant
                 constraints)
      in

      let docstr_by_region =
        List.map2
          (fun title body -> sprintf "%s\n\n%s" title body)
          test_names docstr_body_by_region
      in

      let model_eval_type_defs_s, model_eval_type_annots, model_eval_exprs =
        model_evals
        |> List.map (fun model_eval -> parse_term model_eval |> unwrap)
        |> unzip3
      in

      (* Deduplicate type definitions *)
      let type_defs : Ast.stmt list =
        let model_type_defs_flattened : Ast.stmt list =
          let model_type_defs_by_arg = models_type_defs |> List.flatten in
          model_type_defs_by_arg
          |> List.map (fun (_, stmts) -> stmts)
          |> List.flatten
        in
        model_type_defs_flattened @ (model_eval_type_defs_s |> List.flatten)
        |> uniq_stmts
      in

      let tests : Ast.stmt list =
        match output_as_dict with
        | true ->
            let test_value : Ast.stmt =
              Ast.mk_test_data_dict ~test_names ~f_args_list:models_terms
                ~expected_list:model_eval_exprs
            in
            [ test_value ]
        | false ->
            let test_functions : Ast.stmt list =
              List.map
                (fun ( test_name,
                       docstr,
                       model,
                       model_eval_type_annot,
                       model_eval ) ->
                  Ast.def_test_function ~test_name ~f_name:f_id_name
                    ~docstr:(Some docstr) ~f_args:model
                    ~output_type_annot:model_eval_type_annot
                    ~expected:model_eval)
                (zip5 test_names docstr_by_region models_terms
                   model_eval_type_annots model_eval_exprs)
            in
            test_functions
      in
      type_defs @ tests

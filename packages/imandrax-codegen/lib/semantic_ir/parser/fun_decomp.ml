(** Parser for transforming MIR Fun_decomp to semantic IR *)

let parse_term = Term.parse_term

open Common_
open Printf
open CCFun
module Sir = Types

module Docstring_utils : sig
  (** Format the invariant and constraints as a YAML string

  ```yaml
  invariant: invariant
  constraints:
    - constraint1
    - constraint2
  ```
  *)
  val format_invariant_constraints_as_yaml_string
    : ?with_fence:bool -> string -> string list -> string

  (** Format the invariant and constraints as a markdown list *)
  val format_invariant_constraints_as_markdown_list
    : string -> string list -> string
end = struct
  let format_invariant_constraints_as_yaml_string
      ?(with_fence : bool = false)
      (invariant : string)
      (constraints : string list)
      : string =
    let yaml_meta : Yaml.value =
      `O
        [ "invariant", `String invariant
        ; "constraints", `A (List.map (fun s -> `String s) constraints)
        ]
    in
    let yaml_str = Yaml.to_string_exn yaml_meta in
    if with_fence
    then (
      let fence_yaml = String.trim %> sprintf "```yaml\n%s\n```" in
      fence_yaml yaml_str)
    else yaml_str
  ;;

  let indent i s =
    let indent_str = String.make i ' ' in
    indent_str ^ CCString.ltrim s
  ;;

  let format_invariant_constraints_as_markdown_list
      (invariant : string)
      (constraints : string list)
      : string =
    let invariant_str = sprintf "- invariant: %s" invariant in
    let constraints_strs =
      constraints |> List.map (fun s -> sprintf "- %s" s)
    in
    invariant_str
    :: "- constraints:"
    :: (constraints_strs |> List.map (indent 4))
    |> String.concat "\n"
    |> fun s -> s ^ "\n"
  ;;
end

(* Return a tuple of
  - 0: model input, a list of model terms
  - 1: model eval, the model eval term
  - 3: invariant string
  - 4: constraints string list
*)
let parse_region (region : (Term.term, Type.t) Mir.Region.t_poly)
    : (string * Term.term) list * Term.term * (string * string list) =
  let (model : (string * Term.term) list) =
    region.meta
    |> List.assoc "model"
    |> function
    | Assoc assoc ->
        assoc
        |> List.map (function
             | arg_name, Region.Term t -> arg_name, t
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
        invariant, constraints
    | _ -> failwith "Never: meta_str should be an Assoc"
  in

  model, model_eval_term, (invariant, constraints)
;;

type model =
  { name : string
  ; ty : Sir.type_expr
  ; tm : Sir.value
  }

let parse_fun_decomp (fun_decomp : Mir.Fun_decomp.t) : Sir.test_suite =
  match fun_decomp with
  | { f_id = Uid.{ name = f_id_name; view = _ }; f_args = _; regions } ->
      let (test_name_by_region : string list) =
        let n_region = regions |> List.length in
        List.map (fun i -> Printf.sprintf "test_%d" i) CCList.(1 -- n_region)
      in

      (* The last list is the dimension of region
       The second to last list is the dimension of f_args
       *)
      let ( (model_by_arg_by_region : (string * Term.term) list list)
          , (model_eval_by_region : Term.term list)
          , (invariant_and_constraints_by_region : (string * string list) list)
          ) =
        regions |> List.map parse_region |> unzip3
      in

      let models_by_region : model list list =
        model_by_arg_by_region
        |> List.map (fun (model_by_arg : (string * Term.term) list) ->
               List.map
                 (fun (arg_name, model) ->
                   let type_annot, term_expr = parse_term model |> unwrap in
                   { name = arg_name; ty = type_annot; tm = term_expr })
                 model_by_arg)
      in

      let ( (model_eval_type_annot_by_region : Sir.type_expr list)
          , (model_eval_expr_by_region : Sir.value list) ) =
        model_eval_by_region
        |> List.map (fun model_eval ->
               parse_term model_eval
               |> unwrap
               |> fun (type_annot_opt, term_expr) -> type_annot_opt, term_expr)
        |> List.split
      in

      let (docstr_by_region : string list) =
        let docstr_body_by_region : string list =
          invariant_and_constraints_by_region
          |> List.map (fun (invariant, constraints) ->
                 Docstring_utils.format_invariant_constraints_as_markdown_list
                   invariant
                   constraints)
        in
        List.map2
          (fun title body -> sprintf "%s\n\n%s" title body)
          test_name_by_region
          docstr_body_by_region
      in

      zip5
        test_name_by_region
        docstr_by_region
        models_by_region
        model_eval_expr_by_region
        model_eval_type_annot_by_region
      |> List.map
           (fun
             ( (test_name : string)
             , (docstr : string)
             , (models : model list)
             , (expected : Sir.value)
             , (expected_type : Sir.type_expr) )
           ->
             let f_args =
               models |> List.map (fun model -> model.name, model.ty, model.tm)
             in
             let (test_decl : Sir.test_decl) =
               { name = test_name
               ; f_name = f_id_name
               ; f_args
               ; f_output = expected_type, expected
               ; docstr
               }
             in
             test_decl)
;;

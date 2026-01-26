open Common_
(** Parser for transforming MIR Fun_decomp to semantic IR *)

open Printf
open CCFun
module Sir = Semantic_ir.Types

module Docstring_utils : sig
  val format_invariant_constraints_as_yaml_string :
    ?with_fence:bool -> string -> string list -> string
  (** Format the invariant and constraints as a YAML string

  ```yaml
  invariant: invariant
  constraints:
    - constraint1
    - constraint2
  ```
  *)

  (** Format the invariant and constraints as a markdown list *)
  val format_invariant_constraints_as_markdown_list :
    string -> string list -> string
end = struct
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
    let constraints_strs =
      constraints |> List.map (fun s -> sprintf "- %s" s)
    in
    invariant_str :: "- constraints:"
    :: (constraints_strs |> List.map (indent 4))
    |> String.concat "\n"
    |> fun s -> s ^ "\n"
end

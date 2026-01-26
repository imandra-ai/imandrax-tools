(** Parser for transforming MIR Term to semantic IR *)

open Common_
open Printf

module Sir = Semantic_ir.Types


(** Parse a MIR term to Type expression and term expression.
- Arg: Mir term

- Return:
  - | Ok of:
    - type-annotation (expression option)
    - expression (term)
  - | Error of string
*)
let rec parse_term (term : Term.term) :
    (Sir.type_expr option * Sir.value, string) result =
  (* Dev note:
    A term has fields of:
      - view ((Term.t, Type.t) Term.view)
      - ty (Type.t)

    (Term.t, Type.t) Term.view is a variant of
    - | Const of Const.t
    - | Apply of { f : Term.t; l : Term.t list }
    - | Construct of {
      - c: Type.t Applied_symbol.t_poly
      - args: Term.t list
      - labels: Uid.t list option
      - }
    - | Tuple of { l : Term.t list }
    - | Record of {
      - rows: (Type.t Applied_symbol.t_poly * Term.t) list
      - rest: Term.t option
      - }
    *)
  let debug = true in

  match ((term.view : (Term.term, Type.t) Term.view), (term.ty : Type.t)) with
  (* Constant *)
  | Term.Const const, _ -> (
      match const with
      | Const_bool b -> Ok (Some (TBase "bool"), VConst (CBool b))
      | Const_float f -> Ok (Some (TBase "float"), VConst (CFloat f))
      | Const_q q ->
          let num = Q.num q in
          let den = Q.den q in
          (* Convert rational to float *)
          let float_val = Z.to_float num /. Z.to_float den in
          Ok (Some (TBase "float"), VConst (CFloat float_val))
      | Const_z z -> Ok (Some (TBase "int"), VConst (CInt (Z.to_int z)))
      | Const_string s -> Ok (Some (TBase "str"), VConst (CString s))
      | c ->
          (* Uid and real_approx *)
          let msg = sprintf "unhandled const %s" (Imandrax_api.Const.show c) in
          Error msg)
  (* Tuple *)
  | Term.Tuple { l = (terms : Term.term list) }, (_ty : Type.t) ->
      let parsed_elems =
        List.map (fun term -> parse_term term |> unwrap) terms
      in
      let type_annot_of_elems, term_of_elems = List.split parsed_elems in
      let tuple_type =
        type_annot_of_elems
        |> List.map (CCOption.get_exn_or "Tuple element has no type annotation")
      in
      Ok (Some (TTuple tuple_type), Sir.VTuple term_of_elems)
  (* Record *)
  | ( Term.Record { rows; rest = _ },
      (ty : Type.t) ) ->
      (* Get record type name from ty *)
      let ty_name =
        match ty.view with
        | Ty_view.Constr (constr_name_uid, _constr_args) -> constr_name_uid.name
        | _ -> failwith "Never: ty should be a constr"
      in
      let record_type, _type_vars = type_expr_of_mir_ty_view_constr ty.view in

      (* Extract field names and values from rows *)
      let fields =
        rows
        |> List.map (fun (field_sym, term) ->
               let field_name = (field_sym : Type.t Applied_symbol.t_poly).sym.id.name in
               let _field_ty, field_val = parse_term term |> unwrap in
               (field_name, field_val))
      in

      Ok (Some record_type, Sir.VRecord { type_name = ty_name; fields })
  (* Construct - LChar.t *)
  | ( Term.Construct
        {
          c = (_ : Type.t Applied_symbol.t_poly);
          args = (construct_args : Term.term list);
          labels = _;
        },
      {
        view =
          Ty_view.Constr
            (Uid.{ name = "LChar.t"; view = _ }, ty_view_constr_args);
        _;
      } ) ->
      if not debug then ()
      else if List.length ty_view_constr_args <> 0 then
        failwith "Never: LChar.t should have no args"
      else ();

      let _bool_type_annot_s, bool_values =
        List.map (fun arg -> parse_term arg |> unwrap) construct_args
        |> List.split
      in

      (* Extract boolean constants from the parsed values *)
      let bools =
        List.map
          (function
            | Sir.VConst (Sir.CBool b) -> b
            | _ -> failwith "LChar.t args must be boolean constants")
          bool_values
      in

      (* Convert list of 8 bools to a char *)
      let char_val = char_of_bools bools in
      Ok (Some (TBase "char"), Sir.VConst (Sir.CChar char_val))
  (* Construct - list *)
  | ( Term.Construct
        {
          c = (_ : Type.t Applied_symbol.t_poly);
          args = (construct_args : Term.term list);
          labels = _;
        },
      {
        view =
          Ty_view.Constr (Uid.{ name = "list"; view = _ }, ty_view_constr_args);
        _;
      } ) ->
      (* For empty list, the construct arg is empty.
      For non-empty list, the construct arg has two terms, with the first term
      being the head and the second term being the tail (existing list).
    *)
      (if not debug then ()
       else if List.length ty_view_constr_args <> 1 then
         let args_str =
           CCString.concat ", " (List.map Type.show ty_view_constr_args)
         in
         let msg =
           sprintf "Never: list should have only 1 arg, got %d: %s"
             (List.length ty_view_constr_args)
             args_str
         in
         failwith msg);

      (* Get element type from ty_view_constr_args *)
      let elem_type =
        match ty_view_constr_args with
        | [ elem_ty ] ->
            let ty_expr, _params =
              type_expr_of_mir_ty_view_constr elem_ty.view
            in
            ty_expr
        | _ -> failwith "Never: list should have exactly 1 type arg"
      in
      let list_type = Sir.TApp ("list", [ elem_type ]) in

      let list_value =
        match construct_args with
        | [] ->
            (* Nil - empty list *)
            Sir.VList []
        | _ -> (
            let _type_annot_of_elems, term_of_elems =
              List.map (fun arg -> parse_term arg |> unwrap) construct_args
              |> List.split
            in
            match term_of_elems with
            | [] -> failwith "Never: empty construct arg for non-Nil"
            | [ _ ] -> failwith "Never: single element list for non-Nil"
            | [ head; tail ] -> (
                (* Cons - need to flatten the tail into a full list *)
                match tail with
                | Sir.VList tail_elems -> Sir.VList (head :: tail_elems)
                | _ ->
                    failwith
                      "Never: tail of cons list should be a VList after parsing")
            | _ -> failwith "Never: more than 2 elements list for non-Nil")
      in

      Ok (Some list_type, list_value)
  (* Construct - other *)
  | ( Term.Construct
        {
          c = (construct : Type.t Applied_symbol.t_poly);
          args = (construct_args : Term.term list);
          (* TODO: labels are for inline records; handle them *)
          labels = _;
        },
      (ty : Type.t) ) ->
      let variant_constr_name = construct.sym.id.name in

      let variant_type, _type_vars =
        type_expr_of_mir_ty_view_constr ty.view
      in

      let _constr_arg_type_annots, constr_arg_values =
        List.map (fun arg -> parse_term arg |> unwrap) construct_args
        |> List.split
      in

      let value =
        match variant_constr_name with
        (* map Option's None variant to CUnit *)
        | "None" -> Sir.VConst Sir.CUnit
        | _ -> Sir.VConstruct { constructor = variant_constr_name; args = constr_arg_values }
      in
      Ok (Some variant_type, value)
  | Term.Apply { f : Term.term; l : Term.term list }, (ty : Type.t) -> (
      try
        (* Extract Map key and value type from ty *)
        let key_ty, val_ty =
          match ty.view with
          | Ty_view.Constr
              ( { name = "Map.t"; _ },
                ([ key_ty; val_ty ] : Type.t list) ) ->
              let key_ty_expr, _ =
                type_expr_of_mir_ty_view_constr key_ty.view
              in
              let val_ty_expr, _ =
                type_expr_of_mir_ty_view_constr val_ty.view
              in
              (key_ty_expr, val_ty_expr)
          | _ -> raise (Early_return "Non-map Apply term view")
        in

        let map_type = Sir.TApp ("Map.t", [ key_ty; val_ty ]) in

        (* Parse the [l] of Map.add'
        - [Map.add' m k v] adds the pair (k, v) to map m
        - it should be exactly 3 terms
        - if the first term is `Map.const`, it contains the default value
        - if the first term is `Map.add'` again, we recursively call parsing

        Return:
          - the key-value pairs that have been parsed so far
          - the default value
       *)
        let rec parse_map_term_view
            (f : Term.term)
            (l : Term.term list)
            (accu_key_val_pairs : (Term.term * Term.term) list) :
            (Term.term * Term.term) list * Term.term =
          let key_val_pairs, default =
            match f with
            | ({
                 view =
                   Term.Sym
                     Applied_symbol.
                       { sym = { id = Uid.{ name = "Map.add'"; _ }; _ }; _ };
                 _;
               } :
                Term.term) ->
                (* f is Map.add', l should have exactly 3 terms
               - the first term, which should have view of Apply, goes to next call
               - the second and third terms go to the key_val_pairs
             *)
                let key_val_pairs, default =
                  match l with
                  | [ inner_l1; inner_l2; inner_l3 ] ->
                      let next_accu =
                        accu_key_val_pairs @ [ (inner_l2, inner_l3) ]
                      in
                      let f_, l_ =
                        match inner_l1.view with
                        | Term.Apply { f; l; _ } -> (f, l)
                        | _ ->
                            failwith
                              "Never: Map.add' first term should be Apply"
                      in
                      parse_map_term_view f_ l_ next_accu
                  | _ ->
                      let msg =
                        sprintf
                          "Never: Map.add' should have exactly 3 terms in the \
                           l, but got %d"
                          (List.length l)
                      in
                      failwith msg
                in
                (key_val_pairs, default)
            | ({
                 view =
                   Term.Sym
                     Applied_symbol.
                       { sym = { id = Uid.{ name = "Map.const"; _ }; _ }; _ };
                 _;
               } :
                Term.term) ->
                (* f is Map.const, l should have only one term, which is default *)
                let default_term =
                  match l with
                  | [ default_term ] -> default_term
                  | _ ->
                      failwith
                        "Never: Map.const should have exactly 1 term in the l"
                in
                (accu_key_val_pairs, default_term)
            | _ -> failwith "Never: Map.add' or Map.const not found"
          in
          (key_val_pairs, default)
        in
        let key_val_pairs, default = parse_map_term_view f l [] in

        let _default_val_type_annot, default_val =
          default |> parse_term |> unwrap
        in

        let key_terms, val_terms = key_val_pairs |> List.rev |> List.split in

        let _key_type_annots, key_vals =
          key_terms |> List.map (fun k -> parse_term k |> unwrap) |> List.split
        in

        let _val_type_annots, val_vals =
          val_terms |> List.map (fun k -> parse_term k |> unwrap) |> List.split
        in

        let map_value =
          Sir.VMap
            {
              default = default_val;
              entries = CCList.combine key_vals val_vals;
            }
        in

        Ok (Some map_type, map_value)
      with Early_return msg -> Error msg)
  | _, _ ->
      let msg = "case other than const, construct, or apply" in
      Error msg

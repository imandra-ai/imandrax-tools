open Common_

(** Parse MIR Decl.t to SIR type declaration

Arg:
  decl: MIR Decl.t

Return:
  Ok: SIR type declaration
  Error: string
    - Error if decl is not a Ty, i.e., it's Fun, Theorem, Rule_spec, or Verify
*)
let parse_decl (decl : (Term.t, Type.t) Decl.t_poly)
    : (Sir.type_decl, string) result =
  match decl with
  | Ty (ty_view_def : Type.t Ty_view.def_poly) ->
      let { Ty_view.name = decl_name_uid
          ; params : Uid.t list
          ; decl = ty_view_decl
          ; clique = _
          ; timeout = _
          } =
        ty_view_def
      in

      let { Uid.name = decl_name; view = _ } = decl_name_uid in
      let type_params = List.map (fun (uid : Uid.t) -> uid.name) params in

      (match ty_view_decl with
      | Algebraic (adt_rows : (Uid.t, Type.t) Ty_view.adt_row list) ->
          let constructors =
            List.map
              (fun (adt_row : (Uid.t, Type.t) Ty_view.adt_row) ->
                let Ty_view.{ c; labels; args; doc = _ } = adt_row in
                let constructor_name = c.name in

                let field_names =
                  match labels with
                  | None -> anonymous_arg_names (List.length args)
                  | Some id_list ->
                      List.map (fun (id : Uid.t) -> id.name) id_list
                in

                let field_types =
                  List.map
                    (fun (arg : Type.t) ->
                      let Type.{ view = arg_ty_view; generation = _ } = arg in
                      let ty_expr, _params =
                        type_expr_of_mir_ty_view arg_ty_view
                      in
                      ty_expr)
                    args
                in

                let (fields : Sir.Variant_field.t list) =
                  List.map2
                    (fun name ty ->
                      match labels with
                      | None -> Sir.Variant_field.Positional ty
                      | Some _ -> Sir.Variant_field.Named (name, ty))
                    field_names
                    field_types
                in

                { Sir.vc_name = constructor_name; vc_fields = fields })
              adt_rows
          in

          Ok (Sir.Variant { name = decl_name; type_params; constructors })
      | Record (rec_rows : (Uid.t, Type.t) Ty_view.rec_row list) ->
          let fields =
            List.map
              (fun (rec_row : (Uid.t, Type.t) Ty_view.rec_row) ->
                let Ty_view.{ f : Uid.t; ty : Type.t; doc = _ } = rec_row in
                let field_name = f.name in
                let Type.{ view = arg_ty_view; generation = _ } = ty in
                let ty_expr, _params =
                  type_expr_of_mir_ty_view arg_ty_view
                in

                { Sir.rf_name = field_name; rf_type = ty_expr })
              rec_rows
          in

          Ok (Sir.Record { name = decl_name; type_params; fields })
      | _ -> Error "WIP: not Algebraic and not Record")
  | Fun _ -> Error "WIP: Fun"
  | _ -> Error "parse_decl_to_semantic: expected Ty | Fun"
;;

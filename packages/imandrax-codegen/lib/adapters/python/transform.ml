(** Transform Semantic IR to Python AST *)

(* Use Python adapter's own AST definitions *)
open Ast_types
open Ast
module Sir = Semantic_ir

(** Convert SIR type_expr to Ast.type_expr (for mk_dataclass_def) *)
let rec ast_type_expr_of_sir_type_expr (te : Sir.Types.type_expr) : Ast.type_expr =
  match te with
  | Sir.Types.TBase name -> Ast.Base (Config.map_type_name name)
  | Sir.Types.TVar name -> Ast.Base name
  | Sir.Types.TApp (name, args) -> (
      let mapped_name = Config.map_type_name name in
      match args with
      | [] -> Ast.Base mapped_name
      | _ -> Ast.Generic (mapped_name, List.map ast_type_expr_of_sir_type_expr args))
  | Sir.Types.TTuple exprs ->
      Ast.Generic ("tuple", List.map ast_type_expr_of_sir_type_expr exprs)
  | Sir.Types.TArrow (arg, ret) ->
      Ast.Generic
        ( "Callable",
          [
            Ast.Generic ("list", [ ast_type_expr_of_sir_type_expr arg ]);
            ast_type_expr_of_sir_type_expr ret;
          ] )

(** Convert SIR type_expr to Python type annotation *)
let rec annot_of_sir_type_expr (te : Sir.Types.type_expr) : expr =
  match te with
  | Sir.Types.TBase name -> mk_name_expr (Config.map_type_name name)
  | Sir.Types.TVar name -> mk_name_expr name
  | Sir.Types.TApp (name, args) -> (
      let mapped_name = Config.map_type_name name in
      match args with
      | [] -> mk_name_expr mapped_name
      | [ single ] ->
          Subscript
            {
              value = mk_name_expr mapped_name;
              slice = annot_of_sir_type_expr single;
              ctx = Load;
            }
      | multiple ->
          Subscript
            {
              value = mk_name_expr mapped_name;
              slice = tuple_of_exprs (List.map annot_of_sir_type_expr multiple);
              ctx = Load;
            })
  | Sir.Types.TTuple exprs ->
      tuple_annot_of_annots (List.map annot_of_sir_type_expr exprs)
  | Sir.Types.TArrow (arg, ret) ->
      (* Callable[[arg_type], ret_type] *)
      Subscript
        {
          value = mk_name_expr "Callable";
          slice =
            tuple_of_exprs
              [
                list_of_exprs [ annot_of_sir_type_expr arg ];
                annot_of_sir_type_expr ret;
              ];
          ctx = Load;
        }

(** Generate TypeVar definitions for type parameters *)
let stmts_of_type_params (params : string list) : stmt list =
  List.map
    (fun param ->
      (* Generate: a = TypeVar('a') *)
      Assign
        {
          targets = [ mk_name_expr param ];
          value =
            Call
              {
                func = mk_name_expr "TypeVar";
                args = [ mk_string_expr param ];
                keywords = [];
              };
          type_comment = None;
        })
    params

(** Convert variant constructor to Python dataclass *)
let dataclass_decl_of_variant_constructor
    (type_params : string list)
    (constr : Sir.Types.variant_constructor) : stmt =
  let rows : (string * Ast.type_expr) list =
    List.mapi
      (fun i field ->
        match field with
        | Sir.Types.Positional ty ->
            ("arg" ^ string_of_int i, ast_type_expr_of_sir_type_expr ty)
        | Sir.Types.Named (name, ty) -> (name, ast_type_expr_of_sir_type_expr ty))
      constr.vc_fields
  in
  mk_dataclass_def constr.vc_name type_params rows

(** Convert SIR type_decl to Python statements *)
let stmts_of_sir_type_decl (decl : Sir.Types.type_decl) : stmt list =
  match decl with
  | Sir.Types.Variant { name; type_params; constructors } ->
      let type_var_stmts = stmts_of_type_params type_params in
      let constructor_defs =
        List.map (dataclass_decl_of_variant_constructor type_params) constructors
      in
      let union_type_expr =
        List.map
          (fun (c : Sir.Types.variant_constructor) -> mk_name_expr c.vc_name)
          constructors
      in
      let union_def = mk_union_def name union_type_expr in
      type_var_stmts @ constructor_defs @ [ union_def ]
  | Sir.Types.Record { name; type_params; fields } ->
      let type_var_stmts = stmts_of_type_params type_params in
      let rows : (string * Ast.type_expr) list =
        List.map
          (fun (rf : Sir.Types.record_field) ->
            (rf.rf_name, ast_type_expr_of_sir_type_expr rf.rf_type))
          fields
      in
      let record_def = mk_dataclass_def name type_params rows in
      type_var_stmts @ [ record_def ]
  | Sir.Types.Alias { name; type_params = _; target } ->
      (* Generate: Name = target_type *)
      [
        Assign
          {
            targets = [ mk_name_expr name ];
            value = annot_of_sir_type_expr target;
            type_comment = None;
          };
      ]

(** Convert SIR constant to Python constant *)
let ast_const_of_sir_const (c : Sir.Types.const_value) : constant_value =
  match c with
  | Sir.Types.CInt i -> Int i
  | Sir.Types.CFloat f -> Float f
  | Sir.Types.CBool b -> Bool b
  | Sir.Types.CString s -> String s
  | Sir.Types.CChar ch -> String (String.make 1 ch)
  | Sir.Types.CUnit -> Unit

(** Convert SIR value to Python expression *)
let rec ast_expr_of_sir_value (v : Sir.Types.value) : expr =
  match v with
  | Sir.Types.VConst c ->
      Constant { value = ast_const_of_sir_const c; kind = None }
  | Sir.Types.VTuple vs -> tuple_of_exprs (List.map ast_expr_of_sir_value vs)
  | Sir.Types.VList vs -> list_of_exprs (List.map ast_expr_of_sir_value vs)
  | Sir.Types.VRecord { type_name; fields } ->
      let kwargs =
        List.map (fun (name, value) -> (name, ast_expr_of_sir_value value)) fields
      in
      mk_dataclass_value type_name ~args:[] ~kwargs
  | Sir.Types.VConstruct { constructor; args } ->
      let arg_exprs = List.map ast_expr_of_sir_value args in
      mk_dataclass_value constructor ~args:arg_exprs ~kwargs:[]
  | Sir.Types.VName name -> mk_name_expr name
  | Sir.Types.VBinOp (left, op, right) -> (
      let left_expr = ast_expr_of_sir_value left in
      let right_expr = ast_expr_of_sir_value right in
      match op with
      | Sir.Types.Eq | Sir.Types.Lt | Sir.Types.Gt ->
          Compare
            {
              left = left_expr;
              ops = [ Config.map_cmp_op op ];
              comparators = [ right_expr ];
            }
      | Sir.Types.And | Sir.Types.Or ->
          (* TODO: BoolOp type in ast_types.ml is incomplete - it doesn't store operands *)
          failwith
            "Boolean operations (And/Or) are not yet supported in the AST"
      | Sir.Types.Add | Sir.Types.Sub | Sir.Types.Mult | Sir.Types.Div ->
          BinOp
            { left = left_expr; op = Config.map_bin_op op; right = right_expr })
  | Sir.Types.VIfThenElse (cond, then_val, else_val) ->
      IfExp (ast_expr_of_sir_value cond, ast_expr_of_sir_value then_val, ast_expr_of_sir_value else_val)
  | Sir.Types.VMap { default; entries } ->
      let key_val_pairs =
        List.map (fun (k, v) -> (ast_expr_of_sir_value k, ast_expr_of_sir_value v)) entries
      in
      mk_defaultdict_value (ast_expr_of_sir_value default) key_val_pairs

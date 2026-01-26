(** Transform Semantic IR to Python AST *)

(* Use Python adapter's own AST definitions *)
open Ast_types
open Ast
module Sir = Semantic_ir

(** Convert SIR type_expr to Ast.type_expr (for mk_dataclass_def) *)
let rec semantic_type_to_ast_type (te : Sir.Types.type_expr) : Ast.type_expr =
  match te with
  | Sir.Types.TBase name -> Ast.Base (Config.map_type_name name)
  | Sir.Types.TVar name -> Ast.Base name
  | Sir.Types.TApp (name, args) ->
      let mapped_name = Config.map_type_name name in
      (match args with
       | [] -> Ast.Base mapped_name
       | _ -> Ast.Generic (mapped_name, List.map semantic_type_to_ast_type args))
  | Sir.Types.TTuple exprs ->
      Ast.Generic ("tuple", List.map semantic_type_to_ast_type exprs)
  | Sir.Types.TArrow (arg, ret) ->
      Ast.Generic ("Callable", [
        Ast.Generic ("list", [semantic_type_to_ast_type arg]);
        semantic_type_to_ast_type ret
      ])

(** Convert SIR type_expr to Python type annotation *)
let rec type_expr_to_annotation (te : Sir.Types.type_expr) : expr =
  match te with
  | Sir.Types.TBase name -> mk_name_expr (Config.map_type_name name)
  | Sir.Types.TVar name -> mk_name_expr name
  | Sir.Types.TApp (name, args) ->
      let mapped_name = Config.map_type_name name in
      (match args with
       | [] -> mk_name_expr mapped_name
       | [single] ->
           Subscript {
             value = mk_name_expr mapped_name;
             slice = type_expr_to_annotation single;
             ctx = Load;
           }
       | multiple ->
           Subscript {
             value = mk_name_expr mapped_name;
             slice = tuple_of_exprs (List.map type_expr_to_annotation multiple);
             ctx = Load;
           })
  | Sir.Types.TTuple exprs ->
      tuple_annot_of_annots (List.map type_expr_to_annotation exprs)
  | Sir.Types.TArrow (arg, ret) ->
      (* Callable[[arg_type], ret_type] *)
      Subscript {
        value = mk_name_expr "Callable";
        slice = tuple_of_exprs [
          list_of_exprs [type_expr_to_annotation arg];
          type_expr_to_annotation ret;
        ];
        ctx = Load;
      }

(** Generate TypeVar definitions for type parameters *)
let type_params_to_stmts (params : string list) : stmt list =
  List.map (fun param ->
    (* Generate: a = TypeVar('a') *)
    Assign {
      targets = [mk_name_expr param];
      value = Call {
        func = mk_name_expr "TypeVar";
        args = [mk_string_expr param];
        keywords = [];
      };
      type_comment = None;
    }
  ) params

(** Convert variant constructor to Python dataclass *)
let variant_constructor_to_dataclass
    (type_params : string list)
    (constr : Sir.Types.variant_constructor) : stmt =
  let rows : (string * Ast.type_expr) list =
    List.mapi (fun i field ->
      match field with
      | Sir.Types.Positional ty -> ("arg" ^ string_of_int i, semantic_type_to_ast_type ty)
      | Sir.Types.Named (name, ty) -> (name, semantic_type_to_ast_type ty)
    ) constr.vc_fields
  in
  mk_dataclass_def constr.vc_name type_params rows

(** Convert SIR type_decl to Python statements *)
let type_decl_to_stmts (decl : Sir.Types.type_decl) : stmt list =
  match decl with
  | Sir.Types.Variant { name; type_params; constructors } ->
      let type_var_stmts = type_params_to_stmts type_params in
      let constructor_defs =
        List.map (variant_constructor_to_dataclass type_params) constructors
      in
      let union_type_expr =
        List.map (fun (c : Sir.Types.variant_constructor) -> mk_name_expr c.vc_name) constructors
      in
      let union_def = mk_union_def name union_type_expr in
      type_var_stmts @ constructor_defs @ [union_def]

  | Sir.Types.Record { name; type_params; fields } ->
      let type_var_stmts = type_params_to_stmts type_params in
      let rows : (string * Ast.type_expr) list =
        List.map (fun (rf : Sir.Types.record_field) ->
          (rf.rf_name, semantic_type_to_ast_type rf.rf_type)
        ) fields
      in
      let record_def = mk_dataclass_def name type_params rows in
      type_var_stmts @ [record_def]

  | Sir.Types.Alias { name; type_params = _; target } ->
      (* Generate: Name = target_type *)
      [Assign {
        targets = [mk_name_expr name];
        value = type_expr_to_annotation target;
        type_comment = None;
      }]

(** Convert SIR constant to Python constant *)
let const_to_python_value (c : Sir.Types.const_value) : constant_value =
  match c with
  | Sir.Types.CInt i -> Int i
  | Sir.Types.CFloat f -> Float f
  | Sir.Types.CBool b -> Bool b
  | Sir.Types.CString s -> String s
  | Sir.Types.CChar ch -> String (String.make 1 ch)
  | Sir.Types.CUnit -> Unit

(** Convert SIR value to Python expression *)
let rec value_to_expr (v : Sir.Types.value) : expr =
  match v with
  | Sir.Types.VConst c -> Constant { value = const_to_python_value c; kind = None }

  | Sir.Types.VTuple vs -> tuple_of_exprs (List.map value_to_expr vs)

  | Sir.Types.VList vs -> list_of_exprs (List.map value_to_expr vs)

  | Sir.Types.VRecord { type_name; fields } ->
      let kwargs = List.map (fun (name, value) ->
        (name, value_to_expr value)
      ) fields in
      mk_dataclass_value type_name ~args:[] ~kwargs

  | Sir.Types.VConstruct { constructor; args } ->
      let arg_exprs = List.map value_to_expr args in
      mk_dataclass_value constructor ~args:arg_exprs ~kwargs:[]

  | Sir.Types.VName name -> mk_name_expr name

  | Sir.Types.VBinOp (left, op, right) ->
      let left_expr = value_to_expr left in
      let right_expr = value_to_expr right in
      (match op with
       | Sir.Types.Eq | Sir.Types.Lt | Sir.Types.Gt ->
           Compare {
             left = left_expr;
             ops = [Config.map_cmp_op op];
             comparators = [right_expr];
           }
       | Sir.Types.And | Sir.Types.Or ->
           (* TODO: BoolOp type in ast_types.ml is incomplete - it doesn't store operands *)
           failwith "Boolean operations (And/Or) are not yet supported in the AST"
       | Sir.Types.Add | Sir.Types.Sub | Sir.Types.Mult | Sir.Types.Div ->
           BinOp {
             left = left_expr;
             op = Config.map_bin_op op;
             right = right_expr;
           })

  | Sir.Types.VIfThenElse (cond, then_val, else_val) ->
      IfExp (value_to_expr cond, value_to_expr then_val, value_to_expr else_val)

  | Sir.Types.VMap { default; entries } ->
      let key_val_pairs = List.map (fun (k, v) ->
        (value_to_expr k, value_to_expr v)
      ) entries in
      mk_defaultdict_value (value_to_expr default) key_val_pairs

(** Transform Semantic IR to Python AST *)

(* Use Python adapter's own AST definitions *)
open Ast
module Sir = Semantic_ir

(** Convert SIR type_expr to Ast.type_expr (for mk_dataclass_def) *)
let rec ast_type_expr_of_sir (te : Sir.type_expr) : Ast.type_expr =
  match te with
  | Sir.TBase name -> Ast.Base (Config.map_type_name name)
  | Sir.TVar name -> Ast.Base name
  | Sir.TApp (name, args) ->
      let mapped_name = Config.map_type_name name in
      (match args with
      | [] -> Ast.Base mapped_name
      | _ -> Ast.Generic (mapped_name, List.map ast_type_expr_of_sir args))
  | Sir.TTuple exprs ->
      Ast.Generic ("tuple", List.map ast_type_expr_of_sir exprs)
  | Sir.TArrow (arg, ret) ->
      Ast.Generic
        ( "Callable"
        , [ Ast.Generic ("list", [ ast_type_expr_of_sir arg ])
          ; ast_type_expr_of_sir ret
          ] )
;;

(** Convert SIR type_expr to Python type annotation *)
let rec annot_of_sir_type_expr (te : Sir.type_expr) : expr =
  match te with
  | Sir.TBase name -> mk_name_expr (Config.map_type_name name)
  | Sir.TVar name -> mk_name_expr name
  | Sir.TApp (name, args) ->
      let mapped_name = Config.map_type_name name in
      (match args with
      | [] -> mk_name_expr mapped_name
      | [ single ] ->
          Subscript
            { value = mk_name_expr mapped_name
            ; slice = annot_of_sir_type_expr single
            ; ctx = Load
            }
      | multiple ->
          Subscript
            { value = mk_name_expr mapped_name
            ; slice = tuple_of_exprs (List.map annot_of_sir_type_expr multiple)
            ; ctx = Load
            })
  | Sir.TTuple exprs ->
      tuple_annot_of_annots (List.map annot_of_sir_type_expr exprs)
  | Sir.TArrow (arg, ret) ->
      (* Callable[[arg_type], ret_type] *)
      Subscript
        { value = mk_name_expr "Callable"
        ; slice =
            tuple_of_exprs
              [ list_of_exprs [ annot_of_sir_type_expr arg ]
              ; annot_of_sir_type_expr ret
              ]
        ; ctx = Load
        }
;;

(** Generate TypeVar definitions for type parameters *)
let stmts_of_type_params (params : string list) : stmt list =
  List.map
    (fun param ->
      (* Generate: a = TypeVar('a') *)
      Assign
        { targets = [ mk_name_expr param ]
        ; value =
            Call
              { func = mk_name_expr "TypeVar"
              ; args = [ mk_string_expr param ]
              ; keywords = []
              }
        ; type_comment = None
        })
    params
;;

(** Convert variant constructor to Python dataclass *)
let dataclass_decl_of_variant_constructor
    (type_params : string list)
    (constr : Sir.variant_constructor)
    : stmt =
  let rows : (string * Ast.type_expr) list =
    List.mapi
      (fun i field ->
        match field with
        | Sir.Variant_field.Positional ty ->
            "arg" ^ string_of_int i, ast_type_expr_of_sir ty
        | Sir.Variant_field.Named (name, ty) -> name, ast_type_expr_of_sir ty)
      constr.vc_fields
  in
  mk_dataclass_def constr.vc_name type_params rows
;;

(** Convert SIR type_decl to Python statements *)
let stmts_of_sir_type_decl (decl : Sir.type_decl) : stmt list =
  match decl with
  | Sir.Variant { name; type_params; constructors } ->
      let (type_var_stmts : stmt list) = stmts_of_type_params type_params in
      let extract_field_type_params (vc : Sir.variant_constructor) : string list
          =
        vc.Sir.vc_fields
        |> List.map (fun (v_field : Sir.Variant_field.t) ->
               let field_type_expr = Sir.Variant_field.type_expr v_field in
               Sir.type_var_names_of_type_expr field_type_expr)
        |> CCList.concat
        |> CCList.uniq ~eq:CCString.equal
      in
      (* Map: constructor name -> unique type params *)
      let vc_type_name_to_params_map : (string * string list) list =
        constructors
        |> List.map (fun (vc : Sir.variant_constructor) ->
               let vc_type_name = vc.Sir.vc_name in
               let vc_field_type_params = extract_field_type_params vc in
               vc_type_name, vc_field_type_params)
      in
      let (constructor_defs : stmt list) =
        (constructors : Sir.variant_constructor list)
        |> List.map (fun vc ->
               let vc_type_params =
                 List.assoc vc.Sir.vc_name vc_type_name_to_params_map
               in
               dataclass_decl_of_variant_constructor vc_type_params vc)
      in
      let union_type_expr =
        (constructors : Sir.variant_constructor list)
        |> List.map (fun (vc : Sir.variant_constructor) ->
               let vc_type_params =
                 List.assoc vc.Sir.vc_name vc_type_name_to_params_map
               in
               mk_generic_type_annot vc.Sir.vc_name vc_type_params)
      in
      let union_def = mk_union_def name union_type_expr in
      type_var_stmts @ constructor_defs @ [ union_def ]
  | Sir.Record { name; type_params; fields } ->
      let type_var_stmts = stmts_of_type_params type_params in
      let rows : (string * Ast.type_expr) list =
        List.map
          (fun (rf : Sir.record_field) ->
            rf.rf_name, ast_type_expr_of_sir rf.rf_type)
          fields
      in
      let record_def = mk_dataclass_def name type_params rows in
      type_var_stmts @ [ record_def ]
  | Sir.Alias { name; type_params = _; target } ->
      (* Generate: Name = target_type *)
      [ Assign
          { targets = [ mk_name_expr name ]
          ; value = annot_of_sir_type_expr target
          ; type_comment = None
          }
      ]
;;

(** Convert SIR constant to Python constant *)
let ast_const_of_sir_const (c : Sir.const_value) : constant_value =
  match c with
  | Sir.CInt i -> Int i
  | Sir.CFloat f -> Float f
  | Sir.CBool b -> Bool b
  | Sir.CString s -> String s
  | Sir.CChar ch -> String (String.make 1 ch)
  | Sir.CUnit -> Unit
;;

(** Convert SIR value to Python expression *)
let rec ast_expr_of_sir_value (v : Sir.value) : expr =
  match v with
  | Sir.VConst c -> Constant { value = ast_const_of_sir_const c; kind = None }
  | Sir.VTuple vs -> tuple_of_exprs (List.map ast_expr_of_sir_value vs)
  | Sir.VList vs -> list_of_exprs (List.map ast_expr_of_sir_value vs)
  | Sir.VRecord { type_name; fields } ->
      let kwargs =
        List.map (fun (name, value) -> name, ast_expr_of_sir_value value) fields
      in
      mk_dataclass_value type_name ~args:[] ~kwargs
  | Sir.VConstruct { constructor; args } ->
      let arg_exprs = List.map ast_expr_of_sir_value args in
      mk_dataclass_value constructor ~args:arg_exprs ~kwargs:[]
  | Sir.VName name -> mk_name_expr name
  | Sir.VBinOp (left, op, right) ->
      let left_expr = ast_expr_of_sir_value left in
      let right_expr = ast_expr_of_sir_value right in
      (match op with
      | Sir.Eq | Sir.Lt | Sir.Gt ->
          Compare
            { left = left_expr
            ; ops = [ Config.map_cmp_op op ]
            ; comparators = [ right_expr ]
            }
      | Sir.And | Sir.Or ->
          (* TODO: BoolOp type in ast_types.ml is incomplete - it doesn't store operands *)
          failwith
            "Boolean operations (And/Or) are not yet supported in the AST"
      | Sir.Add | Sir.Sub | Sir.Mult | Sir.Div ->
          BinOp
            { left = left_expr; op = Config.map_bin_op op; right = right_expr })
  | Sir.VIfThenElse (cond, then_val, else_val) ->
      IfExp
        ( ast_expr_of_sir_value cond
        , ast_expr_of_sir_value then_val
        , ast_expr_of_sir_value else_val )
  | Sir.VMap { default; entries } ->
      let key_val_pairs =
        List.map
          (fun (k, v) -> ast_expr_of_sir_value k, ast_expr_of_sir_value v)
          entries
      in
      mk_defaultdict_value (ast_expr_of_sir_value default) key_val_pairs
;;

let ast_stmts_of_sir_value_assignment (value_assign : Sir.Value_assignment.t)
    : stmt list =
  let type_params = Sir.Value_assignment.type_var value_assign in
  let type_param_defs = stmts_of_type_params type_params in
  let type_annot : Ast.expr = value_assign.ty |> annot_of_sir_type_expr in
  let term_expr : Ast.expr = value_assign.tm |> ast_expr_of_sir_value in

  let assign_stmt =
    let target = value_assign.var_name in
    Ast.AnnAssign
      { target = Ast.Name { Ast.id = target; ctx = Ast.Load }
      ; annotation = type_annot
      ; value = Some term_expr
      ; simple = 1
      }
  in
  type_param_defs @ [ assign_stmt ]
;;

(*
Create a test function definition statement

Args:
  test_decl:
    - test_name: The name of the test function
    - f_name: The name of the function to be tested
    - docstr: The docstring of the test function
    - f_args: The arguments of the function to be tested
    - output_type_annot: The type annotation of the output of the function to be tested
    - expected: The expected value of the output of the function to be tested

Example:
```python
def name():
    """docstr"""
    result = f(x)
    expected = ...
    assert result == expected
```
*)
let test_func_def_of_test_decl (test_decl : Sir.test_decl) : stmt =
  let ( (test_name : string)
      , (f_name : string)
      , (docstr : string)
      , (f_args : (string * expr * expr) list)
      , (output_type_annot : expr)
      , (expected : expr) ) =
    let f_args =
      test_decl.f_args
      |> List.map (fun (name, ty, tm) ->
             name, annot_of_sir_type_expr ty, ast_expr_of_sir_value tm)
    in
    let output_type_annot, expected = test_decl.f_output in
    ( test_decl.name
    , test_decl.f_name
    , test_decl.docstr
    , f_args
    , output_type_annot |> annot_of_sir_type_expr
    , expected |> ast_expr_of_sir_value )
  in
  let call_keywords : keyword list =
    List.map (fun (k, _, v) -> { arg = Some k; value = v }) f_args
  in
  (* `f(x)` *)
  let call : expr =
    Call
      { func = Name { id = f_name; ctx = mk_ctx () }
      ; args = []
      ; keywords = call_keywords
      }
  in
  (* `result = f(x)` *)
  let assign_call_result : stmt =
    mk_assign
      (Name { id = "result"; ctx = mk_ctx () })
      (Some output_type_annot)
      call
  in
  (* `expected = ...` *)
  let assign_expected : stmt =
    mk_assign
      (Name { id = "expected"; ctx = mk_ctx () })
      (Some output_type_annot)
      expected
  in

  (* `assert result == expected` *)
  let assert_eq : stmt =
    mk_assert_eq
      (Name { id = "result"; ctx = mk_ctx () })
      (Name { id = "expected"; ctx = mk_ctx () })
  in

  let func_body =
    [ ExprStmt { value = Constant { value = String docstr; kind = None } }
    ; assign_call_result
    ; assign_expected
    ; assert_eq
    ]
  in
  FunctionDef
    { name = test_name
    ; args = empty_arguments ()
    ; body = func_body
    ; decorator_list = []
    ; returns = None
    ; type_comment = None
    ; type_params = []
    }
;;

let test_data_dict_of_test_decl (test_decl : Sir.test_decl) : expr =
  let (args : (string * expr) list) =
    test_decl.f_args
    |> List.map (fun (name, _ty, tm) -> name, ast_expr_of_sir_value tm)
  in
  let (expected : expr) = test_decl.f_output |> snd |> ast_expr_of_sir_value in
  let input_kwargs_dict : expr =
    let keys, values = List.split args in
    let key_opt_exprs =
      keys
      |> List.map (fun k -> Some (Constant { value = String k; kind = None }))
    in
    Dict { keys = key_opt_exprs; values }
  in
  Dict
    { keys =
        [ Some (Constant { value = String "input_kwargs"; kind = None })
        ; Some (Constant { value = String "expected"; kind = None })
        ]
    ; values = [ input_kwargs_dict; expected ]
    }
;;

(** Parse SIR test suite to an assignment statement of a test data dictionary *)
let test_data_dict_of_test_suite (test_suite : Sir.test_suite) : stmt =
  let (test_names : string list) =
    test_suite |> List.map (fun (test_decl : Sir.test_decl) -> test_decl.name)
  in

  let (test_data_dict_items : expr list) =
    test_suite |> List.map test_data_dict_of_test_decl
  in
  let agg_dict =
    Dict
      { keys =
          List.map
            (fun test_name ->
              Some (Constant { value = String test_name; kind = None }))
            test_names
      ; values = test_data_dict_items
      }
  in
  let agg_dict_type_annot =
    Subscript
      { value = Name { id = "dict"; ctx = mk_ctx () }
      ; slice =
          Tuple
            { elts =
                [ Name { id = "str"; ctx = mk_ctx () }
                ; Subscript
                    { value = Name { id = "dict"; ctx = mk_ctx () }
                    ; slice =
                        Tuple
                          { elts =
                              [ Name { id = "str"; ctx = mk_ctx () }
                              ; Name { id = "Any"; ctx = mk_ctx () }
                              ]
                          ; ctx = mk_ctx ()
                          ; dims = []
                          }
                    ; ctx = mk_ctx ()
                    }
                ]
            ; ctx = mk_ctx ()
            ; dims = []
            }
      ; ctx = mk_ctx ()
      }
  in
  AnnAssign
    { target = Name { id = "tests"; ctx = mk_ctx () }
    ; annotation = agg_dict_type_annot
    ; value = Some agg_dict
    ; simple = 1
    }
;;

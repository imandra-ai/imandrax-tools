include Ast_types

(* Create a list of anonymous argument names *)
let anonymous_arg_names (i : int) : string list =
  List.init i (fun i -> "arg" ^ string_of_int i)

let%expect_test "anonymous_arg_names" =
  let names = anonymous_arg_names 3 in
  List.iter (Printf.printf "%s ") names;
  [%expect {| arg0 arg1 arg2 |}]

(* Constructor helpers
==================== *)

(* Placeholder for ctx *)
let mk_ctx () = Load

(*
simple: an integer flag that indicates whether the assignment target is a "simple" name or not.
Specifically:
- simple = 1 when the target is just a Name (e.g., x: int = 5)
- simple = 0 when the target is an Attribute or Subscript (e.g., obj.x: int = 5 or list[0]: int = 5)
*)
let mk_ann_assign_simple_flat () = 1
let mk_bool_expr (b : bool) : expr = Constant { value = Bool b; kind = None }

let mk_string_expr (s : string) : expr =
  Constant { value = String s; kind = None }

let mk_name_expr (id : string) : expr = Name { id; ctx = mk_ctx () }

(* Convert 8-bit bool list to a char *)
let bools_to_char (bools : bool list) : char =
  if List.length bools <> 8 then
    invalid_arg "bools_to_char: list must contain exactly 8 booleans"
  else
    let rec bools_to_int acc = function
      | [] -> acc
      | b :: rest ->
          let bit = if b then 1 else 0 in
          bools_to_int ((acc lsl 1) lor bit) rest
    in
    let ascii_value = bools_to_int 0 bools in
    Char.chr ascii_value

(* Convert a char to a list of bools *)
let bools_of_char (c : char) : bool list =
  let ascii_value = Char.code c in
  let rec int_to_bools acc n bit_pos =
    if bit_pos < 0 then acc
    else
      let bit = (n lsr bit_pos) land 1 in
      int_to_bools ((bit = 1) :: acc) n (bit_pos - 1)
  in
  int_to_bools [] ascii_value 7

(* Convert a list of expressions of bools to a char expression *)
let char_expr_of_bool_list_expr (exprs : expr list) : expr =
  let bools =
    List.map
      (function
        | Constant { value = Bool b; _ } -> b
        | _ -> invalid_arg "bool_list_expr_to_string: expected bool constant")
      exprs
  in
  let char = bools_to_char bools in
  mk_string_expr (String.make 1 char)

(* Convert a list of expressions to a tuple expression *)
let tuple_of_exprs (exprs : expr list) : expr =
  Tuple { elts = exprs; ctx = Load; dims = [] }

(* Create a tuple type annotation from a list of type annotation

Example:
  - `(int, str)` -> `tuple[int, str]`
*)
let tuple_annot_of_annots (annots : expr list) : expr =
  Subscript
    {
      value = Name { id = "tuple"; ctx = mk_ctx () };
      slice = tuple_of_exprs annots;
      ctx = mk_ctx ();
    }

(* Empty list expression: `[]` *)
let empty_list_expr () : expr = List { elts = []; ctx = mk_ctx () }

(* Create a list expression from a list of expressions

Example: `1`, `2`, `3` -> `[1, 2, 3]`
*)
let list_of_exprs (exprs : expr list) : expr =
  List { elts = exprs; ctx = mk_ctx () }

(* Create a list expresion from a singleton expression head and a list expression (tail)

Example: `1`, `[2, 3]` -> `[1, 2, 3]`
*)
let cons_list_expr (head : expr) (tail : expr) : expr =
  match tail with
  | List { elts; _ } -> List { elts = head :: elts; ctx = mk_ctx () }
  | _ -> invalid_arg "cons_list_expr: tail is not a list expr"

let empty_arguments () : arguments =
  {
    posonlyargs = [];
    args = [];
    vararg = None;
    kwonlyargs = [];
    kw_defaults = [];
    kwarg = None;
    defaults = [];
  }

(* Constructor APIs
==================== *)

(* Type view constructor name to Python type name *)
let ty_view_constr_name_mapping : (string * string) list =
  [ ("int", "int"); ("bool", "bool"); ("string", "str"); ("real", "float") ]

(* Create an assign statement from a target (LHS), an optional type annotation, and a value (RHS)

Example:
- `x`, `int`, `5` -> `x: int = 5`
*)
let mk_assign (target : expr) (type_annotation : expr option) (value : expr) :
    stmt =
  match type_annotation with
  | None -> Assign { targets = [ target ]; value; type_comment = None }
  | Some type_annotation ->
      AnnAssign
        {
          target;
          annotation = type_annotation;
          value = Some value;
          simple = mk_ann_assign_simple_flat ();
        }

(** Create type annotation from a dataclass row type

    Case:
    - Basic: for single type name, it's simply a Name ast node
    - Generic: for generic type, it's a chained Subscript ast node

    Example:
    - Basic: `int` -> `int`
    - Generic: `int, Optional, list` -> list[Optional[int]] *)
let type_annot_of_chained_generic_types (type_names : string list) : expr =
  match type_names with
  | [] ->
      invalid_arg "type_annot_of_chained_generic_types: empty type name list"
  | fst :: rest ->
      let base = Name { id = fst; ctx = mk_ctx () } in
      List.fold_left
        (fun (acc : expr) (next : string) ->
          Subscript
            {
              value = acc;
              slice = Name { id = next; ctx = mk_ctx () };
              ctx = mk_ctx ();
            })
        base rest

(** Create a dataclass definition statement from its name and rows of fields

Args:
  - name: The name of the dataclass
  - rows :: (string * string) list
    - (variable_name, type_name) pairs

Example:
- `Foo`, `x: int`, `y: str`: |
  ```
  @dataclass
  class Foo:
    x: int
    y: str
  ```
*)
let mk_dataclass_def (name : string) (rows : (string * string list) list) : stmt =
  let body : stmt list =
    match rows with
    | [] -> [ Pass ]
    | _ ->
        List.map
          (fun ((tgt, row_types) : string * string list) ->
            AnnAssign
              {
                target = Name { id = tgt; ctx = mk_ctx () };
                annotation = type_annot_of_chained_generic_types row_types;
                value = None;
                simple = mk_ann_assign_simple_flat ();
              })
          rows
  in
  ClassDef
    {
      name;
      bases = [];
      keywords = [];
      body;
      decorator_list = [ Name { id = "dataclass"; ctx = mk_ctx () } ];
    }

(* Initiate a dataclass instance from its name and arguments (both pos and kw)

Example: `Foo`, `5`, `y='hello'` -> `Foo(5, y='hello')`
*)
let mk_dataclass_value
    (dataclass_name : string)
    ~(args : expr list)
    ~(kwargs : (string * expr) list) : expr =
  let keywords : keyword list =
    List.map (fun (k, v) -> { arg = Some k; value = v }) kwargs
  in
  Call { func = Name { id = dataclass_name; ctx = mk_ctx () }; args; keywords }

(* Create an union definition statement from a list of member names

Example:
- `Status`, `str`, `int` -> `Status = str | int`
*)
let mk_union_def (name : string) (union_names : string list) : stmt =
  let left_targets = [ Name { id = name; ctx = mk_ctx () } ] in
  let right_value =
    match union_names with
    | [] -> invalid_arg "def_union: empty union"
    | [ single ] -> Name { id = single; ctx = mk_ctx () }
    | _ ->
        let component_exprs : expr list =
          List.map
            (fun component_name ->
              Name { id = component_name; ctx = mk_ctx () })
            union_names
        in
        let rec mk_union (components : expr list) : expr =
          match components with
          | [] | [ _ ] -> invalid_arg "mk_union: need at least 2 elements"
          | [ left; right ] -> BinOp { left; op = BitOr; right }
          | left :: right :: rest ->
              let merged_left_and_right = BinOp { left; op = BitOr; right } in
              mk_union (merged_left_and_right :: rest)
        in
        mk_union component_exprs
  in
  Assign { targets = left_targets; value = right_value; type_comment = None }

(* AST for defining types corresponding to variant
  - Each variant constructor is a dataclass with anonymous fields
  - The variant is a union of the dataclasses

  Args:
    - variants: A list of variant constructor name, and types of its arguments

  TODO: this should be deprecated
*)
let variant_dataclass (name : string) (variants : (string * string list) list) :
    stmt list =
  let variant_names = List.map fst variants in
  (* Define a single variant constructor as a dataclass *)
  let def_variant_constructor_as_dataclass (variant : string * string list) :
      stmt =
    let name = fst variant in
    let rows : (string * string list) list =
      List.mapi
        (fun i type_name -> ("arg" ^ string_of_int i, [type_name]))
        (snd variant)
    in
    mk_dataclass_def name rows
  in
  let constructor_defs =
    List.map def_variant_constructor_as_dataclass variants
  in
  constructor_defs @ [ mk_union_def name variant_names ]

(* Create a defaultdict type annotation from its key and value types *)
let mk_defaultdict_type_annotation (key_type : string) (value_type : string) :
    expr =
  Subscript
    {
      value = Name { id = "defaultdict"; ctx = mk_ctx () };
      slice =
        tuple_of_exprs
          [
            Name { id = key_type; ctx = mk_ctx () };
            Name { id = value_type; ctx = mk_ctx () };
          ];
      ctx = mk_ctx ();
    }

(* Initiate a defaultdict instance *)
let mk_defaultdict_value
    (default_value : expr)
    (key_val_pairs : (expr * expr) list) : expr =
  let mk_no_arg_lambda ret : expr =
    Lambda { args = empty_arguments (); body = ret }
  in

  let mk_dict (key_val_pairs : (expr * expr) list) : expr =
    let keys_, values = CCList.split key_val_pairs in
    let keys = keys_ |> List.map (fun k -> Some k) in
    Dict { keys; values }
  in

  Call
    {
      func = Name { id = "defaultdict"; ctx = mk_ctx () };
      args =
        (if List.length key_val_pairs = 0 then
           [ mk_no_arg_lambda default_value ]
         else [ mk_no_arg_lambda default_value; mk_dict key_val_pairs ]);
      keywords = [];
    }

(* Test function related constructors
-------------------- *)

(* Create an assert statement from a left and right expressions *)
let mk_assert_eq (left : expr) (right : expr) : stmt =
  Assert
    {
      test = Compare { left; ops = [ Eq ]; comparators = [ right ] };
      msg = None;
    }

(*
Create a test function definition statement

Args:
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
let mk_test_function_def
    ~(test_name : string)
    ~(f_name : string)
    ~(docstr : string option)
    ~(f_args : (string * expr) list)
    ~(output_type_annot : expr option)
    ~(expected : expr) : stmt =
  let call_keywords : keyword list =
    List.map (fun (k, v) -> { arg = Some k; value = v }) f_args
  in
  (* `f(x)` *)
  let call : expr =
    Call
      {
        func = Name { id = f_name; ctx = mk_ctx () };
        args = [];
        keywords = call_keywords;
      }
  in
  (* `result = f(x)` *)
  let assign_call_result : stmt =
    mk_assign (Name { id = "result"; ctx = mk_ctx () }) output_type_annot call
  in
  (* `expected = ...` *)
  let assign_expected : stmt =
    mk_assign
      (Name { id = "expected"; ctx = mk_ctx () })
      output_type_annot expected
  in

  (* `assert result == expected` *)
  let assert_eq : stmt =
    mk_assert_eq
      (Name { id = "result"; ctx = mk_ctx () })
      (Name { id = "expected"; ctx = mk_ctx () })
  in

  let func_body =
    match docstr with
    | None -> [ assign_call_result; assign_expected; assert_eq ]
    | Some docstr ->
        [
          ExprStmt { value = Constant { value = String docstr; kind = None } };
          assign_call_result;
          assign_expected;
          assert_eq;
        ]
  in
  FunctionDef
    {
      name = test_name;
      args = empty_arguments ();
      body = func_body;
      decorator_list = [];
      returns = None;
      type_comment = None;
      type_params = [];
    }

(*
Create a test data dictionary item (one test case) from its arguments and expected value

```python
{
    'input_kwargs': {'x': 4},
    'expected': 5,
}
```
*)
let mk_test_data_dict_item (args : (string * expr) list) (expected : expr) :
    expr =
  let input_kwargs_dict : expr =
    let keys, values = List.split args in
    let key_opt_exprs =
      keys
      |> List.map (fun k -> Some (Constant { value = String k; kind = None }))
    in
    Dict { keys = key_opt_exprs; values }
  in
  Dict
    {
      keys =
        [
          Some (Constant { value = String "input_kwargs"; kind = None });
          Some (Constant { value = String "expected"; kind = None });
        ];
      values = [ input_kwargs_dict; expected ];
    }

(*
Create a test data dictionary (multiple test cases)
```python
tests: dict[str, dict[str, Any]] = {
  "test_1": {
    'input_kwargs': {'x': 4},
    'expected': 5,
  }
}
```
*)
let mk_test_data_dict
    ~(test_names : string list)
    ~(f_args_list : (string * expr) list list)
    ~(expected_list : expr list) : stmt =
  let (test_data_dict_items : expr list) =
    List.map
      (fun (args, expected) -> mk_test_data_dict_item args expected)
      (List.combine f_args_list expected_list)
  in
  let agg_dict =
    Dict
      {
        keys =
          List.map
            (fun test_name ->
              Some (Constant { value = String test_name; kind = None }))
            test_names;
        values = test_data_dict_items;
      }
  in
  let agg_dict_type_annot =
    Subscript
      {
        value = Name { id = "dict"; ctx = mk_ctx () };
        slice =
          Tuple
            {
              elts =
                [
                  Name { id = "str"; ctx = mk_ctx () };
                  Subscript
                    {
                      value = Name { id = "dict"; ctx = mk_ctx () };
                      slice =
                        Tuple
                          {
                            elts =
                              [
                                Name { id = "str"; ctx = mk_ctx () };
                                Name { id = "Any"; ctx = mk_ctx () };
                              ];
                            ctx = mk_ctx ();
                            dims = [];
                          };
                      ctx = mk_ctx ();
                    };
                ];
              ctx = mk_ctx ();
              dims = [];
            };
        ctx = mk_ctx ();
      }
  in
  AnnAssign
    {
      target = Name { id = "tests"; ctx = mk_ctx () };
      annotation = agg_dict_type_annot;
      value = Some agg_dict;
      simple = 1;
    }

(* Expect test
==================== *)

let%expect_test "bool list expr to string" =
  let bools = [ false; true; false; false; false; false; false; true ] in
  let c = bools_to_char bools in
  Printf.printf "%c\n" c;
  [%expect {| A |}]

let%expect_test "char to bools" =
  let c = '0' in
  let bools = bools_of_char c in
  List.iter (Printf.printf "%b ") bools;
  [%expect {| false false false false true true false false |}]

let%expect_test "build union" =
  let union_stmt = mk_union_def "Status" [ "str"; "int" ] in
  print_endline (show_stmt union_stmt);
  [%expect
    {|
    (Ast_types.Assign
       { Ast_types.targets =
         [(Ast_types.Name { Ast_types.id = "Status"; ctx = Ast_types.Load })];
         value =
         (Ast_types.BinOp
            { Ast_types.left =
              (Ast_types.Name { Ast_types.id = "str"; ctx = Ast_types.Load });
              op = Ast_types.BitOr;
              right =
              (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load }) });
         type_comment = None })
    |}]

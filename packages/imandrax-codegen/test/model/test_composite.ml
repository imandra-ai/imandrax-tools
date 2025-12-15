open Test_utils

let%expect_test "variant_and_record" =
  test_parse_model (Some "composite") "variant_and_record";
  [%expect
    {|
    name: variant_and_record
    iml_code:
    type direction = North | South | East | West

    type position = { x: int; y: int; z: real }

    type movement =
      | Stay of position
      | Move of position * direction

    let v =
      fun w ->
          if w = Move ({x=1; y=2; z=3.0}, North) then true else false

    Type defs:
    (Ast.ClassDef
       { Ast.name = "position"; bases = []; keywords = [];
         body =
         [(Ast.AnnAssign
             { Ast.target = (Ast.Name { Ast.id = "x"; ctx = Ast.Load });
               annotation = (Ast.Name { Ast.id = "int"; ctx = Ast.Load });
               value = None; simple = 1 });
           (Ast.AnnAssign
              { Ast.target = (Ast.Name { Ast.id = "y"; ctx = Ast.Load });
                annotation = (Ast.Name { Ast.id = "int"; ctx = Ast.Load });
                value = None; simple = 1 });
           (Ast.AnnAssign
              { Ast.target = (Ast.Name { Ast.id = "z"; ctx = Ast.Load });
                annotation = (Ast.Name { Ast.id = "real"; ctx = Ast.Load });
                value = None; simple = 1 })
           ];
         decorator_list = [(Ast.Name { Ast.id = "dataclass"; ctx = Ast.Load })] })
    (Ast.ClassDef
       { Ast.name = "North"; bases = []; keywords = []; body = [Ast.Pass];
         decorator_list = [(Ast.Name { Ast.id = "dataclass"; ctx = Ast.Load })] })
    (Ast.Assign
       { Ast.targets = [(Ast.Name { Ast.id = "direction"; ctx = Ast.Load })];
         value = (Ast.Name { Ast.id = "North"; ctx = Ast.Load });
         type_comment = None })
    (Ast.ClassDef
       { Ast.name = "Move"; bases = []; keywords = [];
         body =
         [(Ast.AnnAssign
             { Ast.target = (Ast.Name { Ast.id = "arg0"; ctx = Ast.Load });
               annotation = (Ast.Name { Ast.id = "position"; ctx = Ast.Load });
               value = None; simple = 1 });
           (Ast.AnnAssign
              { Ast.target = (Ast.Name { Ast.id = "arg1"; ctx = Ast.Load });
                annotation = (Ast.Name { Ast.id = "direction"; ctx = Ast.Load });
                value = None; simple = 1 })
           ];
         decorator_list = [(Ast.Name { Ast.id = "dataclass"; ctx = Ast.Load })] })
    (Ast.Assign
       { Ast.targets = [(Ast.Name { Ast.id = "movement"; ctx = Ast.Load })];
         value = (Ast.Name { Ast.id = "Move"; ctx = Ast.Load });
         type_comment = None })

    Type annot:
    (Ast.Name { Ast.id = "movement"; ctx = Ast.Load })

    Expr:
    (Ast.Call
       { Ast.func = (Ast.Name { Ast.id = "Move"; ctx = Ast.Load });
         args =
         [(Ast.Call
             { Ast.func = (Ast.Name { Ast.id = "position"; ctx = Ast.Load });
               args =
               [(Ast.Constant { Ast.value = (Ast.Int 1); kind = None });
                 (Ast.Constant { Ast.value = (Ast.Int 2); kind = None });
                 (Ast.Constant { Ast.value = (Ast.Float 3.); kind = None })];
               keywords = [] });
           (Ast.Call
              { Ast.func = (Ast.Name { Ast.id = "North"; ctx = Ast.Load });
                args = []; keywords = [] })
           ];
         keywords = [] })
    |}]

let%expect_test "inline record" =
  test_parse_model (Some "composite") "inline_record";
  [%expect
    {|
    name: inline_record
    iml_code:
    type event =
        | Click of { x: int; y: int }
        | Keypress of { key: LChar.t; modifiers: LString.t list }
        | Scroll of { delta: real }

    let v = Scroll {delta = 2.0}
    let v = fun w -> if w = v then true else false

    Type defs:
    (Ast.ClassDef
       { Ast.name = "Scroll"; bases = []; keywords = [];
         body =
         [(Ast.AnnAssign
             { Ast.target = (Ast.Name { Ast.id = "arg0"; ctx = Ast.Load });
               annotation = (Ast.Name { Ast.id = "real"; ctx = Ast.Load });
               value = None; simple = 1 })
           ];
         decorator_list = [(Ast.Name { Ast.id = "dataclass"; ctx = Ast.Load })] })
    (Ast.Assign
       { Ast.targets = [(Ast.Name { Ast.id = "event"; ctx = Ast.Load })];
         value = (Ast.Name { Ast.id = "Scroll"; ctx = Ast.Load });
         type_comment = None })

    Type annot:
    (Ast.Name { Ast.id = "event"; ctx = Ast.Load })

    Expr:
    (Ast.Call
       { Ast.func = (Ast.Name { Ast.id = "Scroll"; ctx = Ast.Load });
         args = [(Ast.Constant { Ast.value = (Ast.Float 2.); kind = None })];
         keywords = [] })
    |}]

let%expect_test "map 0" =
  test_parse_model (Some "composite") "map_int_bool_0";
  [%expect
    {|
    name: map_int_bool
    iml_code:
    let v : (int, bool) Map.t =
      Map.const false

    let v = fun w -> if w = v then true else false

    Type defs:

    Type annot:
    (Ast.Subscript
       { Ast.value = (Ast.Name { Ast.id = "defaultdict"; ctx = Ast.Load });
         slice =
         (Ast.Tuple
            { Ast.elts =
              [(Ast.Name { Ast.id = "int"; ctx = Ast.Load });
                (Ast.Name { Ast.id = "bool"; ctx = Ast.Load })];
              ctx = Ast.Load; dims = [] });
         ctx = Ast.Load })

    Expr:
    (Ast.Call
       { Ast.func = (Ast.Name { Ast.id = "defaultdict"; ctx = Ast.Load });
         args =
         [(Ast.Lambda
             { Ast.args =
               { Ast.posonlyargs = []; args = []; vararg = None; kwonlyargs = [];
                 kw_defaults = []; kwarg = None; defaults = [] };
               body =
               (Ast.Constant { Ast.value = (Ast.Bool false); kind = None }) })
           ];
         keywords = [] })
    |}]

let%expect_test "map 1" =
  test_parse_model (Some "composite") "map_int_bool_1";
  [%expect
    {|
    name: map_int_bool_1
    iml_code:
    let v : (int, bool) Map.t =
      Map.const false
      |> Map.add 2 true

    let v = fun w -> if w = v then true else false

    Type defs:

    Type annot:
    (Ast.Subscript
       { Ast.value = (Ast.Name { Ast.id = "defaultdict"; ctx = Ast.Load });
         slice =
         (Ast.Tuple
            { Ast.elts =
              [(Ast.Name { Ast.id = "int"; ctx = Ast.Load });
                (Ast.Name { Ast.id = "bool"; ctx = Ast.Load })];
              ctx = Ast.Load; dims = [] });
         ctx = Ast.Load })

    Expr:
    (Ast.Call
       { Ast.func = (Ast.Name { Ast.id = "defaultdict"; ctx = Ast.Load });
         args =
         [(Ast.Lambda
             { Ast.args =
               { Ast.posonlyargs = []; args = []; vararg = None; kwonlyargs = [];
                 kw_defaults = []; kwarg = None; defaults = [] };
               body =
               (Ast.Constant { Ast.value = (Ast.Bool false); kind = None }) });
           (Ast.Dict
              { Ast.keys =
                [(Some (Ast.Constant { Ast.value = (Ast.Int 2); kind = None }))];
                values =
                [(Ast.Constant { Ast.value = (Ast.Bool true); kind = None })] })
           ];
         keywords = [] })
    |}]

let%expect_test "map 2" =
  test_parse_model (Some "composite") "map_int_bool_2";
  [%expect
    {|
    name: map_int_bool_2
    iml_code:
    let v : (int, bool) Map.t =
      Map.const false
      |> Map.add 2 true
      |> Map.add 3 true

    let v = fun w -> if w = v then true else false

    Type defs:

    Type annot:
    (Ast.Subscript
       { Ast.value = (Ast.Name { Ast.id = "defaultdict"; ctx = Ast.Load });
         slice =
         (Ast.Tuple
            { Ast.elts =
              [(Ast.Name { Ast.id = "int"; ctx = Ast.Load });
                (Ast.Name { Ast.id = "bool"; ctx = Ast.Load })];
              ctx = Ast.Load; dims = [] });
         ctx = Ast.Load })

    Expr:
    (Ast.Call
       { Ast.func = (Ast.Name { Ast.id = "defaultdict"; ctx = Ast.Load });
         args =
         [(Ast.Lambda
             { Ast.args =
               { Ast.posonlyargs = []; args = []; vararg = None; kwonlyargs = [];
                 kw_defaults = []; kwarg = None; defaults = [] };
               body =
               (Ast.Constant { Ast.value = (Ast.Bool false); kind = None }) });
           (Ast.Dict
              { Ast.keys =
                [(Some (Ast.Constant { Ast.value = (Ast.Int 2); kind = None }));
                  (Some (Ast.Constant { Ast.value = (Ast.Int 3); kind = None }))];
                values =
                [(Ast.Constant { Ast.value = (Ast.Bool true); kind = None });
                  (Ast.Constant { Ast.value = (Ast.Bool true); kind = None })]
                })
           ];
         keywords = [] })
    |}]

let%expect_test "map 3" =
  test_parse_model (Some "composite") "map_int_bool_3";
  [%expect
    {|
    name: map_int_bool_3
    iml_code:
    let v : (int, bool) Map.t =
      Map.const false
      |> Map.add 2 true
      |> Map.add 3 false
      |> Map.add 5 true

    let v = fun w -> if w = v then true else false

    Type defs:

    Type annot:
    (Ast.Subscript
       { Ast.value = (Ast.Name { Ast.id = "defaultdict"; ctx = Ast.Load });
         slice =
         (Ast.Tuple
            { Ast.elts =
              [(Ast.Name { Ast.id = "int"; ctx = Ast.Load });
                (Ast.Name { Ast.id = "bool"; ctx = Ast.Load })];
              ctx = Ast.Load; dims = [] });
         ctx = Ast.Load })

    Expr:
    (Ast.Call
       { Ast.func = (Ast.Name { Ast.id = "defaultdict"; ctx = Ast.Load });
         args =
         [(Ast.Lambda
             { Ast.args =
               { Ast.posonlyargs = []; args = []; vararg = None; kwonlyargs = [];
                 kw_defaults = []; kwarg = None; defaults = [] };
               body =
               (Ast.Constant { Ast.value = (Ast.Bool false); kind = None }) });
           (Ast.Dict
              { Ast.keys =
                [(Some (Ast.Constant { Ast.value = (Ast.Int 2); kind = None }));
                  (Some (Ast.Constant { Ast.value = (Ast.Int 3); kind = None }));
                  (Some (Ast.Constant { Ast.value = (Ast.Int 5); kind = None }))];
                values =
                [(Ast.Constant { Ast.value = (Ast.Bool true); kind = None });
                  (Ast.Constant { Ast.value = (Ast.Bool false); kind = None });
                  (Ast.Constant { Ast.value = (Ast.Bool true); kind = None })]
                })
           ];
         keywords = [] })
    |}]

let%expect_test "nonempty set" =
  test_parse_model (Some "composite") "set_nonempty";
  [%expect
    {|
    name: set_nonempty
    iml_code:
    let v = Set.of_list [1; 2; 3; 2; 1]

    let v = fun w -> if w = v then true else false

    Type defs:

    Type annot:
    (Ast.Subscript
       { Ast.value = (Ast.Name { Ast.id = "defaultdict"; ctx = Ast.Load });
         slice =
         (Ast.Tuple
            { Ast.elts =
              [(Ast.Name { Ast.id = "int"; ctx = Ast.Load });
                (Ast.Name { Ast.id = "bool"; ctx = Ast.Load })];
              ctx = Ast.Load; dims = [] });
         ctx = Ast.Load })

    Expr:
    (Ast.Call
       { Ast.func = (Ast.Name { Ast.id = "defaultdict"; ctx = Ast.Load });
         args =
         [(Ast.Lambda
             { Ast.args =
               { Ast.posonlyargs = []; args = []; vararg = None; kwonlyargs = [];
                 kw_defaults = []; kwarg = None; defaults = [] };
               body =
               (Ast.Constant { Ast.value = (Ast.Bool false); kind = None }) });
           (Ast.Dict
              { Ast.keys =
                [(Some (Ast.Constant { Ast.value = (Ast.Int 1); kind = None }));
                  (Some (Ast.Constant { Ast.value = (Ast.Int 3); kind = None }));
                  (Some (Ast.Constant { Ast.value = (Ast.Int 2); kind = None }))];
                values =
                [(Ast.Constant { Ast.value = (Ast.Bool true); kind = None });
                  (Ast.Constant { Ast.value = (Ast.Bool true); kind = None });
                  (Ast.Constant { Ast.value = (Ast.Bool true); kind = None })]
                })
           ];
         keywords = [] })
    |}]

let%expect_test "nonempty multiset" =
  test_parse_model (Some "composite") "multiset_nonempty";
  [%expect
    {|
    name: multiset_nonempty
    iml_code:
    let v = Multiset.of_list [1; 2; 3; 2; 1]

    let v = fun w -> if w = v then true else false

    Type defs:

    Type annot:
    (Ast.Subscript
       { Ast.value = (Ast.Name { Ast.id = "defaultdict"; ctx = Ast.Load });
         slice =
         (Ast.Tuple
            { Ast.elts =
              [(Ast.Name { Ast.id = "int"; ctx = Ast.Load });
                (Ast.Name { Ast.id = "int"; ctx = Ast.Load })];
              ctx = Ast.Load; dims = [] });
         ctx = Ast.Load })

    Expr:
    (Ast.Call
       { Ast.func = (Ast.Name { Ast.id = "defaultdict"; ctx = Ast.Load });
         args =
         [(Ast.Lambda
             { Ast.args =
               { Ast.posonlyargs = []; args = []; vararg = None; kwonlyargs = [];
                 kw_defaults = []; kwarg = None; defaults = [] };
               body = (Ast.Constant { Ast.value = (Ast.Int 0); kind = None }) });
           (Ast.Dict
              { Ast.keys =
                [(Some (Ast.Constant { Ast.value = (Ast.Int 1); kind = None }));
                  (Some (Ast.Constant { Ast.value = (Ast.Int 3); kind = None }));
                  (Some (Ast.Constant { Ast.value = (Ast.Int 2); kind = None }))];
                values =
                [(Ast.Constant { Ast.value = (Ast.Int 2); kind = None });
                  (Ast.Constant { Ast.value = (Ast.Int 1); kind = None });
                  (Ast.Constant { Ast.value = (Ast.Int 2); kind = None })]
                })
           ];
         keywords = [] })
    |}]

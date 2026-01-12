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

    Type annot:
    (Ast_types.Name { Ast_types.id = "movement"; ctx = Ast_types.Load })

    Expr:
    (Ast_types.Call
       { Ast_types.func =
         (Ast_types.Name { Ast_types.id = "Move"; ctx = Ast_types.Load });
         args =
         [(Ast_types.Call
             { Ast_types.func =
               (Ast_types.Name
                  { Ast_types.id = "position"; ctx = Ast_types.Load });
               args =
               [(Ast_types.Constant
                   { Ast_types.value = (Ast_types.Int 1); kind = None });
                 (Ast_types.Constant
                    { Ast_types.value = (Ast_types.Int 2); kind = None });
                 (Ast_types.Constant
                    { Ast_types.value = (Ast_types.Float 3.); kind = None })
                 ];
               keywords = [] });
           (Ast_types.Call
              { Ast_types.func =
                (Ast_types.Name { Ast_types.id = "North"; ctx = Ast_types.Load });
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

    Type annot:
    (Ast_types.Name { Ast_types.id = "event"; ctx = Ast_types.Load })

    Expr:
    (Ast_types.Call
       { Ast_types.func =
         (Ast_types.Name { Ast_types.id = "Scroll"; ctx = Ast_types.Load });
         args =
         [(Ast_types.Constant
             { Ast_types.value = (Ast_types.Float 2.); kind = None })
           ];
         keywords = [] })
    |}]

let%expect_test "map 0" =
  test_parse_model (Some "composite") "map_int_bool_0";
  [%expect
    {|
    name: map_int_bool_0
    iml_code:
    let v : (int, bool) Map.t =
      Map.const false

    let v = fun w -> if w = v then true else false

    Type annot:
    (Ast_types.Subscript
       { Ast_types.value =
         (Ast_types.Name { Ast_types.id = "defaultdict"; ctx = Ast_types.Load });
         slice =
         (Ast_types.Tuple
            { Ast_types.elts =
              [(Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
                (Ast_types.Name { Ast_types.id = "bool"; ctx = Ast_types.Load })];
              ctx = Ast_types.Load; dims = [] });
         ctx = Ast_types.Load })

    Expr:
    (Ast_types.Call
       { Ast_types.func =
         (Ast_types.Name { Ast_types.id = "defaultdict"; ctx = Ast_types.Load });
         args =
         [(Ast_types.Lambda
             { Ast_types.args =
               { Ast_types.posonlyargs = []; args = []; vararg = None;
                 kwonlyargs = []; kw_defaults = []; kwarg = None; defaults = [] };
               body =
               (Ast_types.Constant
                  { Ast_types.value = (Ast_types.Bool false); kind = None })
               })
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

    Type annot:
    (Ast_types.Subscript
       { Ast_types.value =
         (Ast_types.Name { Ast_types.id = "defaultdict"; ctx = Ast_types.Load });
         slice =
         (Ast_types.Tuple
            { Ast_types.elts =
              [(Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
                (Ast_types.Name { Ast_types.id = "bool"; ctx = Ast_types.Load })];
              ctx = Ast_types.Load; dims = [] });
         ctx = Ast_types.Load })

    Expr:
    (Ast_types.Call
       { Ast_types.func =
         (Ast_types.Name { Ast_types.id = "defaultdict"; ctx = Ast_types.Load });
         args =
         [(Ast_types.Lambda
             { Ast_types.args =
               { Ast_types.posonlyargs = []; args = []; vararg = None;
                 kwonlyargs = []; kw_defaults = []; kwarg = None; defaults = [] };
               body =
               (Ast_types.Constant
                  { Ast_types.value = (Ast_types.Bool false); kind = None })
               });
           (Ast_types.Dict
              { Ast_types.keys =
                [(Some (Ast_types.Constant
                          { Ast_types.value = (Ast_types.Int 2); kind = None }))
                  ];
                values =
                [(Ast_types.Constant
                    { Ast_types.value = (Ast_types.Bool true); kind = None })
                  ]
                })
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
       |> Map.add 3 false

    let v = fun w -> if w = v then true else false

    Type annot:
    (Ast_types.Subscript
       { Ast_types.value =
         (Ast_types.Name { Ast_types.id = "defaultdict"; ctx = Ast_types.Load });
         slice =
         (Ast_types.Tuple
            { Ast_types.elts =
              [(Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
                (Ast_types.Name { Ast_types.id = "bool"; ctx = Ast_types.Load })];
              ctx = Ast_types.Load; dims = [] });
         ctx = Ast_types.Load })

    Expr:
    (Ast_types.Call
       { Ast_types.func =
         (Ast_types.Name { Ast_types.id = "defaultdict"; ctx = Ast_types.Load });
         args =
         [(Ast_types.Lambda
             { Ast_types.args =
               { Ast_types.posonlyargs = []; args = []; vararg = None;
                 kwonlyargs = []; kw_defaults = []; kwarg = None; defaults = [] };
               body =
               (Ast_types.Constant
                  { Ast_types.value = (Ast_types.Bool false); kind = None })
               });
           (Ast_types.Dict
              { Ast_types.keys =
                [(Some (Ast_types.Constant
                          { Ast_types.value = (Ast_types.Int 2); kind = None }));
                  (Some (Ast_types.Constant
                           { Ast_types.value = (Ast_types.Int 3); kind = None }))
                  ];
                values =
                [(Ast_types.Constant
                    { Ast_types.value = (Ast_types.Bool true); kind = None });
                  (Ast_types.Constant
                     { Ast_types.value = (Ast_types.Bool false); kind = None })
                  ]
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

    Type annot:
    (Ast_types.Subscript
       { Ast_types.value =
         (Ast_types.Name { Ast_types.id = "defaultdict"; ctx = Ast_types.Load });
         slice =
         (Ast_types.Tuple
            { Ast_types.elts =
              [(Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
                (Ast_types.Name { Ast_types.id = "bool"; ctx = Ast_types.Load })];
              ctx = Ast_types.Load; dims = [] });
         ctx = Ast_types.Load })

    Expr:
    (Ast_types.Call
       { Ast_types.func =
         (Ast_types.Name { Ast_types.id = "defaultdict"; ctx = Ast_types.Load });
         args =
         [(Ast_types.Lambda
             { Ast_types.args =
               { Ast_types.posonlyargs = []; args = []; vararg = None;
                 kwonlyargs = []; kw_defaults = []; kwarg = None; defaults = [] };
               body =
               (Ast_types.Constant
                  { Ast_types.value = (Ast_types.Bool false); kind = None })
               });
           (Ast_types.Dict
              { Ast_types.keys =
                [(Some (Ast_types.Constant
                          { Ast_types.value = (Ast_types.Int 1); kind = None }));
                  (Some (Ast_types.Constant
                           { Ast_types.value = (Ast_types.Int 3); kind = None }));
                  (Some (Ast_types.Constant
                           { Ast_types.value = (Ast_types.Int 2); kind = None }))
                  ];
                values =
                [(Ast_types.Constant
                    { Ast_types.value = (Ast_types.Bool true); kind = None });
                  (Ast_types.Constant
                     { Ast_types.value = (Ast_types.Bool true); kind = None });
                  (Ast_types.Constant
                     { Ast_types.value = (Ast_types.Bool true); kind = None })
                  ]
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

    Type annot:
    (Ast_types.Subscript
       { Ast_types.value =
         (Ast_types.Name { Ast_types.id = "defaultdict"; ctx = Ast_types.Load });
         slice =
         (Ast_types.Tuple
            { Ast_types.elts =
              [(Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
                (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load })];
              ctx = Ast_types.Load; dims = [] });
         ctx = Ast_types.Load })

    Expr:
    (Ast_types.Call
       { Ast_types.func =
         (Ast_types.Name { Ast_types.id = "defaultdict"; ctx = Ast_types.Load });
         args =
         [(Ast_types.Lambda
             { Ast_types.args =
               { Ast_types.posonlyargs = []; args = []; vararg = None;
                 kwonlyargs = []; kw_defaults = []; kwarg = None; defaults = [] };
               body =
               (Ast_types.Constant
                  { Ast_types.value = (Ast_types.Int 0); kind = None })
               });
           (Ast_types.Dict
              { Ast_types.keys =
                [(Some (Ast_types.Constant
                          { Ast_types.value = (Ast_types.Int 1); kind = None }));
                  (Some (Ast_types.Constant
                           { Ast_types.value = (Ast_types.Int 3); kind = None }));
                  (Some (Ast_types.Constant
                           { Ast_types.value = (Ast_types.Int 2); kind = None }))
                  ];
                values =
                [(Ast_types.Constant
                    { Ast_types.value = (Ast_types.Int 2); kind = None });
                  (Ast_types.Constant
                     { Ast_types.value = (Ast_types.Int 1); kind = None });
                  (Ast_types.Constant
                     { Ast_types.value = (Ast_types.Int 2); kind = None })
                  ]
                })
           ];
         keywords = [] })
    |}]

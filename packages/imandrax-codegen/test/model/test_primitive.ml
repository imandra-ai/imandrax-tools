open Test_utils

let%expect_test "bool list" =
  test_parse_model (Some "primitive") "bool_list";

  [%expect
    {|
    name: bool list
    iml_code:
    let v =
      fun w ->
        if w = [true; false] then true else false

    Type annot:
    (Ast_types.Subscript
       { Ast_types.value =
         (Ast_types.Name { Ast_types.id = "list"; ctx = Ast_types.Load });
         slice = (Ast_types.Name { Ast_types.id = "bool"; ctx = Ast_types.Load });
         ctx = Ast_types.Load })

    Expr:
    (Ast_types.List
       { Ast_types.elts =
         [(Ast_types.Constant
             { Ast_types.value = (Ast_types.Bool true); kind = None });
           (Ast_types.Constant
              { Ast_types.value = (Ast_types.Bool false); kind = None })
           ];
         ctx = Ast_types.Load })
    |}]

let%expect_test "empty list" =
  test_parse_model (Some "primitive") "empty_list";
  [%expect
    {|
    name: empty list
    iml_code:
    let v =
      fun w ->
        if w = [] then true else false

    Type annot:
    None

    Expr:
    (Ast_types.List { Ast_types.elts = []; ctx = Ast_types.Load })
    |}]

let%expect_test "int option" =
  test_parse_model (Some "primitive") "int_option";
  [%expect
    {|
    name: int option
    iml_code:
    let v =
      fun w ->
        if w = Some 2 then true else false

    Type annot:
    (Ast_types.Subscript
       { Ast_types.value =
         (Ast_types.Name { Ast_types.id = "option"; ctx = Ast_types.Load });
         slice = (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
         ctx = Ast_types.Load })

    Expr:
    (Ast_types.Call
       { Ast_types.func =
         (Ast_types.Name { Ast_types.id = "Some"; ctx = Ast_types.Load });
         args =
         [(Ast_types.Constant
             { Ast_types.value = (Ast_types.Int 2); kind = None })
           ];
         keywords = [] })
    |}]

let%expect_test "int" =
  test_parse_model (Some "primitive") "int";
  [%expect
    {|
    name: int
    iml_code:
    let v =
      fun w ->
        if w = 2 then true else false

    Type annot:
    (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load })

    Expr:
    (Ast_types.Constant { Ast_types.value = (Ast_types.Int 2); kind = None })
    |}]

let%expect_test "LChar" =
  test_parse_model (Some "primitive") "LChar";
  [%expect
    {|
    name: LChar
    iml_code:
    let v =
      fun w ->
        if w = LChar.zero then true else false

    Type annot:
    (Ast_types.Name { Ast_types.id = "str"; ctx = Ast_types.Load })

    Expr:
    (Ast_types.Constant
       { Ast_types.value = (Ast_types.String "\000"); kind = None })
    |}]

let%expect_test "LString" =
  test_parse_model (Some "primitive") "LString";
  [%expect
    {|
    name: LString
    iml_code:
    let v =
      fun w ->
        if w = {l|hi|l} then true else false

    Type annot:
    (Ast_types.Subscript
       { Ast_types.value =
         (Ast_types.Name { Ast_types.id = "list"; ctx = Ast_types.Load });
         slice = (Ast_types.Name { Ast_types.id = "str"; ctx = Ast_types.Load });
         ctx = Ast_types.Load })

    Expr:
    (Ast_types.List
       { Ast_types.elts =
         [(Ast_types.Constant
             { Ast_types.value = (Ast_types.String "h"); kind = None });
           (Ast_types.Constant
              { Ast_types.value = (Ast_types.String "i"); kind = None })
           ];
         ctx = Ast_types.Load })
    |}]

let%expect_test "real" =
  test_parse_model (Some "primitive") "real";
  [%expect
    {|
    name: real
    iml_code:
    let v =
      fun w ->
        if w = 3.14 then true else false

    Type annot:
    (Ast_types.Name { Ast_types.id = "float"; ctx = Ast_types.Load })

    Expr:
    (Ast_types.Constant { Ast_types.value = (Ast_types.Float 3.14); kind = None })
    |}]

let%expect_test "record" =
  test_parse_model (Some "primitive") "record";
  [%expect
    {|
    name: record
    iml_code:
    type user = {
        id: int;
        active: bool;
    }

    let v = {id = 1; active = true}

    let v =
      fun w ->
        if w = v then true else false

    Type annot:
    (Ast_types.Name { Ast_types.id = "user"; ctx = Ast_types.Load })

    Expr:
    (Ast_types.Call
       { Ast_types.func =
         (Ast_types.Name { Ast_types.id = "user"; ctx = Ast_types.Load });
         args =
         [(Ast_types.Constant
             { Ast_types.value = (Ast_types.Int 1); kind = None });
           (Ast_types.Constant
              { Ast_types.value = (Ast_types.Bool true); kind = None })
           ];
         keywords = [] })
    |}]

let%expect_test "single element int list" =
  test_parse_model (Some "primitive") "single_element_int_list";
  [%expect
    {|
    name: single element int list
    iml_code:
    let v =
      fun w ->
        if w = [1] then true else false

    Type annot:
    (Ast_types.Subscript
       { Ast_types.value =
         (Ast_types.Name { Ast_types.id = "list"; ctx = Ast_types.Load });
         slice = (Ast_types.Name { Ast_types.id = "int"; ctx = Ast_types.Load });
         ctx = Ast_types.Load })

    Expr:
    (Ast_types.List
       { Ast_types.elts =
         [(Ast_types.Constant
             { Ast_types.value = (Ast_types.Int 1); kind = None })
           ];
         ctx = Ast_types.Load })
    |}]

let%expect_test "variant1" =
  test_parse_model (Some "primitive") "variant1";
  [%expect
    {|
    name: variant1
    iml_code:
    type status =
        | Active
        | Waitlist of int

    let v = Active

    let v =
      fun w ->
        if w = v then true else false

    Type annot:
    (Ast_types.Name { Ast_types.id = "status"; ctx = Ast_types.Load })

    Expr:
    (Ast_types.Call
       { Ast_types.func =
         (Ast_types.Name { Ast_types.id = "Active"; ctx = Ast_types.Load });
         args = []; keywords = [] })
    |}]

let%expect_test "variant2" =
  test_parse_model (Some "primitive") "variant2";
  [%expect
    {|
    name: variant2
    iml_code:
    type status =
        | Active
        | Waitlist of int

    let v = Waitlist 1

    let v =
      fun w ->
        if w = v then true else false

    Type annot:
    (Ast_types.Name { Ast_types.id = "status"; ctx = Ast_types.Load })

    Expr:
    (Ast_types.Call
       { Ast_types.func =
         (Ast_types.Name { Ast_types.id = "Waitlist"; ctx = Ast_types.Load });
         args =
         [(Ast_types.Constant
             { Ast_types.value = (Ast_types.Int 1); kind = None })
           ];
         keywords = [] })
    |}]

let%expect_test "variant3" =
  test_parse_model (Some "primitive") "variant3";
  [%expect
    {|
    name: variant3
    iml_code:
    type status =
        | Active
        | Waitlist of int * bool

    let v = Waitlist (2, true)

    let v =
      fun w ->
        if w = v then true else false

    Type annot:
    (Ast_types.Name { Ast_types.id = "status"; ctx = Ast_types.Load })

    Expr:
    (Ast_types.Call
       { Ast_types.func =
         (Ast_types.Name { Ast_types.id = "Waitlist"; ctx = Ast_types.Load });
         args =
         [(Ast_types.Constant
             { Ast_types.value = (Ast_types.Int 2); kind = None });
           (Ast_types.Constant
              { Ast_types.value = (Ast_types.Bool true); kind = None })
           ];
         keywords = [] })
    |}]

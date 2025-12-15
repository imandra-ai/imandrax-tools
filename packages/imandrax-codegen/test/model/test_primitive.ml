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

    Type defs:

    Type annot:
    (Ast.Subscript
       { Ast.value = (Ast.Name { Ast.id = "list"; ctx = Ast.Load });
         slice = (Ast.Name { Ast.id = "bool"; ctx = Ast.Load }); ctx = Ast.Load })

    Expr:
    (Ast.List
       { Ast.elts =
         [(Ast.Constant { Ast.value = (Ast.Bool true); kind = None });
           (Ast.Constant { Ast.value = (Ast.Bool false); kind = None })];
         ctx = Ast.Load })
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

    Type defs:

    Type annot:
    None

    Expr:
    (Ast.List { Ast.elts = []; ctx = Ast.Load })
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

    Type defs:
    (Ast.ClassDef
       { Ast.name = "Some"; bases = []; keywords = [];
         body =
         [(Ast.AnnAssign
             { Ast.target = (Ast.Name { Ast.id = "arg0"; ctx = Ast.Load });
               annotation = (Ast.Name { Ast.id = "int"; ctx = Ast.Load });
               value = None; simple = 1 })
           ];
         decorator_list = [(Ast.Name { Ast.id = "dataclass"; ctx = Ast.Load })] })
    (Ast.Assign
       { Ast.targets = [(Ast.Name { Ast.id = "option"; ctx = Ast.Load })];
         value = (Ast.Name { Ast.id = "Some"; ctx = Ast.Load });
         type_comment = None })

    Type annot:
    (Ast.Name { Ast.id = "option"; ctx = Ast.Load })

    Expr:
    (Ast.Call
       { Ast.func = (Ast.Name { Ast.id = "Some"; ctx = Ast.Load });
         args = [(Ast.Constant { Ast.value = (Ast.Int 2); kind = None })];
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

    Type defs:

    Type annot:
    (Ast.Name { Ast.id = "int"; ctx = Ast.Load })

    Expr:
    (Ast.Constant { Ast.value = (Ast.Int 2); kind = None })
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

    Type defs:

    Type annot:
    (Ast.Name { Ast.id = "str"; ctx = Ast.Load })

    Expr:
    (Ast.Constant { Ast.value = (Ast.String "\000"); kind = None })
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

    Type defs:

    Type annot:
    (Ast.Subscript
       { Ast.value = (Ast.Name { Ast.id = "list"; ctx = Ast.Load });
         slice = (Ast.Name { Ast.id = "str"; ctx = Ast.Load }); ctx = Ast.Load })

    Expr:
    (Ast.List
       { Ast.elts =
         [(Ast.Constant { Ast.value = (Ast.String "h"); kind = None });
           (Ast.Constant { Ast.value = (Ast.String "i"); kind = None })];
         ctx = Ast.Load })
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

    Type defs:

    Type annot:
    (Ast.Name { Ast.id = "float"; ctx = Ast.Load })

    Expr:
    (Ast.Constant { Ast.value = (Ast.Float 3.14); kind = None })
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

    Type defs:
    (Ast.ClassDef
       { Ast.name = "user"; bases = []; keywords = [];
         body =
         [(Ast.AnnAssign
             { Ast.target = (Ast.Name { Ast.id = "id"; ctx = Ast.Load });
               annotation = (Ast.Name { Ast.id = "int"; ctx = Ast.Load });
               value = None; simple = 1 });
           (Ast.AnnAssign
              { Ast.target = (Ast.Name { Ast.id = "active"; ctx = Ast.Load });
                annotation = (Ast.Name { Ast.id = "bool"; ctx = Ast.Load });
                value = None; simple = 1 })
           ];
         decorator_list = [(Ast.Name { Ast.id = "dataclass"; ctx = Ast.Load })] })

    Type annot:
    (Ast.Name { Ast.id = "user"; ctx = Ast.Load })

    Expr:
    (Ast.Call
       { Ast.func = (Ast.Name { Ast.id = "user"; ctx = Ast.Load });
         args =
         [(Ast.Constant { Ast.value = (Ast.Int 1); kind = None });
           (Ast.Constant { Ast.value = (Ast.Bool true); kind = None })];
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

    Type defs:

    Type annot:
    (Ast.Subscript
       { Ast.value = (Ast.Name { Ast.id = "list"; ctx = Ast.Load });
         slice = (Ast.Name { Ast.id = "int"; ctx = Ast.Load }); ctx = Ast.Load })

    Expr:
    (Ast.List
       { Ast.elts = [(Ast.Constant { Ast.value = (Ast.Int 1); kind = None })];
         ctx = Ast.Load })
    |}]

let%expect_test "tuple (bool * int)" =
  test_parse_model (Some "primitive") "tuple_(bool_*_int)";
  [%expect
    {|
    name: tuple (bool * int)
    iml_code:
    let v =
      fun w ->
        if w = (true, 2) then true else false

    Type defs:

    Type annot:
    (Ast.Subscript
       { Ast.value = (Ast.Name { Ast.id = "tuple"; ctx = Ast.Load });
         slice =
         (Ast.Tuple
            { Ast.elts =
              [(Ast.Name { Ast.id = "bool"; ctx = Ast.Load });
                (Ast.Name { Ast.id = "int"; ctx = Ast.Load })];
              ctx = Ast.Load; dims = [] });
         ctx = Ast.Load })

    Expr:
    (Ast.Tuple
       { Ast.elts =
         [(Ast.Constant { Ast.value = (Ast.Bool true); kind = None });
           (Ast.Constant { Ast.value = (Ast.Int 2); kind = None })];
         ctx = Ast.Load; dims = [] })
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

    Type defs:
    (Ast.ClassDef
       { Ast.name = "Active"; bases = []; keywords = []; body = [Ast.Pass];
         decorator_list = [(Ast.Name { Ast.id = "dataclass"; ctx = Ast.Load })] })
    (Ast.Assign
       { Ast.targets = [(Ast.Name { Ast.id = "status"; ctx = Ast.Load })];
         value = (Ast.Name { Ast.id = "Active"; ctx = Ast.Load });
         type_comment = None })

    Type annot:
    (Ast.Name { Ast.id = "status"; ctx = Ast.Load })

    Expr:
    (Ast.Call
       { Ast.func = (Ast.Name { Ast.id = "Active"; ctx = Ast.Load }); args = [];
         keywords = [] })
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

    Type defs:
    (Ast.ClassDef
       { Ast.name = "Waitlist"; bases = []; keywords = [];
         body =
         [(Ast.AnnAssign
             { Ast.target = (Ast.Name { Ast.id = "arg0"; ctx = Ast.Load });
               annotation = (Ast.Name { Ast.id = "int"; ctx = Ast.Load });
               value = None; simple = 1 })
           ];
         decorator_list = [(Ast.Name { Ast.id = "dataclass"; ctx = Ast.Load })] })
    (Ast.Assign
       { Ast.targets = [(Ast.Name { Ast.id = "status"; ctx = Ast.Load })];
         value = (Ast.Name { Ast.id = "Waitlist"; ctx = Ast.Load });
         type_comment = None })

    Type annot:
    (Ast.Name { Ast.id = "status"; ctx = Ast.Load })

    Expr:
    (Ast.Call
       { Ast.func = (Ast.Name { Ast.id = "Waitlist"; ctx = Ast.Load });
         args = [(Ast.Constant { Ast.value = (Ast.Int 1); kind = None })];
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

    Type defs:
    (Ast.ClassDef
       { Ast.name = "Waitlist"; bases = []; keywords = [];
         body =
         [(Ast.AnnAssign
             { Ast.target = (Ast.Name { Ast.id = "arg0"; ctx = Ast.Load });
               annotation = (Ast.Name { Ast.id = "int"; ctx = Ast.Load });
               value = None; simple = 1 });
           (Ast.AnnAssign
              { Ast.target = (Ast.Name { Ast.id = "arg1"; ctx = Ast.Load });
                annotation = (Ast.Name { Ast.id = "bool"; ctx = Ast.Load });
                value = None; simple = 1 })
           ];
         decorator_list = [(Ast.Name { Ast.id = "dataclass"; ctx = Ast.Load })] })
    (Ast.Assign
       { Ast.targets = [(Ast.Name { Ast.id = "status"; ctx = Ast.Load })];
         value = (Ast.Name { Ast.id = "Waitlist"; ctx = Ast.Load });
         type_comment = None })

    Type annot:
    (Ast.Name { Ast.id = "status"; ctx = Ast.Load })

    Expr:
    (Ast.Call
       { Ast.func = (Ast.Name { Ast.id = "Waitlist"; ctx = Ast.Load });
         args =
         [(Ast.Constant { Ast.value = (Ast.Int 2); kind = None });
           (Ast.Constant { Ast.value = (Ast.Bool true); kind = None })];
         keywords = [] })
    |}]

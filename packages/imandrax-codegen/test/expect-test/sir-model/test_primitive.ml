open Utils
open Printf
module Sir = Semantic_ir

let print_info name iml_code = printf "name: %s\niml_code:\n%s\n" name iml_code

let%expect_test "bool list" =
  let name, iml_code, model = load_artifact "primitive" "bool_list" in

  print_info name iml_code;
  [%expect
    {|
    name: bool list
    iml_code:
    let v =
      fun w ->
        if w = [true; false] then true else false
    |}];

  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect
    {|
    { Types.Value_assignment.var_name = "w";
      ty = (Types.TApp ("list", [(Types.TBase "bool")]));
      tm =
      (Types.VList
         [(Types.VConst (Types.CBool true)); (Types.VConst (Types.CBool false))])
      }
    |}]
;;

let%expect_test "empty list" =
  let name, iml_code, model = load_artifact "primitive" "empty_list" in
  print_info name iml_code;
  [%expect
    {|
    name: empty list
    iml_code:
    let v =
      fun w ->
        if w = [] then true else false
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect
    {|
    { Types.Value_assignment.var_name = "w";
      ty = (Types.TApp ("list", [(Types.TBase "_a_0")])); tm = (Types.VList []) }
    |}]
;;

let%expect_test "int option" =
  let name, iml_code, model = load_artifact "primitive" "int_option" in
  print_info name iml_code;
  [%expect
    {|
    name: int option
    iml_code:
    let v =
      fun w ->
        if w = Some 2 then true else false
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect
    {|
    { Types.Value_assignment.var_name = "w";
      ty = (Types.TApp ("option", [(Types.TBase "int")]));
      tm =
      Types.VConstruct {constructor = "Some";
        args = [(Types.VConst (Types.CInt 2))]}
      }
    |}]
;;

let%expect_test "int" =
  let name, iml_code, model = load_artifact "primitive" "int" in
  print_info name iml_code;
  [%expect
    {|
    name: int
    iml_code:
    let v =
      fun w ->
        if w = 2 then true else false
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect
    {|
    { Types.Value_assignment.var_name = "w"; ty = (Types.TBase "int");
      tm = (Types.VConst (Types.CInt 2)) }
    |}]
;;

let%expect_test "LChar" =
  let name, iml_code, model = load_artifact "primitive" "LChar" in
  print_info name iml_code;
  [%expect
    {|
    name: LChar
    iml_code:
    let v =
      fun w ->
        if w = LChar.zero then true else false
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect
    {|
    { Types.Value_assignment.var_name = "w"; ty = (Types.TBase "char");
      tm = (Types.VConst (Types.CChar '\000')) }
    |}]
;;

let%expect_test "LString" =
  let name, iml_code, model = load_artifact "primitive" "LString" in
  print_info name iml_code;
  [%expect
    {|
    name: LString
    iml_code:
    let v =
      fun w ->
        if w = {l|hi|l} then true else false
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect
    {|
    { Types.Value_assignment.var_name = "w";
      ty = (Types.TApp ("list", [(Types.TBase "LChar.t")]));
      tm =
      (Types.VList
         [(Types.VConst (Types.CChar 'h')); (Types.VConst (Types.CChar 'i'))])
      }
    |}]
;;

let%expect_test "real" =
  let name, iml_code, model = load_artifact "primitive" "real" in
  print_info name iml_code;
  [%expect
    {|
    name: real
    iml_code:
    let v =
      fun w ->
        if w = 3.14 then true else false
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect
    {|
    { Types.Value_assignment.var_name = "w"; ty = (Types.TBase "float");
      tm = (Types.VConst (Types.CFloat 3.14)) }
    |}]
;;

let%expect_test "record" =
  let name, iml_code, model = load_artifact "primitive" "record" in
  print_info name iml_code;
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
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect
    {|
    { Types.Value_assignment.var_name = "w"; ty = (Types.TBase "user");
      tm =
      Types.VRecord {type_name = "user";
        fields =
        [("id", (Types.VConst (Types.CInt 1)));
          ("active", (Types.VConst (Types.CBool true)))]}
      }
    |}]
;;

let%expect_test "single element int list" =
  let name, iml_code, model =
    load_artifact "primitive" "single_element_int_list"
  in
  print_info name iml_code;
  [%expect
    {|
    name: single element int list
    iml_code:
    let v =
      fun w ->
        if w = [1] then true else false
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect
    {|
    { Types.Value_assignment.var_name = "w";
      ty = (Types.TApp ("list", [(Types.TBase "int")]));
      tm = (Types.VList [(Types.VConst (Types.CInt 1))]) }
    |}]
;;

let%expect_test "variant1" =
  let name, iml_code, model = load_artifact "primitive" "variant1" in
  print_info name iml_code;
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
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect
    {|
    { Types.Value_assignment.var_name = "w"; ty = (Types.TBase "status");
      tm = Types.VConstruct {constructor = "Active"; args = []} }
    |}]
;;

let%expect_test "variant2" =
  let name, iml_code, model = load_artifact "primitive" "variant2" in
  print_info name iml_code;
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
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect
    {|
    { Types.Value_assignment.var_name = "w"; ty = (Types.TBase "status");
      tm =
      Types.VConstruct {constructor = "Waitlist";
        args = [(Types.VConst (Types.CInt 1))]}
      }
    |}]
;;

let%expect_test "variant3" =
  let name, iml_code, model = load_artifact "primitive" "variant3" in
  print_info name iml_code;
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
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect
    {|
    { Types.Value_assignment.var_name = "w"; ty = (Types.TBase "status");
      tm =
      Types.VConstruct {constructor = "Waitlist";
        args = [(Types.VConst (Types.CInt 2)); (Types.VConst (Types.CBool true))]}
      }
    |}]
;;

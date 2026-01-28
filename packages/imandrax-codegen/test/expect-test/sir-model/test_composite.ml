open Test_utils
open Printf
module Sir = Semantic_ir

let print_info name iml_code = printf "name: %s\niml_code:\n%s\n" name iml_code

let%expect_test "variant_and_record" =
  let (name, iml_code, model) = load_artifact "composite" "variant_and_record" in
  print_info name iml_code;
  [%expect {|
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
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect {|
    { Types.Value_assignment.var_name = "w"; ty = (Types.TBase "movement");
      tm =
      Types.VConstruct {constructor = "Move";
        args =
        [Types.VRecord {type_name = "position";
           fields =
           [("x", (Types.VConst (Types.CInt 1)));
             ("y", (Types.VConst (Types.CInt 2)));
             ("z", (Types.VConst (Types.CFloat 3.)))]};
          Types.VConstruct {constructor = "North"; args = []}]}
      }
    |}];
;;

let%expect_test "inline record" =
  let (name, iml_code, model) = load_artifact "composite" "inline_record" in
  print_info name iml_code;
  [%expect {|
    name: inline_record
    iml_code:
    type event =
        | Click of { x: int; y: int }
        | Keypress of { key: LChar.t; modifiers: LString.t list }
        | Scroll of { delta: real }

    let v = Scroll {delta = 2.0}
    let v = fun w -> if w = v then true else false
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect {|
    { Types.Value_assignment.var_name = "w"; ty = (Types.TBase "event");
      tm =
      Types.VConstruct {constructor = "Scroll";
        args = [(Types.VConst (Types.CFloat 2.))]}
      }
    |}];
;;

let%expect_test "map 0" =
  let (name, iml_code, model) = load_artifact "composite" "map_int_bool_0" in
  print_info name iml_code;
  [%expect {|
    name: map_int_bool_0
    iml_code:
    let v : (int, bool) Map.t =
      Map.const false

    let v = fun w -> if w = v then true else false
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect {|
    { Types.Value_assignment.var_name = "w";
      ty = (Types.TApp ("Map.t", [(Types.TBase "int"); (Types.TBase "bool")]));
      tm =
      Types.VMap {default = (Types.VConst (Types.CBool false)); entries = []} }
    |}];
;;

let%expect_test "map 1" =
  let (name, iml_code, model) = load_artifact "composite" "map_int_bool_1" in
  print_info name iml_code;
  [%expect {|
    name: map_int_bool_1
    iml_code:
    let v : (int, bool) Map.t =
      Map.const false
       |> Map.add 2 true

    let v = fun w -> if w = v then true else false
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect {|
    { Types.Value_assignment.var_name = "w";
      ty = (Types.TApp ("Map.t", [(Types.TBase "int"); (Types.TBase "bool")]));
      tm =
      Types.VMap {default = (Types.VConst (Types.CBool false));
        entries =
        [((Types.VConst (Types.CInt 2)), (Types.VConst (Types.CBool true)))]}
      }
    |}];
;;

let%expect_test "map 2" =
  let (name, iml_code, model) = load_artifact "composite" "map_int_bool_2" in
  print_info name iml_code;
  [%expect {|
    name: map_int_bool_2
    iml_code:
    let v : (int, bool) Map.t =
      Map.const false
       |> Map.add 2 true
       |> Map.add 3 false

    let v = fun w -> if w = v then true else false
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect {|
    { Types.Value_assignment.var_name = "w";
      ty = (Types.TApp ("Map.t", [(Types.TBase "int"); (Types.TBase "bool")]));
      tm =
      Types.VMap {default = (Types.VConst (Types.CBool false));
        entries =
        [((Types.VConst (Types.CInt 2)), (Types.VConst (Types.CBool true)));
          ((Types.VConst (Types.CInt 3)), (Types.VConst (Types.CBool false)))]}
      }
    |}];
;;

let%expect_test "nonempty set" =
  let (name, iml_code, model) = load_artifact "composite" "set_nonempty" in
  print_info name iml_code;
  [%expect {|
    name: set_nonempty
    iml_code:
    let v = Set.of_list [1; 2; 3; 2; 1]

    let v = fun w -> if w = v then true else false
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect {|
    { Types.Value_assignment.var_name = "w";
      ty = (Types.TApp ("Map.t", [(Types.TBase "int"); (Types.TBase "bool")]));
      tm =
      Types.VMap {default = (Types.VConst (Types.CBool false));
        entries =
        [((Types.VConst (Types.CInt 1)), (Types.VConst (Types.CBool true)));
          ((Types.VConst (Types.CInt 3)), (Types.VConst (Types.CBool true)));
          ((Types.VConst (Types.CInt 2)), (Types.VConst (Types.CBool true)))]}
      }
    |}];
;;

let%expect_test "nonempty multiset" =
  let (name, iml_code, model) = load_artifact "composite" "multiset_nonempty" in
  print_info name iml_code;
  [%expect {|
    name: multiset_nonempty
    iml_code:
    let v = Multiset.of_list [1; 2; 3; 2; 1]

    let v = fun w -> if w = v then true else false
    |}];
  print_endline (Sir.Value_assignment.show (parse_model model));
  [%expect {|
    { Types.Value_assignment.var_name = "w";
      ty = (Types.TApp ("Map.t", [(Types.TBase "int"); (Types.TBase "int")]));
      tm =
      Types.VMap {default = (Types.VConst (Types.CInt 0));
        entries =
        [((Types.VConst (Types.CInt 1)), (Types.VConst (Types.CInt 2)));
          ((Types.VConst (Types.CInt 3)), (Types.VConst (Types.CInt 1)));
          ((Types.VConst (Types.CInt 2)), (Types.VConst (Types.CInt 2)))]}
      }
    |}];
;;

exception Json_error of string

type t =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of t list
  | Object of (string * t) list

let field (obj : t) (name : string) : t =
  match obj with
  | Object fields ->
    (match List.assoc_opt name fields with
     | Some v -> v
     | None -> raise (Json_error ("missing field: " ^ name)))
  | _ -> raise (Json_error "expected object")

let to_float (j : t) : float =
  match j with
  | Number f -> f
  | _ -> raise (Json_error "expected number")

let to_int (j : t) : int =
  match j with
  | Number f -> int_of_float f
  | _ -> raise (Json_error "expected number")

let to_string (j : t) : string =
  match j with
  | String s -> s
  | _ -> raise (Json_error "expected string")

let to_bool (j : t) : bool =
  match j with
  | Bool b -> b
  | _ -> raise (Json_error "expected bool")

let to_list (j : t) : t list =
  match j with
  | Array l -> l
  | _ -> raise (Json_error "expected array")


(* Parser
   ====== *)

type parse_state = { buf: string; mutable pos: int }

let peek st = if st.pos < String.length st.buf then st.buf.[st.pos] else '\000'
let advance st = st.pos <- st.pos + 1
let consume st = let c = peek st in advance st; c

let skip_ws st =
  while st.pos < String.length st.buf &&
        let c = st.buf.[st.pos] in c = ' ' || c = '\t' || c = '\n' || c = '\r'
  do advance st done

let expect st c =
  skip_ws st;
  if peek st <> c then
    raise (Json_error (Printf.sprintf "expected '%c' at pos %d" c st.pos));
  advance st

let rec parse_value st : t =
  skip_ws st;
  match peek st with
  | '"' -> parse_string st
  | '{' -> parse_object st
  | '[' -> parse_array st
  | 'n' -> parse_null st
  | 't' -> parse_true st
  | 'f' -> parse_false st
  | '-' | '0'..'9' -> parse_number st
  | c -> raise (Json_error (Printf.sprintf "unexpected char '%c' at pos %d" c st.pos))

and parse_string st : t =
  advance st;
  let b = Buffer.create 32 in
  let rec loop () =
    match consume st with
    | '"' -> String (Buffer.contents b)
    | '\\' ->
      (match consume st with
       | '"' -> Buffer.add_char b '"'; loop ()
       | '\\' -> Buffer.add_char b '\\'; loop ()
       | '/' -> Buffer.add_char b '/'; loop ()
       | 'n' -> Buffer.add_char b '\n'; loop ()
       | 't' -> Buffer.add_char b '\t'; loop ()
       | 'r' -> Buffer.add_char b '\r'; loop ()
       | _ -> raise (Json_error "invalid escape"))
    | c -> Buffer.add_char b c; loop ()
  in
  loop ()

and parse_object st : t =
  advance st;
  skip_ws st;
  if peek st = '}' then (advance st; Object [])
  else
    let rec loop acc =
      skip_ws st;
      let key = match parse_string st with String s -> s | _ -> assert false in
      expect st ':';
      let v = parse_value st in
      let acc = (key, v) :: acc in
      skip_ws st;
      if peek st = ',' then (advance st; loop acc)
      else (expect st '}'; Object (List.rev acc))
    in
    loop []

and parse_array st : t =
  advance st;
  skip_ws st;
  if peek st = ']' then (advance st; Array [])
  else
    let rec loop acc =
      let v = parse_value st in
      let acc = v :: acc in
      skip_ws st;
      if peek st = ',' then (advance st; loop acc)
      else (expect st ']'; Array (List.rev acc))
    in
    loop []

and parse_null st : t =
  for _ = 1 to 4 do advance st done; Null

and parse_true st : t =
  for _ = 1 to 4 do advance st done; Bool true

and parse_false st : t =
  for _ = 1 to 5 do advance st done; Bool false

and parse_number st : t =
  let start = st.pos in
  if peek st = '-' then advance st;
  while st.pos < String.length st.buf &&
        let c = st.buf.[st.pos] in
        (c >= '0' && c <= '9') || c = '.' || c = 'e' || c = 'E' || c = '+' || c = '-'
  do advance st done;
  let s = String.sub st.buf start (st.pos - start) in
  Number (float_of_string s)

let parse (s : string) : t =
  let st = { buf = s; pos = 0 } in
  parse_value st


(* Emitter
   ======= *)

let emit (j : t) : string =
  let b = Buffer.create 128 in
  let rec go = function
    | Null -> Buffer.add_string b "null"
    | Bool true -> Buffer.add_string b "true"
    | Bool false -> Buffer.add_string b "false"
    | Number f -> Buffer.add_string b (Printf.sprintf "%.17g" f)
    | String s ->
      Buffer.add_char b '"';
      String.iter (fun c ->
        match c with
        | '"' -> Buffer.add_string b "\\\""
        | '\\' -> Buffer.add_string b "\\\\"
        | '\n' -> Buffer.add_string b "\\n"
        | '\t' -> Buffer.add_string b "\\t"
        | '\r' -> Buffer.add_string b "\\r"
        | c -> Buffer.add_char b c) s;
      Buffer.add_char b '"'
    | Array l ->
      Buffer.add_char b '[';
      List.iteri (fun i v -> if i > 0 then Buffer.add_char b ','; go v) l;
      Buffer.add_char b ']'
    | Object fields ->
      Buffer.add_char b '{';
      List.iteri (fun i (k, v) ->
        if i > 0 then Buffer.add_char b ',';
        go (String k); Buffer.add_char b ':'; go v) fields;
      Buffer.add_char b '}'
  in
  go j;
  Buffer.contents b

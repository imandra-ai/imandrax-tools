(** Host entry point for Six_swiss.match_price.

    Reads one JSON object on stdin, writes one JSON value on stdout. Both the
    exact and float codecs are compiled in and selected per call by the
    "encoding" field, so a single binary serves both kinds of caller:

    - "exact" (default): [int] and [real] travel as strings in their canonical
      forms -- "42" and "3/2". Lossless, and the only correct choice for IML,
      whose [int] is arbitrary-precision and whose [real] is a rational.
    - "float": [int] and [real] travel as JSON numbers. Lossy -- integers
      beyond 2^53 and any non-dyadic rational are approximated -- but
      convenient for callers that just want floats.

    Deliberately no [open Iml_prelude] here: the prelude shadows [int], [(=)]
    and friends, and this module needs Stdlib semantics for its own plumbing. *)

module Z = Iml_prelude.Z
module Q = Iml_prelude.Q

type encoding =
  | Exact
  | Float

let encoding_of_json (j : Json.t) : encoding =
  match Json.field_opt j "encoding" with
  | None -> Exact
  | Some e ->
    (match Json.to_string e with
     | "exact" -> Exact
     | "float" -> Float
     | s -> raise (Json.Json_error ("unknown encoding: " ^ s)))

(* Scalar codecs
   ============== *)

let int_of_json enc (j : Json.t) : Iml_prelude.int =
  match enc with
  | Exact -> Z.of_string (Json.to_string j)
  | Float -> Z.of_int (Json.to_int j)

let json_of_int enc (z : Iml_prelude.int) : Json.t =
  match enc with
  | Exact -> Json.String (Z.to_string z)
  | Float -> Json.Number (Z.to_float z)

let real_of_json enc (j : Json.t) : Iml_prelude.real =
  match enc with
  | Exact -> Q.of_string (Json.to_string j)
  | Float -> Q.of_float (Json.to_float j)

let json_of_real enc (r : Iml_prelude.real) : Json.t =
  match enc with
  | Exact -> Json.String (Q.to_string r)
  | Float -> Json.Number (Q.to_float r)

(* Decoders: JSON -> Six_swiss values
   =================================== *)

let order_type_of_json (j : Json.t) : Six_swiss.order_type =
  match Json.to_string j with
  | "market" -> Six_swiss.Market
  | "limit" -> Six_swiss.Limit
  | "quote" -> Six_swiss.Quote
  | s -> raise (Json.Json_error ("unknown order_type: " ^ s))

let order_of_json enc (j : Json.t) : Six_swiss.order =
  { Six_swiss.order_id = int_of_json enc (Json.field j "order_id")
  ; order_type = order_type_of_json (Json.field j "order_type")
  ; order_qty = int_of_json enc (Json.field j "order_qty")
  ; order_price = real_of_json enc (Json.field j "order_price")
  ; order_time = int_of_json enc (Json.field j "order_time")
  }

let order_book_of_json enc (j : Json.t) : Six_swiss.order_book =
  { Six_swiss.buys = Stdlib.List.map (order_of_json enc) (Json.to_list (Json.field j "buys"))
  ; sells = Stdlib.List.map (order_of_json enc) (Json.to_list (Json.field j "sells"))
  }

(* Encoders: Six_swiss values -> JSON
   =================================== *)

let json_of_fill_price enc (fp : Six_swiss.fill_price) : Json.t =
  match fp with
  | None -> Json.Null
  | Some r -> json_of_real enc r

(* Entry point
   ============ *)

let read_all_stdin () =
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_channel buf Stdlib.stdin 1
     done
   with End_of_file -> ());
  Buffer.contents buf

let () =
  let json = Json.parse (read_all_stdin ()) in
  let enc = encoding_of_json json in
  let ob = order_book_of_json enc (Json.field json "order_book") in
  let ref_price = real_of_json enc (Json.field json "ref_price") in
  let result = Six_swiss.match_price ob ref_price in
  print_string (Json.emit (json_of_fill_price enc result))

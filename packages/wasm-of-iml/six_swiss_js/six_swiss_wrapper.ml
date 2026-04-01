open Imandrax_api_prelude

(* Deserialization: JSON -> Six_swiss types
   ========================================= *)

let order_type_of_json (j : Json.t) : Six_swiss.order_type =
  match Json.to_string j with
  | "market" -> Market
  | "limit" -> Limit
  | "quote" -> Quote
  | s -> raise (Json.Json_error ("unknown order_type: " ^ s))

let order_of_json (j : Json.t) : Six_swiss.order =
  { order_id = Z.of_int (Json.to_int (Json.field j "order_id"));
    order_type = order_type_of_json (Json.field j "order_type");
    order_qty = Z.of_int (Json.to_int (Json.field j "order_qty"));
    order_price = Q.of_float (Json.to_float (Json.field j "order_price"));
    order_time = Z.of_int (Json.to_int (Json.field j "order_time")) }

let order_book_of_json (j : Json.t) : Six_swiss.order_book =
  { buys = Stdlib.List.map order_of_json (Json.to_list (Json.field j "buys"));
    sells = Stdlib.List.map order_of_json (Json.to_list (Json.field j "sells")) }


(* Serialization: Six_swiss types -> JSON
   ======================================= *)

let json_of_fill_price (fp : Six_swiss.fill_price) : Json.t =
  match fp with
  | None -> Null
  | Some r -> Number (Q.to_float r)


(* Main
   ===== *)

let () =
  let buf = Stdlib.Buffer.create 4096 in
  (try while true do
     Stdlib.Buffer.add_char buf (Stdlib.input_char Stdlib.stdin)
   done with Stdlib.End_of_file -> ());
  let input = Stdlib.Buffer.contents buf in
  let json = Json.parse input in
  let ob = order_book_of_json (Json.field json "order_book") in
  let ref_price = Q.of_float (Json.to_float (Json.field json "ref_price")) in
  let result = Six_swiss.match_price ob ref_price in
  Stdlib.print_string (Json.emit (json_of_fill_price result))

(* extracted by imandrax-extract from "six_swiss.iml" *)

open Imandrax_api_prelude
#1 "/Users/linhongyu/Documents/playground/demo-mcp/six_swiss/six_swiss.iml"
type order_type =
  | Market 
  | Limit 
  | Quote 
#6 "/Users/linhongyu/Documents/playground/demo-mcp/six_swiss/six_swiss.iml"
type order =
  {
  order_id: int ;
  order_type: order_type ;
  order_qty: int ;
  order_price: real ;
  order_time: int }
#14 "/Users/linhongyu/Documents/playground/demo-mcp/six_swiss/six_swiss.iml"
type fill_price = real option
#16 "/Users/linhongyu/Documents/playground/demo-mcp/six_swiss/six_swiss.iml"
let older_price (o1 : order) (o2 : order) : real=
  if o1.order_time > o2.order_time then o2.order_price else o1.order_price
#22 "/Users/linhongyu/Documents/playground/demo-mcp/six_swiss/six_swiss.iml"
type order_book = {
  buys: order list ;
  sells: order list }
#27 "/Users/linhongyu/Documents/playground/demo-mcp/six_swiss/six_swiss.iml"
let best_buy (ob : order_book) : order option=
  match ob.buys with | [] -> None | hd::_ -> Some hd
#32 "/Users/linhongyu/Documents/playground/demo-mcp/six_swiss/six_swiss.iml"
let best_sell (ob : order_book) : order option=
  match ob.sells with | [] -> None | hd::_ -> Some hd
#37 "/Users/linhongyu/Documents/playground/demo-mcp/six_swiss/six_swiss.iml"
let next_buy (ob : order_book) : order option=
  match ob.buys with | [] | _::[] -> None | _::second::_ -> Some second
#42 "/Users/linhongyu/Documents/playground/demo-mcp/six_swiss/six_swiss.iml"
let next_sell (ob : order_book) : order option=
  match ob.sells with | [] | _::[] -> None | _::second::_ -> Some second
#47 "/Users/linhongyu/Documents/playground/demo-mcp/six_swiss/six_swiss.iml"
let get_next_order_prices (ob : order_book) : (real option * real option)=
  let next_buy_order = next_buy ob in
  let b_bid =
    match next_buy_order with
    | Some order when order.order_type <> Market -> Some (order.order_price)
    | _ -> None in
  let next_sell_order = next_sell ob in
  let b_ask =
    match next_sell_order with
    | Some order when order.order_type <> Market -> Some (order.order_price)
    | _ -> None in
  (b_bid, b_ask)
#62 "/Users/linhongyu/Documents/playground/demo-mcp/six_swiss/six_swiss.iml"
let calculate_market_market_fill_price (b_bid : real option)
  (b_ask : real option) (ref_price : real) : real=
  match (b_bid, b_ask) with
  | (None, None) -> ref_price
  | (None, Some ask) -> if ask <. ref_price then ask else ref_price
  | (Some bid, None) -> if bid >. ref_price then bid else ref_price
  | (Some bid, Some ask) ->
      if bid >. ref_price
      then bid
      else if ask <. ref_price then ask else ref_price
#75 "/Users/linhongyu/Documents/playground/demo-mcp/six_swiss/six_swiss.iml"
let handle_market_market_orders (bb : order) (bs : order) (ob : order_book)
  (ref_price : real) : fill_price=
  if bb.order_qty <> bs.order_qty
  then None
  else
    (let (b_bid, b_ask) = get_next_order_prices ob in
     Some (calculate_market_market_fill_price b_bid b_ask ref_price))
#82 "/Users/linhongyu/Documents/playground/demo-mcp/six_swiss/six_swiss.iml"
let handle_quote_limit_orders (bb : order) (bs : order) (ob : order_book) :
  fill_price=
  if bb.order_time > bs.order_time
  then
    (if bb.order_qty < bs.order_qty
     then Some (bs.order_price)
     else
       if bb.order_qty = bs.order_qty
       then
         (match next_sell ob with
          | Some next_sell_order -> Some (next_sell_order.order_price)
          | None -> Some (bb.order_price))
       else None)
  else Some (bb.order_price)
#95 "/Users/linhongyu/Documents/playground/demo-mcp/six_swiss/six_swiss.iml"
let handle_quote_market_orders (bb : order) (bs : order) (ob : order_book) :
  fill_price=
  if bb.order_time > bs.order_time
  then
    let next_sell_limit = next_sell ob in
    (if bb.order_qty < bs.order_qty
     then Some (bs.order_price)
     else
       if bb.order_qty = bs.order_qty
       then
         (match next_sell_limit with
          | Some order -> Some (order.order_price)
          | None -> Some (bb.order_price))
       else None)
  else Some (bb.order_price)
#109 "/Users/linhongyu/Documents/playground/demo-mcp/six_swiss/six_swiss.iml"
let handle_limit_quote_orders (bb : order) (bs : order) (ob : order_book) :
  fill_price=
  if bb.order_time > bs.order_time
  then
    (if bs.order_qty < bb.order_qty
     then Some (bb.order_price)
     else
       if bs.order_qty = bb.order_qty
       then
         (match next_buy ob with
          | Some next_buy_order -> Some (next_buy_order.order_price)
          | None -> Some (bs.order_price))
       else None)
  else Some (bs.order_price)
#122 "/Users/linhongyu/Documents/playground/demo-mcp/six_swiss/six_swiss.iml"
let handle_market_quote_orders (bb : order) (bs : order) (ob : order_book) :
  fill_price=
  if bb.order_time > bs.order_time
  then
    (if bs.order_qty < bb.order_qty
     then Some (bs.order_price)
     else
       if bb.order_qty = bs.order_qty
       then
         (match next_buy ob with
          | Some next_buy_limit -> Some (next_buy_limit.order_price)
          | None -> Some (bs.order_price))
       else None)
  else Some (bs.order_price)
#135 "/Users/linhongyu/Documents/playground/demo-mcp/six_swiss/six_swiss.iml"
let match_price (ob : order_book) (ref_price : real) : fill_price=
  match ((best_buy ob), (best_sell ob)) with
  | (None, _) | (_, None) -> None
  | (Some bb, Some bs) ->
      let (bb_type, bs_type) = ((bb.order_type), (bs.order_type)) in
      (match (bb_type, bs_type) with
       | (Limit, Limit) | (Quote, Quote) -> Some (older_price bb bs)
       | (Market, Market) -> handle_market_market_orders bb bs ob ref_price
       | (Market, Limit) -> Some (bs.order_price)
       | (Limit, Market) -> Some (bb.order_price)
       | (Quote, Limit) -> handle_quote_limit_orders bb bs ob
       | (Quote, Market) -> handle_quote_market_orders bb bs ob
       | (Limit, Quote) -> handle_limit_quote_orders bb bs ob
       | (Market, Quote) -> handle_market_quote_orders bb bs ob)

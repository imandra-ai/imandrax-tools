"""Python bindings for Six_swiss.match_price via js_of_ocaml."""

import json
import subprocess
import os
from dataclasses import dataclass
from enum import Enum
from typing import Optional


class OrderType(Enum):
    MARKET = "market"
    LIMIT = "limit"
    QUOTE = "quote"


@dataclass
class Order:
    order_id: int
    order_type: OrderType
    order_qty: int
    order_price: float
    order_time: int


@dataclass
class OrderBook:
    buys: list[Order]
    sells: list[Order]


FillPrice = Optional[float]

# Path to the compiled JS wrapper
_JS_WRAPPER = os.path.join(
    os.path.dirname(__file__),
    "..", "..", "..", "_build", "default",
    "packages", "wasm-of-iml", "six_swiss_js",
    "six_swiss_wrapper.bc.js",
)


def _order_to_dict(o: Order) -> dict:
    return {
        "order_id": o.order_id,
        "order_type": o.order_type.value,
        "order_qty": o.order_qty,
        "order_price": o.order_price,
        "order_time": o.order_time,
    }


def _order_book_to_dict(ob: OrderBook) -> dict:
    return {
        "buys": [_order_to_dict(o) for o in ob.buys],
        "sells": [_order_to_dict(o) for o in ob.sells],
    }


def match_price(ob: OrderBook, ref_price: float) -> FillPrice:
    payload = json.dumps({
        "order_book": _order_book_to_dict(ob),
        "ref_price": ref_price,
    })
    result = subprocess.run(
        ["node", _JS_WRAPPER],
        input=payload, capture_output=True, text=True, check=True,
    )
    parsed = json.loads(result.stdout)
    return parsed

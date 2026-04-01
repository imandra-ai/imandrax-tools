"""
Python bindings for Six_swiss.match_price via the slim (num-based) prelude.

Transports `int` and `real` in their exact canonical forms -- "42" and "3/2" --
so nothing is lost across the boundary. IML's `int` is arbitrary-precision and
its `real` is a rational, neither of which survives a JSON float.

The dataclasses are imported from the reference `six_swiss_js` module rather
than redefined, so both backends are driven through one set of types.
"""

import json
import os
import subprocess
import sys
from fractions import Fraction

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'six_swiss_js'))

from six_swiss_js import Order, OrderBook, OrderType  # type: ignore

__all__ = ['Order', 'OrderBook', 'OrderType', 'match_price', 'Real']

Real = int | float | Fraction

_REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))
_WRAPPER = os.path.join(
    _REPO_ROOT,
    '_build',
    'default',
    'packages',
    'wasm-of-iml',
    'six_swiss_slim',
    'six_swiss_wrapper.exe',
)

# The JSON protocol is identical whichever way the wrapper is executed, so the
# command is overridable to run the same cases against a different backend:
#   SIX_SWISS_SLIM_CMD='wasmtime /path/to/code.wasm'   (standalone WASI)
#   SIX_SWISS_SLIM_CMD='node /path/to/wrapper.bc.js'   (js_of_ocaml)
_CMD = os.environ.get('SIX_SWISS_SLIM_CMD', '').split() or [_WRAPPER]


def _to_exact(v: Real) -> str:
    """
    Canonical IML `real`: "n/d", or a bare "n" when the denominator is 1.

    Fraction(float) is exact, so a float argument is converted the way IML's
    Real.of_float does -- to the rational the float actually denotes, not to a
    prettier decimal approximation of it.
    """
    f = Fraction(v)
    return str(f.numerator) if f.denominator == 1 else f'{f.numerator}/{f.denominator}'


def _order_to_dict(o: 'Order') -> dict[str, str]:
    return {
        'order_id': str(int(o.order_id)),
        'order_type': o.order_type.value,
        'order_qty': str(int(o.order_qty)),
        'order_price': _to_exact(o.order_price),
        'order_time': str(int(o.order_time)),
    }


def match_price(ob: OrderBook, ref_price: Real) -> Fraction | None:
    payload = json.dumps(
        {
            'encoding': 'exact',
            'order_book': {
                'buys': [_order_to_dict(o) for o in ob.buys],
                'sells': [_order_to_dict(o) for o in ob.sells],
            },
            'ref_price': _to_exact(ref_price),
        }
    )
    result = subprocess.run(
        _CMD,
        input=payload,
        capture_output=True,
        text=True,
        check=True,
    )
    parsed = json.loads(result.stdout)
    return None if parsed is None else Fraction(parsed)

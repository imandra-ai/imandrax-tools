"""Recursion-limit handling for deeply nested terms."""

import sys
from collections.abc import Generator
from contextlib import contextmanager

# ImandraX terms nest arbitrarily deep, and both the generated `imandrax_api.lib`
# twine decoders and this package's pretty-printer walk them recursively.
# CPython's default limit of
# 1000 frames therefore caps handling at ~200 levels.
#
# Python-to-Python calls do not consume C stack (3.11+), so a limit this size does
# not risk a stack overflow on the default 8MB thread stack.
# 50k ≈ 10,000 levels of term nesting at ~5 frames per level.
DEEP_RECURSION_LIMIT: int = 50_000


@contextmanager
def raise_rec_limit() -> Generator[None, None, None]:
    """
    Temporarily raise the recursion limit for a synchronous walk over a term.

    - Never lowers an already-higher limit, and restores the previous value on exit.
    - The body must not `await`: the recursion limit is process-global, so another
    coroutine resuming inside this block would observe the raised value.
    """
    prev = sys.getrecursionlimit()
    if prev >= DEEP_RECURSION_LIMIT:
        yield
        return
    sys.setrecursionlimit(DEEP_RECURSION_LIMIT)
    try:
        yield
    finally:
        sys.setrecursionlimit(prev)

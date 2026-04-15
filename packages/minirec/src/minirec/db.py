"""
A database of IML source code and eval result that is cached with snapshot + env trick.

Set UNTRUSTING=1 and run pytest to update the snapshot cache.

"""

from __future__ import annotations

import os

from imandrax_api import url_prod
from imandrax_api_models import Error, ErrorMessage, EvalRes, Location, Position
from imandrax_api_models.client import ImandraXClient, get_imandrax_api_key
from inline_snapshot import get_snapshot_value, snapshot

UNTRUSTING = os.environ.get('UNTRUSTING') in ['1', 'true', 'True']


def _eval_src(src: str) -> EvalRes:
    c = ImandraXClient(url=url_prod, auth_token=get_imandrax_api_key())
    return c.eval_src(src)


# ====================


IML_INFIX_OPERATOR = """\
let land : int -> int -> int = fun (x : int) (y : int) -> 0 [@@opaque]
"""


def trust() -> EvalRes:
    iml = IML_INFIX_OPERATOR

    ss: EvalRes = snapshot(
        EvalRes(
            success=False,
            errors=[
                Error(
                    msg=ErrorMessage(
                        msg='syntax error',
                        locs=[
                            Location(
                                file='<none>',
                                start=Position(line=1, col=5),
                                stop=Position(line=1, col=8),
                            )
                        ],
                        backtrace="""\
Raised at Imandrax_ocaml_parse_base__Imandrax_parse.Make.wrap in file "src/ocaml-parse/base/imandrax_parse.ml", line 80, characters 4-49
Called from Imandrax_ocaml_parse_base__Imandrax_parse.Make.wrap_and_rw.(fun) in file "src/ocaml-parse/base/imandrax_parse.ml", line 142, characters 6-32
""",
                    ),
                    kind='{ Kind.name = "SyntaxErr" }',
                    process='imandrax-server',
                )
            ],
        )
    )
    if UNTRUSTING:
        assert ss == _eval_src(iml)

    return get_snapshot_value(ss)


EVAL_RES_INFIX_OPERATOR = trust()

# ====================

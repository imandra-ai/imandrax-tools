import os

import pytest
from google.protobuf.message import Message
from imandrax_api import Client, url_prod
from inline_snapshot import snapshot

from imandrax_api_models.proto_models import EvalRes
from imandrax_api_models.proto_utils import proto_to_dict


@pytest.fixture
def c() -> Client:
    c = Client(url=url_prod, auth_token=os.environ['IMANDRAX_API_KEY'])
    return c


def test_eval_res(c: Client):
    src = 'let x = 1'
    eval_res_msg: Message = c.eval_src(src)
    eval_res = EvalRes.model_validate(proto_to_dict(eval_res_msg))
    assert eval_res == snapshot(EvalRes(success=True))


def test_eval_res_(c: Client):
    src = 'let x = 1'
    eval_res_msg: Message = c.eval_src(src)
    eval_res = EvalRes.model_validate(eval_res_msg)
    assert eval_res == snapshot(EvalRes(success=True))

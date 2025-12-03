# pyright: basic
# %%
import base64
import json
from pathlib import Path

from imandrax_api import Client, url_dev, url_prod
from imandrax_api.lib import read_artifact_data
from IPython.core.getipython import get_ipython
from rich import print

if ip := get_ipython():
    ip.run_line_magic('reload_ext', 'autoreload')
    ip.run_line_magic('autoreload', '2')
import os
from typing import Any

import dotenv
from google.protobuf.json_format import MessageToDict
from google.protobuf.message import Message


def proto_to_dict(proto_obj: Message) -> dict[Any, Any]:
    return MessageToDict(
        proto_obj,
        preserving_proto_field_name=True,
        always_print_fields_with_no_presence=True,
    )


dotenv.load_dotenv('../.env')


# %%
c = Client(
    auth_token=os.environ['IMANDRAX_API_KEY'],
    url=url_dev,
)

# %%
iml = """\
type shape =
| Point
| Circle of int
| Rectangle of int * int
| Triangle of {a: int; b: int; c: int}"""


name = 'movement'
eval_res = c.eval_src(iml)
gd_res_proto = c.get_decls(names=['shape'])
gd_res = proto_to_dict(gd_res_proto)

# %%
gd_res

# %%
art = gd_res['decls'][0]['artifact']
art_data = base64.b64decode(art['data'])
art_kind = art['kind']
print(read_artifact_data(art_data, art_kind))

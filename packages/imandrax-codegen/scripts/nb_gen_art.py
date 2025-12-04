# pyright: basic
# %%
import base64
import json
from pathlib import Path

from imandrax_api import Client, url_dev, url_prod
from imandrax_api.lib import read_artifact_data
from imandrax_api_models.client import ImandraXAsyncClient, ImandraXClient
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
c = ImandraXClient(
    auth_token=os.environ['IMANDRAX_API_KEY'],
    url=url_dev,
)

# %%
iml = """\
type direction = North | South | East | West

type position = { x: int; y: int; z: real }

type movement =
  | Stay of position
  | Move of position * direction

let move = fun w ->
  match w with
  | Stay p -> p
  | Move (p, d) ->
    let x, y, z = p.x, p.y, p.z in
    let x, y, z =
      match d with
      | North -> (x, y+1, z)
      | South -> (x, y-1, z)
      | East -> (x+1, y, z)
      | West -> (x-1, y, z)
    in
    { x; y; z }\
"""

name = 'move'
eval_res = c.eval_src(iml)

# %%
tc_res = c.typecheck(iml)
print(tc_res)

# %%

gd_res = c.get_decls(names=['movement'])
gd_res


# %%
gd_res

# %%
art = gd_res['decls'][0]['artifact']
art_data = base64.b64decode(art['data'])
art_kind = art['kind']
print(read_artifact_data(art_data, art_kind))

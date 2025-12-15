# pyright: basic
# %%
import base64
import json
from pathlib import Path

import yaml
from imandrax_api import Client, url_dev, url_prod
from imandrax_api.lib import read_artifact_data
from IPython.core.getipython import get_ipython
from rich import print
from rich.progress import track

if ip := get_ipython():
    ip.run_line_magic('reload_ext', 'autoreload')
    ip.run_line_magic('autoreload', '2')
import os
from typing import Any, Final

import dotenv
from google.protobuf.json_format import MessageToDict
from google.protobuf.message import Message

curr_dir = Path.cwd() if ip else Path(__file__).parent
dotenv.load_dotenv()


# %%
class LiteralString(str):
    pass


def literal_presenter(dumper, data):
    if '\n' in data:
        return dumper.represent_scalar('tag:yaml.org,2002:str', data, style='|')
    return dumper.represent_scalar('tag:yaml.org,2002:str', data)


yaml.add_representer(LiteralString, literal_presenter)


def proto_to_dict(proto_obj: Message) -> dict[Any, Any]:
    return MessageToDict(
        proto_obj,
        preserving_proto_field_name=True,
        always_print_fields_with_no_presence=True,
    )


# %%
c = Client(auth_token=os.environ['IMANDRAX_API_KEY'], url=url_dev)

out_dir = curr_dir
with (curr_dir / 'get_decl_inputs.yaml').open('r') as f:
    inputs = next(yaml.safe_load_all(f))

# %%
item = inputs[0]

# %%
iml = item['iml']
get_decls_kwargs = item['req_kwargs']
_eval_res = c.eval_src(iml)
c.get_decls(**get_decls_kwargs)


# %%
res_by_item = []
for item in track(inputs):
    iml = item['iml']
    get_decls_kwargs = item['req_kwargs']
    _eval_res = c.eval_src(iml)
    get_decls_res = c.get_decls(**get_decls_kwargs)
    res_by_item.append(proto_to_dict(get_decls_res))


# %%
out_dir = curr_dir / 'result'
out_dir.mkdir(exist_ok=True)
for item, res in zip(inputs, res_by_item, strict=True):
    name = item['name'].replace(' ', '_')
    out = item | {'get_decls_res': res}
    out['iml'] = LiteralString(item['iml'])  # format IML as literal string
    with (out_dir / f'{item["name"]}.yaml').open('w') as f:
        f.write(yaml.dump(out, sort_keys=False, default_flow_style=False))

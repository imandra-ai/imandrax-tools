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


# %% Utils
# ====================


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
inputs = (curr_dir / 'gen_model_inputs.yaml').read_text()
inputs = yaml.safe_load(inputs)
c = Client(auth_token=os.environ['IMANDRAX_API_KEY'], url=url_dev)


# %%
def gen_art(name: str, iml: str) -> dict[str, Any]:
    _eval_res = c.eval_src(iml)
    instance_res = c.instance_src(src='v')
    instance_res = proto_to_dict(instance_res)
    art = instance_res['sat']['model']['artifact']
    order = [
        'data',
        'api_version',
        'kind',
        'storage',
    ]
    return {k: art[k] for k in order}


# %%
results: list[dict[str, Any]] = []
for input_item in track(inputs):
    name = input_item['name']
    iml = input_item['iml']
    try:
        art = gen_art(name, iml)
    except KeyError as _:
        print(f'Failed to generate model for {name}')
        continue
    saved_input = input_item.copy()
    saved_input['iml'] = LiteralString(iml)
    results.append(saved_input | art)

# %% Write to disk
out_dir = curr_dir / 'result'
for res in results:
    file_name = res['name'].replace(' ', '_')
    sub_dir = res.get('category', None)
    if sub_dir:
        write_path = out_dir / sub_dir / f'{file_name}.yaml'
    else:
        write_path = out_dir / f'{file_name}.yaml'
    write_path.parent.mkdir(exist_ok=True, parents=True)
    with write_path.open('w') as f:
        f.write(yaml.dump(res, sort_keys=False, default_flow_style=False))

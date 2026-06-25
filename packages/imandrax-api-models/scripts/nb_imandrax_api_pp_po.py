# pyright: reportUnusedExpression=false
# %%
from typing import cast

from IPython.core.getipython import get_ipython

from imandrax_api_models import Art
from imandrax_api_models.proto_models.api import Artifact

if ip := get_ipython():
    ip.run_line_magic('reload_ext', 'autoreload')
    ip.run_line_magic('autoreload', '2')

from pathlib import Path

CURR_DIR = Path.cwd() if ip else Path(__file__).parent

import os

import dotenv
import imandrax_api
import imandrax_api.lib as xtype
from imandrax_api.lib import twine
from imandrax_tools.pp import pretty as Pp
from imandrax_tools.pp.xtype import show_value

from imandrax_api_models.client import ImandraXClient
from imandrax_api_models.context_utils import format_eval_res

dotenv.load_dotenv()

# %%
c = ImandraXClient(
    # c = imandrax_api.Client(
    # url=imandrax_api.url_prod,
    url='http://localhost:8089',
    # url=imandrax_api.url_dev,
    auth_token=os.environ['IMANDRAX_API_KEY'],
)

# %%
IML_CODE = """\
let rec f x =
    if x > 0 then f (x - 1)
    else 0

let rec f2 (a : int) (xs : int list) : int =
  if a <= 0 then 0
  else match xs with
    | [] -> f2 (a - 1) [1; 2; 3]
    | _ :: tl -> f2 a tl

verify (f 10 = 0)

instance (fun x -> f x >= 0)
"""

eval_res = c.eval_src(IML_CODE)

# %%
task = eval_res.po_results[3].task
assert task is not None
kinds = c.list_artifacts(task).kinds
print(kinds)

arts: dict[str, Art] = {}
for kind in kinds:
    arts[kind] = c.get_artifact(task=task, kind=kind).art

# %%
data = arts['report'].data
d = twine.Decoder(data)
report = cast(
    xtype.Report_Report,
    xtype.artifact_decoders['report'](d, d.entrypoint()),
)

data = arts['po_res'].data
d = twine.Decoder(data)
po_res = cast(
    xtype.Tasks_PO_res_Shallow,
    xtype.artifact_decoders['po_res'](d, d.entrypoint()),
)

data = arts['po_task'].data
d = twine.Decoder(data)
po_task = cast(
    xtype.Tasks_PO_task_Mir,
    xtype.artifact_decoders['po_task'](d, d.entrypoint()),
)

data = arts['show'].data
d = twine.Decoder(data)
show = cast(
    str,
    xtype.artifact_decoders['show'](d, d.entrypoint()),
)

po_result = {
    'show': show,
    'po_task': po_task,
    'po_res': po_res,
    'report': report,
}


def show_sep(t):
    print()
    print(t)
    print()
    print('=' * 20)
    print()


show_sep('PO Task')
print(show_value(po_task))
show_sep('PO Result')
print(show_value(po_res))
show_sep('Report')
print(show_value(report))

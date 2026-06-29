# %%
from IPython.core.getipython import get_ipython

if ip := get_ipython():
    ip.run_line_magic('reload_ext', 'autoreload')
    ip.run_line_magic('autoreload', '2')

from pathlib import Path

CURR_DIR = Path.cwd() if ip else Path(__file__).parent

import os

import dotenv
import imandrax_api

from imandrax_api_models.client import ImandraXClient
from imandrax_api_models.region_decomp import EnrichedDecomposeRes, RegionGroup

dotenv.load_dotenv()

# %%
c = ImandraXClient(
    url=imandrax_api.url_prod,
    # url=imandrax_api.url_dev,
    auth_token=os.environ['IMANDRAX_API_KEY'],
)

IML = """
let classify_triangle (a: int) (b: int) (c: int) : string =
  (if (((a <= 0) || (b <= 0)) || (c <= 0)) then "invalid" else (if ((((a + b) <= c) || ((a + c) <= b)) || ((b + c) <= a)) then "invalid" else (if ((a = b) && (b = c)) then "equilateral" else (if (((a = b) || (b = c)) || (a = c)) then "isosceles" else "scalene"))))

let is_leap_year (year: int) : bool =
  (if ((year mod 400) = 0) then true else (if ((year mod 100) = 0) then false else ((year mod 4) = 0)))

let days_in_month (year: int) (month: int) : int =
  (if ((month < 1) || (month > 12)) then 0 else (if (month = 2) then (if (is_leap_year year) then 29 else 28) else (if ((((month = 4) || (month = 6)) || (month = 9)) || (month = 11)) then 30 else 31)))

let is_valid_date (year: int) (month: int) (day: int) : bool =
  (if (year < 1) then false else (let dim = (days_in_month year month) in
  (if (dim = 0) then false else ((1 <= day) && (day <= dim)))))

let normalize_percentage (value: real) : real =
  (if (value <> value) then 0.0 else (if (value <. 0.0) then 0.0 else (if (value >. 100.0) then 100.0 else value)))
[@@decomp top ~prune:true ()]
"""

_eval_res = c.eval_src(IML)
decomp_res = c.decompose(name='normalize_percentage', prune=True, string_results=True)

# %%
print(decomp_res.regions_str)


# %%
hdr = EnrichedDecomposeRes.from_decomp_res(decomp_res)
print(hdr.to_tree_str())

# %%
print(hdr.model_dump()['region_groups'])

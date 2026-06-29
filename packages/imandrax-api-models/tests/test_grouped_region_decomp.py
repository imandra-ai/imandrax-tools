import os

import imandrax_api
from imandrax_api.lib import RegionStr
from inline_snapshot import snapshot

from imandrax_api_models.proto_models import DecomposeRes
from imandrax_api_models.region_decomp import HumDecomposeRes, RegionGroup


def trust() -> DecomposeRes:
    import dotenv

    from imandrax_api_models.client import ImandraXClient

    dotenv.load_dotenv()
    c = ImandraXClient(
        url=imandrax_api.url_prod,
        auth_token=os.environ['IMANDRAX_API_KEY'],
    )
    IML_CODE = """\
    let classify (x : int) (y : int) : int =
    if x > 0 then
        if y > 0 then
        if x > y then 1
        else 2
        else
        if y < -10 then 3
        else 4
    else
        if y > 0 then 5
        else 6"""
    _eval_res = c.eval_src(IML_CODE)
    return c.decompose(name='classify', string_results=True, prune=True)


def test():
    decomp_res = trust()
    hdr = HumDecomposeRes.from_decomp_res(decomp_res)

    # The raw regions are inherited from DecomposeRes.
    assert hdr.regions_str == snapshot(
        [
            RegionStr(
                constraints_str=['y <= 0', 'x <= 0'],
                invariant_str='6',
                model_str={'x': '0', 'y': '0'},
                model_eval_str='6',
            ),
            RegionStr(
                constraints_str=['y >= 1', 'x <= 0'],
                invariant_str='5',
                model_str={'x': '0', 'y': '1'},
                model_eval_str='5',
            ),
            RegionStr(
                constraints_str=['y >= (-10)', 'y <= 0', 'x >= 1'],
                invariant_str='4',
                model_str={'x': '1', 'y': '0'},
                model_eval_str='4',
            ),
            RegionStr(
                constraints_str=['y <= (-11)', 'x >= 1'],
                invariant_str='3',
                model_str={'x': '1', 'y': '(-11)'},
                model_eval_str='3',
            ),
            RegionStr(
                constraints_str=['x <= y', 'y >= 1', 'x >= 1'],
                invariant_str='2',
                model_str={'x': '1', 'y': '1'},
                model_eval_str='2',
            ),
            RegionStr(
                constraints_str=['x > y', 'y >= 1', 'x >= 1'],
                invariant_str='1',
                model_str={'x': '2', 'y': '1'},
                model_eval_str='1',
            ),
        ]
    )

    # region_groups is auto-populated on validation from regions_str.
    assert hdr.to_tree_str() == snapshot("""\
├── [1] constraints[-1]=x >= 1 invariant=None (w=4, n_children=3, n_descendants=5)
│   ├── [1.1] constraints[-1]=y >= 1 invariant=None (w=2, n_children=2, n_descendants=2)
│   │   ├── [1.1.1] constraints[-1]=x <= y invariant='2' (w=1, n_children=0, n_descendants=0)
│   │   └── [1.1.2] constraints[-1]=x > y invariant='1' (w=1, n_children=0, n_descendants=0)
│   ├── [1.2] constraints[-1]=y <= (-11) invariant='3' (w=1, n_children=0, n_descendants=0)
│   └── [1.3.1] constraints[-1]=y >= (-10) invariant='4' (w=1, n_children=0, n_descendants=0)
├── [2.1] constraints[-1]=x <= 0 invariant='5' (w=1, n_children=0, n_descendants=0)
└── [3.1] constraints[-1]=y <= 0 invariant='6' (w=1, n_children=0, n_descendants=0)\
""")

    # (label_path, full constraint path) for every node, depth-first.
    def _walk(groups: list[RegionGroup]) -> list[tuple[str, list[str]]]:
        out: list[tuple[str, list[str]]] = []
        for g in groups:
            out.append(('.'.join(map(str, g.label_path)), g.constraints))
            out.extend(_walk(g.children))
        return out

    assert _walk(hdr.region_groups) == snapshot(
        [
            ('1', ['x >= 1']),
            ('1.1', ['x >= 1', 'y >= 1']),
            ('1.1.1', ['x >= 1', 'y >= 1', 'x <= y']),
            ('1.1.2', ['x >= 1', 'y >= 1', 'x > y']),
            ('1.2', ['x >= 1', 'y <= (-11)']),
            ('1.3.1', ['x >= 1', 'y <= 0', 'y >= (-10)']),
            ('2.1', ['y >= 1', 'x <= 0']),
            ('3.1', ['x <= 0', 'y <= 0']),
        ]
    )

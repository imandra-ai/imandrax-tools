from inline_snapshot import snapshot

from iml_query.processing import (
    update_top_definition,
)
from iml_query.tree_sitter_utils import (
    get_parser,
)


def test_update_top_definition():
    """
    Test update_top_definition with various scenarios.

    - replacement
    - addition by setting keep_previous_definition to True (it's False by
        default)
    - deletion by setting new_definition to empty string
    """
    iml = """\
let f x =
    let rec g y =
    let rec h z =
        z + 1
    in
    h y + 1
    in
    let i w =
    w + 1
    in
g (i x + 1)


let rec normal_rec x =
    if x < 0 then 0 else normal_rec (x - 1)\
"""

    tree = get_parser().parse(bytes(iml, encoding='utf8'))
    iml_2, tree_2 = update_top_definition(
        iml,
        tree,
        top_def_name='f',
        new_definition='let f = x + 1',
    )
    assert iml_2 == snapshot("""\
let f = x + 1


let rec normal_rec x =
    if x < 0 then 0 else normal_rec (x - 1)\
""")

    iml_3, tree_3 = update_top_definition(
        iml_2,
        tree_2,
        top_def_name='normal_rec',
        new_definition='let g = fun x -> x + 1',
    )
    assert iml_3 == snapshot("""\
let f = x + 1


let g = fun x -> x + 1
""")

    # Add new definition, keep previous definition
    iml_4, tree_4 = update_top_definition(
        iml_3,
        tree_3,
        top_def_name='f',
        new_definition='let f2 = x + 2',
        keep_previous_definition=True,
    )
    assert iml_4 == snapshot("""\
let f = x + 1
let f2 = x + 2


let g = fun x -> x + 1
""")

    # Delete definition by setting new definition to empty string
    iml_5, tree_5 = update_top_definition(
        iml_4,
        tree_4,
        top_def_name='f',
        new_definition='',
    )
    assert iml_5 == snapshot("""\

let f2 = x + 2


let g = fun x -> x + 1
""")
    _ = tree_5

# pyright: basic
from inline_snapshot import snapshot

from iml_query.tree_sitter_utils import (
    get_parser,
    insert_lines,
)


def test_insert_lines_consecutive():
    """Test multiple consecutive insert_lines to verify tree validity."""
    iml = """\
let x = 1
let y = 2"""  # No trailing newline
    parser = get_parser()
    tree = parser.parse(bytes(iml, encoding='utf8'))

    # First insertion after line 1 (second line)
    iml2, tree2 = insert_lines(iml, tree, lines=['let z = 3'], insert_after=1)
    assert iml2 == snapshot("""\
let x = 1
let y = 2
let z = 3
""")
    assert not tree2.root_node.has_error

    # Second insertion after line 2 (using the new tree)
    iml3, tree3 = insert_lines(iml2, tree2, lines=['let w = 4'], insert_after=2)
    assert iml3 == snapshot("""\
let x = 1
let y = 2
let z = 3
let w = 4
""")
    assert not tree3.root_node.has_error

    # Third insertion in the middle (line 1)
    iml4, tree4 = insert_lines(iml3, tree3, lines=['let a = 0'], insert_after=1)
    assert iml4 == snapshot("""\
let x = 1
let y = 2
let a = 0
let z = 3
let w = 4
""")
    assert not tree4.root_node.has_error

    # Insert at the beginning of the file
    iml5, tree5 = insert_lines(
        iml4, tree4, lines=['let b = 0'], insert_after=-1
    )
    assert iml5 == snapshot("""\
let b = 0
let x = 1
let y = 2
let a = 0
let z = 3
let w = 4
""")
    assert not tree5.root_node.has_error

    # Insert after the first line
    iml6, tree6 = insert_lines(iml5, tree5, lines=['let c = 5'], insert_after=0)
    assert iml6 == snapshot("""\
let b = 0
let c = 5
let x = 1
let y = 2
let a = 0
let z = 3
let w = 4
""")
    assert not tree6.root_node.has_error

import doctest

from imandrax_api_models import ErrorMessage, Location, Position, context_utils
from imandrax_api_models.context_utils import format_error_msg
from inline_snapshot import snapshot


def mk_loc(file: str | None) -> Location:
    return Location(
        file=file,
        start=Position(line=2, col=5),
        stop=Position(line=2, col=10),
    )


def test_format_error_msg_picks_source_by_file():
    msg = ErrorMessage(msg='boom', locs=[mk_loc('lib.iml')])
    sources = {
        'main.iml': """\
x
y
""",
        'lib.iml': """\
let a = 1
let bbbbb = 2
""",
    }
    assert format_error_msg(msg, sources) == snapshot(
        {
            'msg': 'boom',
            'location': 'lib.iml:2:5-2:10',
            'error_src': """\
1 | let a = 1
2 | let bbbbb = 2
  |     ^^^^^
3 | \
""",
        }
    )


def test_format_error_msg_unknown_file_has_no_excerpt():
    msg = ErrorMessage(msg='boom', locs=[mk_loc('other.iml')])
    assert format_error_msg(msg, {'main.iml': 'x\n'}) == snapshot(
        {'msg': 'boom', 'location': 'other.iml:2:5-2:10'}
    )


def test_context_utils_doctests():
    results = doctest.testmod(context_utils, verbose=False)
    assert results.attempted > 0
    assert results.failed == 0

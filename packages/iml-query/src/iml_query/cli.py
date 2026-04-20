import sys
from pathlib import Path

from iml_query.tree_sitter_utils import (
    fmt_node,
    get_parser,
    run_query,
)


def main():
    usage = f'{__name__} file.iml --query ts-query.scm'
    iml: str | None = None
    ts_query: str | None = None
    match sys.argv:
        case [_, iml_p]:
            iml = Path(iml_p).read_text()
        case [_, iml_p, '--query', ts_query]:
            iml = Path(iml_p).read_text()
        case _:
            raise ValueError(usage)

    parser = get_parser()
    tree = parser.parse(bytes(iml, 'utf8'))

    if ts_query is not None:
        res = run_query(ts_query, code=iml)
        print(res[0][1])
    else:
        print(fmt_node(tree.root_node))


if __name__ == '__main__':
    main()

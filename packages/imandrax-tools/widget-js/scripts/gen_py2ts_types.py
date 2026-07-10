#!/usr/bin/env uv run
"""
Generate the TS types the widgets consume from their Python pydantic models.

The widget front-end contracts are pydantic models in `imandrax_api_models` and
`imandrax_tools`. This runs `pydantic-to-typescript` (pydantic2ts, which wraps
`json-schema-to-typescript`) to emit them into a single generated module,
`src/generated/node.ts`, that each widget's handwritten `types.ts` re-exports.

pydantic2ts is pointed at this module and reads `__all__` below, so only the
widget-facing entry models are emitted; referenced models (dependency) are pulled in automatically.
Adding a widget = import its entry model here and add it to `__all__`.
"""

from __future__ import annotations

import sys
from pathlib import Path

import pydantic2ts
from imandrax_api_models.region_decomp import RegionGroupView
from imandrax_tools.idf.viz_view import View as IDFView
from imandrax_tools.widget._tasks import TaskEntry

# The models pydantic2ts emits when it imports this module as `--module`.
__all__ = ['RegionGroupView', 'TaskEntry', 'IDFView']

PKG_ROOT = Path(__file__).resolve().parents[1]
TS_PATH = PKG_ROOT / 'src/generated/types.ts'
JSON2TS = PKG_ROOT / 'node_modules/.bin/json2ts'


def main() -> None:
    if not JSON2TS.exists():
        sys.exit(
            f'{JSON2TS} not found -- run `npm ci` in {PKG_ROOT} first '
            '(json-schema-to-typescript is a devDependency).'
        )
    TS_PATH.parent.mkdir(parents=True, exist_ok=True)

    pydantic2ts.generate_typescript_defs(
        module=str(Path(__file__).resolve()),
        output=str(TS_PATH),
        # `--additionalProperties false`: each widget node is a closed contract,
        # so no `[k: string]: unknown` index signatures.
        json2ts_cmd=f'{JSON2TS} --additionalProperties false',
    )
    print(f'wrote {TS_PATH.relative_to(PKG_ROOT)}')


if __name__ == '__main__':
    main()

#!/usr/bin/env uv run
"""
Generate the TS type for the region-decomp widget node from its Python model.

`RegionGroupView` (imandrax_api_models.region_decomp) is the single source of
truth for the shape the JS widget consumes. This runs `pydantic-to-typescript`
(pydantic2ts, which wraps `json-schema-to-typescript`) to emit `generated/node.ts`,
which the handwritten `types.ts` re-exports. pydantic2ts is pointed at THIS
module and reads `__all__` below, so only the widget-facing models are emitted;
referenced models (`RegionNonGroupStat`) are pulled in automatically.
"""

from __future__ import annotations

import sys
from pathlib import Path

from imandrax_api_models.region_decomp import RegionGroupView
from pydantic2ts import generate_typescript_defs

# The models pydantic2ts emits when it imports this module as `--module`.
__all__ = ['RegionGroupView']

PKG_ROOT = Path(__file__).resolve().parents[1]
TS_PATH = PKG_ROOT / 'src/region_decomp/generated/node.ts'
JSON2TS = PKG_ROOT / 'node_modules/.bin/json2ts'


def main() -> None:
    if not JSON2TS.exists():
        sys.exit(
            f'{JSON2TS} not found -- run `npm ci` in {PKG_ROOT} first '
            '(json-schema-to-typescript is a devDependency).'
        )
    TS_PATH.parent.mkdir(parents=True, exist_ok=True)

    generate_typescript_defs(
        module=str(Path(__file__).resolve()),
        output=str(TS_PATH),
        # `--additionalProperties false`: the widget node is a closed contract,
        # so no `[k: string]: unknown` index signatures.
        json2ts_cmd=f'{JSON2TS} --additionalProperties false',
    )
    print(f'wrote {TS_PATH.relative_to(PKG_ROOT)}')


if __name__ == '__main__':
    main()

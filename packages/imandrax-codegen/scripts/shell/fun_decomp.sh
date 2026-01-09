#!/bin/bash
codegen() {
    if [[ $# -ne 1 ]]; then
        echo "Usage: $0 <decomp-file>"
        exit 1
    fi
    yq '.decomp_res.artifact' "$1" -o json \
    | dune exec bin/parse.exe -- - - --mode fun-decomp \
    | uv run imandrax-codegen -
}

codegen "$1"

#!/bin/bash
codegen() {
    if [[ $# -ne 1 ]]; then
        echo "Usage: $0 <model-file>"
        exit 1
    fi
    yq "$1" -o json \
    | dune exec bin/parse.exe -- - - --mode model \
    | uv run py-gen -
}

codegen "$1"

#!/bin/bash
codegen() {
    if [[ $# -ne 1 ]]; then
        echo "Usage: $0 <decl-file>"
        exit 1
    fi
    yq '.get_decls_res.decls[0].artifact' "$1" -o json \
    | dune exec bin/parse.exe -- - - --mode decl \
    | uv run imandrax-codegen -
}

codegen "$1"

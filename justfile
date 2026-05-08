set shell := ["bash", "-cu"]
set dotenv-load := false

mkdocs := "DISABLE_MKDOCS_2_WARNING=true NO_MKDOCS_2_WARNING=1 uv run mkdocs"

list:
    @just --list

# Sync the uv workspace including the docs dependency group.
sync:
    uv sync --group docs

# Serve the docs site locally.
docs-serve port='8765':
    {{mkdocs}} serve \
      -w packages/imandrax-api-models/src \
      -w packages/iml-query/src \
      -w packages/imandrax-tools/src \
      -w packages/codelogician-skill/skill \
      -a 127.0.0.1:{{port}}

# Build the static docs site into ./site.
docs-build:
    {{mkdocs}} build

# Strict build: fail on any mkdocs warning. Use in CI.
docs-build-strict:
    {{mkdocs}} build --strict

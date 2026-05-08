set shell := ["bash", "-cu"]
set dotenv-load := false


mkdocs := "DISABLE_MKDOCS_2_WARNING=true NO_MKDOCS_2_WARNING=1 uv run mkdocs"

list:
    @just --list

# Setup
# =====

# Sync the uv workspace including the docs dependency group.
sync:
    uv sync --group docs

# Docs
# ====

# Serve the docs site locally with live reload.
docs-serve port='8765':
    {{mkdocs}} serve -a 127.0.0.1:{{port}}

docs-serve-watch port='8765' watch-path='.':
    {{mkdocs}} serve -w {{watch-path}} -a 127.0.0.1:{{port}}

# Build the static docs site into ./site.
docs-build:
    {{mkdocs}} build

# Strict build: fail on any mkdocs warning. Use in CI.
docs-build-strict:
    {{mkdocs}} build --strict

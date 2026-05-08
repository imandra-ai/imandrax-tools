set shell := ["bash", "-cu"]
set dotenv-load := false

default:
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
    uv run mkdocs serve -a 127.0.0.1:{{port}}

# Build the static docs site into ./site.
docs-build:
    uv run mkdocs build

# Strict build: fail on any mkdocs warning. Use in CI.
docs-build-strict:
    uv run mkdocs build --strict

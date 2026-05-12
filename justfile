set shell := ["bash", "-cu"]
set dotenv-load := false

# Note: some plugins requires properdocs as hard dependency, which hijacks
# all mkdocs imports to properdocs. This also breaks livereload.
# Switching to properdocs fixes the livereload issue.
# properdocs is a fork by one of the main maintainers of mkdocs.
mkdocs := "DISABLE_MKDOCS_2_WARNING=true NO_MKDOCS_2_WARNING=1 uv run properdocs"

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
      -w docs \
      -a 127.0.0.1:{{port}}

# Build the static docs site into ./site.
docs-build:
    {{mkdocs}} build

# Strict build: fail on any mkdocs warning. Use in CI.
docs-build-strict:
    {{mkdocs}} build --strict

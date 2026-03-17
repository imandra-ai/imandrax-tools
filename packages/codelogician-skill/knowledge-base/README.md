# Knowledge Base

Knowledge base for prompts.

Outputs:
- `context.yaml` will be generated to be used by Jinja. Expected to be used by `jinja -d context.yaml template.jinja`.
- `error_corpus.yaml`

`dune` manages the build process.

## Requirements
- jinja cli (`uv tool install jinja`)
- yq
- dune

# Knowledge Base

ImandraX / IML knowledge base for agentic use.

Outputs:
- `context.yaml` will be generated to be used by Jinja. Expected to be used as `jinja -d context.yaml template.jinja`.
- `iml_eval_corpus.json`

`dune` manages the build process.

## Requirements
- jinja cli (`uv tool install jinja`)
- yq
- dune

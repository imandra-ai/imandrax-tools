# codelogician-skill

CodeLogician [agent skill](https://platform.claude.com/docs/en/agents-and-tools/agent-skills/overview) for working with IML/ImandraX.

## Installation

Configure skill in your agent

or

```bash
npx skills add imandra-ai/imandrax-tools/packages/codelogician-skill/skill
```

# Knowledge base
- templates/skill/src, extended-prelude, context.yaml --> templates/skill/staging --> skill/
  - `templates/skill/staging` holds per-subdir dune files that produce the final outputs via jinja / janet / copy rules
  - `skill/` is an rsync of `templates/skill/staging` minus the dune files (the public distribution)
  - if *.jinja, pass to jinja CLI with `--data context.yaml`, outputs to a new file with file extension .jinja stripped
  - extended-prelude/*.iml -> copy to skill/extended-prelude/
  - SKILL.md has a special rule using the janet script
  - for other markdown files, just copy them over

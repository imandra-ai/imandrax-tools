# codelogician-skill

CodeLogician [agent skill](https://platform.claude.com/docs/en/agents-and-tools/agent-skills/overview) for working with IML/ImandraX.

## Installation

Configure skill in your agent

or

```bash
npx skills add imandra-ai/imandrax-tools/packages/codelogician-skill/skill
```

# Knowledge base
- templates/skill, extended-prelude --> skill/
  - if *.jinja, pass to jinja CLI with `--data context.yaml`, outputs to a new file with file extension .jinja stripped
  - SKILL.md has specil rule using the janet script

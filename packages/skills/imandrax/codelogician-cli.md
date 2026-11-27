---
name: codelogician-cli
description: Guide for using the  `codelogician` / `codelogician-lite` CLI to interact with ImandraX. Includes installation guide, `--json` output, `--async-only` workflow.
---

# `codelogician-lite` / `codelogician` CLI

## Installation

- Installation options:
  - `curl -fsSL https://codelogician.dev/codelogician/install.sh | sh`
  - `uv tool install codelogician`
  - `pip install codelogician`
- Both `codelogician` and `codelogician-lite` will be available after installation. `codelogician-lite` is an alias of `codelogician eval` subcommand.

```
codelogician-lite --help
# codelogician --help
```

`IMANDRA_UNI_KEY` or `IMANDRAX_API_KEY` needs to be set in the environment variables.

`codelogician eval` will be the main workhorse

See other environment variables guide in `codelogician eval --help`

## Store JSON for later programmatic interaction

It can he helpful to store the JSON output of the command you are running for later programmatic interaction, e.g., to use `jq` or a Python script to filter or manipulate the output. Useful to persist time-consuming commands to disk for later structured analysis.

## `--async-only` + `list-artifacts` + `get-artifact` workflow

Very useful for long-running commands that can timeout.

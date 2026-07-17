---
name: codelogician-cli
description: Guide for using the  `codelogician` / `codelogician-lite` CLI to interact with ImandraX and access additional features.
---

# `codelogician-lite` / `codelogician` CLI

## Installation

- Installation commands:
  - `curl -fsSL https://codelogician.dev/codelogician/install.sh | sh`
  - `uv tool install codelogician`
  - `pip install codelogician`
- Both `codelogician` and `codelogician-lite` will be available after installation. `codelogician-lite` is an alias of `codelogician eval` subcommand.

```
codelogician-lite --help
# codelogician --help
```

`IMANDRA_UNI_KEY` or `IMANDRAX_API_KEY` needs to be set in the environment variables.

## Usage

All commands accept a `FILE` argument (path to an IML file, or `-` to read from stdin) and a `--json` flag to output results in JSON format.

Important: Refer to `--help` for arguments and options of each command.

### Store JSON for later programmatic interaction

It can he helpful to store the JSON output of the command you are running for later programmatic interaction, e.g., to use `jq` (or a Python script) to filter or manipulate the output. Reasons for doing so include:
- Some commands (e.g., `check-decomp` for a function with large state-space) can take a long time to run
- Some results are convoluted and need to be filtered or manipulated for further analysis
- Parallizing multiple commands (e.g., `check-vg` and `check-decomp` for different index) can be useful

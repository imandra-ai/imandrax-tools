---
name: codelogician-cli
description: Guide for using the `codelogician` CLI to interact with ImandraX and access additional features.
---

# `codelogician` CLI

## Installation

- `codelogician` can be installed by `uv tool install codelogician` or `pip install codelogician`.
- `codelogician-tools` is a more light-weight version that is bundled with the installation of `codelogician`. Use whichever you prefer. But `codelogician-tools` is recommended for file-system-based agent (Claude Code, Codex, Gemini CLI, etc.).

```
codelogician --help
# codelogician-tools --help
```

`IMANDRA_UNI_KEY` or `IMANDRAX_API_KEY` needs to be set in the environment variables.

## Usage

All commands accept a `FILE` argument (path to an IML file, or `-` to read from stdin) and a `--json` flag to output results in JSON format.

- `check [FILE] [OPTIONS]`:
  - description: Evaluate an IML file without VG and decomp.
  - options:
    - `--with-vgs`: Include verify/instance requests in evaluation (default: false).
    - `--with-decomps`: Include decomp requests in evaluation (default: false).
    - `--json`: Output results as JSON.
- `list-vg [FILE] [OPTIONS]`:
  - description: List verification goals specified by `verify` or `instance` commands in an IML file.
  - options:
    - `--json`: Output results as JSON.
- `check-vg [FILE] [OPTIONS]`:
  - description: Check verification goals specified by `verify` or `instance` commands in an IML file. Runs eval first, then checks VGs concurrently.
  - options:
    - `--index INTEGER`: Index(es) of VGs to check (1-based, repeatable). Defaults to all.
    - `--check-all`: Check all VGs (same as omitting `--index`).
    - `--json`: Output results as JSON (suppresses progress output).
- `list-decomp [FILE] [OPTIONS]`:
  - description: List decomp requests in an IML file.
  - options:
    - `--json`: Output results as JSON.
- `check-decomp [FILE] [OPTIONS]`:
  - description: Check decomp requests in an IML file. Runs eval first, then checks decomps concurrently.
  - options:
    - `--index INTEGER`: Index(es) of decomp requests to check (1-based, repeatable). Defaults to all.
    - `--check-all`: Check all decomp requests (same as omitting `--index`).
    - `--json`: Output results as JSON (suppresses progress output).
- `gen-test`:
  - description: Generate test cases in Python / TypeScript from region decomp in IML. Only available on macOS and Linux.

Refer to `--help` for most up-to-date usage docs.

## Tips

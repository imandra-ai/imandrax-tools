# Codelogician / ImandraX Agent Harness

Common utilities for interacting with ImandraX / IML, for both humans and LLMs.

## Testing

`pytest-recording` is used to record and replay ImandraX API requests.

- by default, it uses `pytest --record-mode=none`, i.e. replay-only, no recording
- `pytest --record-mode=?`:
  - for running tests for the first time, use `--record-mode=once`
  - after editing tests that requires new the API requests, use `--record-mode=new_episodes` to record new API requests
  - to refresh (stale) API requests, use `--record-mode=all`
- `cassettes` directory is gitignored
- see [pytest-recording](https://github.com/kiwicom/pytest-recording) for the official doc

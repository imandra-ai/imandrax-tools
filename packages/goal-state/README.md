# Goal State

A port of `imandrax-vscode/src/goal-state`. Formats the current proof goal state from a `po_res` artifact into a human-readable string.

## Architecture

This crate is a pure Rust library with no Python coupling. The Python extension module lives in the `imandrax-tools` package, which depends on this crate.

### Rust API

```rust
// Format goal state from an already-deserialized PO_res
format_goal_state(po_res: &PO_res) -> Result<String, NoGoalState>

// Convenience helpers that deserialize for you
with_po_res_from_twine(data: &[u8], f: impl FnOnce(&PO_res) -> R) -> Result<R>
with_po_res_from_zip(path: &Path, f: impl FnOnce(&PO_res) -> R) -> Result<R>
```

`NoGoalState` is returned when the po_res does not have open subgoals: either the proof is complete (`Proved`), a counter-model was found (`CounterModel`), or an error occurred.

### Python

The Python bindings are implemented in `imandrax-tools/src/lib.rs` and distributed as part of the `imandrax-tools` wheel:

```python
from imandrax_tools import goal_state

# from a po_res_art.zip path
text = goal_state.format_goal_state_from_zip("/path/to/po_res_art.zip")

# from raw twine bytes
text = goal_state.format_goal_state_from_bytes(data)
```

Both functions return the formatted subgoal string when there are open goals, or raise:
- `goal_state.GoalStateProved` — proof is complete
- `goal_state.GoalStateCounterModel` — a counter-model was found
- `RuntimeError` — deserialization or I/O error

## Development

```bash
# Run Rust tests
cargo test

# Build the Python extension (from imandrax-tools/)
cd ../imandrax-tools
maturin build
```

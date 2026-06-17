# Code generation adapters

This directory holds the per-target-language adapters that turn the
language-neutral Semantic IR (`semantic_ir/`) into target source code.

- `python/` — emits a Python AST (serialized to JSON) that the Python-side
  codegen (`../../python/imandrax_codegen/`) then unparses into source.
- `typescript/` — emits TypeScript source directly.

## Where type-mapping data lives

The type mapping data is not a single table. It is split by concern, and for Python
it is additionally split across the OCaml→Python pipeline boundary:

| Concern | Python | TypeScript |
|---|---|---|
| Type *name* substitution | `python/config.ml` (`type_name_mapping`) | `typescript/config.ml` (`type_name_mapping`) |
| Container / structural emission | `python/transform.ml` | `typescript/emit.ml` |
| Runtime *lib* source | `../../python/imandrax_codegen/unparse.py` (Python side) | `typescript/helper_lib.ml` (OCaml side, cinaps-generated) |
| Extra import pruning | `unparse.py` (`check_needs_option_lib`) | `typescript/config.ml` (`Extra_imports`) |

Note the asymmetry: TypeScript keeps its runtime lib on the OCaml side; thePython option lib lives on the Python side. 

## Reference: IML/SIR → Python

Name substitution (`python/config.ml`):

| IML / SIR | Python | Notes |
|---|---|---|
| `int` | `int` | |
| `real` | `real` | name passes through; optionally aliased `real = float` in the Python preamble, gated by `--include-real-to-float-alias` (default off) |
| `bool` | `bool` | |
| `string` | `str` | |
| `char`, `LChar.t` | `str` | |
| `unit` | `None` | |
| `Map.t` | `defaultdict` | `defaultdict[K, V]`; note: `from collections import defaultdict` is **not** currently emitted by `gen_preamble` |
| `option` | `option` | name matches the runtime lib's name, so it looks like a passthrough but resolves to the option lib (below) |

Structural forms (`python/transform.ml`):

| IML / SIR | Python |
|---|---|
| `'a list` | `list[T]` |
| tuple `(a, b, …)` | `tuple[a, b, …]` |
| arrow `a -> b` | `Callable[[a], b]` |

Runtime libraries / preamble (`unparse.py`):

| Lib | Emitted as | Injected when |
|---|---|---|
| option | `Some[T]` dataclass + `option: TypeAlias = Some[T] \| None` | `option` is referenced (`check_needs_option_lib`) |
| `real` alias | `real = float` | `--include-real-to-float-alias` flag is set |

## Reference: IML/SIR → TypeScript

Name substitution (`typescript/config.ml`):

| IML / SIR | TypeScript | Notes |
|---|---|---|
| `int` | `number` | |
| `real` | `number` | |
| `bool` | `boolean` | |
| `string` | `string` | |
| `char`, `LChar.t` | `string` | |
| `unit` | `null` | |
| `list` | `Array` | emitted as `T[]` (`emit.ml`) |
| `option` | `Option` | custom runtime type (below) |
| `Map.t` | `DefaultMap` | custom runtime type (below) |

Structural forms (`typescript/emit.ml`):

| IML / SIR | TypeScript |
|---|---|
| tuple `(a, b, …)` | `[a, b, …]` |
| arrow `a -> b` | `(a) => b` |

Runtime libraries (`typescript/helper_lib.ml`, tracked via `Extra_imports`):

| Lib | Emitted as | Injected when |
|---|---|---|
| option | `type Option<T> = { tag: "Some"; payload: T } \| null` | `option` is used |
| default map | `class DefaultMap<K, V> implements Map<K, V>` | `Map.t` is used |

# ImandraX Code Generator

## Project Overview

This is `imandrax-codegen`, a code generator that converts ImandraX artifacts (MIR - Mid-level Intermediate Representation) into Python code. The project consists of two main components:

1. **OCaml Parser** (`lib/`, `bin/`): Deserializes YAML/JSON artifacts and converts them to a Python AST representation
2. **Python Code Generator** (`python/py_gen/`): Deserializes the Python AST and generates executable Python code

## Architecture & Data Flow

```
YAML/JSON Artifact → OCaml Parser (parse.exe) → JSON AST → Python Codegen → Python Code
```

### Complete pipeline example:
```bash
yq "." artifact.yaml -o json | dune exec bin/parse.exe -- - --mode model | uv run imandrax-codegen -
```

## Project Structure

```
.
├── lib/                    # OCaml library code
│   ├── parse.ml            # Main parsing logic
│   ├── parse_term.ml       # Term/expression parsing
│   ├── parse_decl.ml       # Declaration parsing
│   ├── parse_model.ml      # Model parsing
│   ├── parse_fun_decomp.ml # Function decomposition parsing
│   ├── parse_common.ml     # Common parsing utilities
│   ├── ast.ml              # Python AST definitions in OCaml
│   ├── ast_types.ml        # AST type definitions
│   ├── art_utils.ml        # Artifact utilities
│   └── pretty_print.ml     # AST pretty printing
├── bin/
│   └── parse.ml            # CLI entry point for parser (parse.exe)
├── python/py_gen/          # Python code generator
│   ├── __main__.py         # CLI entry point (imandrax-codegen command)
│   ├── ast_deserialize.py  # Deserializes JSON AST from OCaml
│   ├── ast_types.py        # Python AST type definitions
│   ├── unparse.py          # Converts Python AST to source code
│   └── utils.py            # Utilities
├── test/
│   ├── data/               # Test input YAML files
│   │   ├── decl/           # Declaration tests
│   │   ├── model/          # Model tests
│   │   └── fun_decomp/     # Function decomposition tests
│   └── e2e/                # End-to-end cram tests
└── scripts/                # Artifact generation scripts
    ├── gen_model_art/
    ├── gen_fun_decomp_art/
    └── gen_decl_art/
```

## Technology Stack

### OCaml Side
- **Build System**: Dune 3.0+
- **Language**: OCaml >= 4.12.0
- **Key Dependencies**:
  - `imandrax-api` (>= 0.18) - ImandraX API for MIR definitions
  - `yojson` (>= 1.6) - JSON parsing/generation
  - `yaml` (>= 3.0) - YAML parsing
  - `containers` (>= 3.4) - Data structures
  - `ppx_deriving`, `ppx_deriving_yojson` - Code generation

### Python Side
- **Package Manager**: `uv` (used for all Python commands)
- **Language**: Python >= 3.13
- **Key Dependencies**:
  - `imandrax-api[async]` (>= 0.18) - ImandraX Python API
  - `typer` - CLI framework
  - `pyyaml` - YAML support
  - `ruff` - Linting and formatting

## Common Commands

Testing the Full Pipeline
Parse modes: `--mode model`, `--mode decl`, `--mode fun_decomp`

```bash
# Test model parsing
yq "." test/data/model/primitive/int_option_none.yaml -o json | \
  dune exec bin/parse.exe -- - --mode model | \
  uv run imandrax-codegen -

# Test declaration parsing
yq "." test/data/decl/variant_with_payload.yaml -o json | \
  dune exec bin/parse.exe -- - --mode decl | \
  uv run imandrax-codegen -
```

```bash
# Show all available make targets
make help
```

## Key Implementation Details

### OCaml → Python AST Serialization Format

The OCaml code serializes Python AST nodes as JSON in a tagged format:

- **Empty variants**: `["Tag"]` → e.g., `["Load"]`, `["Store"]`
- **Variants with fields**: `["Tag", {...}]` → e.g., `["Name", {"id": "x", "ctx": ["Load"]}]`
- **Special values**:
  - `["Unit"]` → Python `None`
  - `["String", "hello"]` → Python `"hello"`
  - `["Int", 42]` → Python `42`
  - `["Bool", true]` → Python `True`

### Parse Modes

The parser supports three modes (specified via `--mode`):

1. **model**: Parses MIR model instances (functions, values)
2. **decl**: Parses type declarations (variants, records, aliases)
3. **fun_decomp**: Parses function decomposition results


## Related Projects

- **imandrax-api**: Located at `../../../imandrax-api/`
  - Defines the MIR (Mid-level IR) types in `src/lib/mir/`
  - Provides both OCaml and Python bindings
- **imandrax-tools**: Parent monorepo at `../..`

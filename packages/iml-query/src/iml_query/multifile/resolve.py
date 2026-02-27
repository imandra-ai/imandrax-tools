"""Core module resolution logic for multi-file IML projects."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING

from iml_query.queries import (
    IMPORT_NAMED_PATH_EXTRACTION_QUERY_SRC,
    IMPORT_NAMED_PATH_QUERY_SRC,
    IMPORT_PATH_ONLY_QUERY_SRC,
)
from iml_query.tree_sitter_utils import (
    delete_nodes,
    run_queries,
    unwrap_bytes,
)

if TYPE_CHECKING:
    from tree_sitter import Node


class ModuleNotFoundError(Exception):
    """Raised when an imported file does not exist."""


class CircularImportError(Exception):
    """Raised when a circular import is detected."""


class NotImplementedImportError(Exception):
    """Raised for unsupported import schemes (findlib:, dune:)."""


@dataclass
class ImlImport:
    module_name: str  # e.g. "Mod_name"
    path: str  # Path in the import statement, e.g. "path/to/file.iml" or "findlib:foo.bar" or "dune:foo.bar"
    extraction_name: str | None  # optional 3rd arg
    import_node: Node  # the full [@@@import ...] node for deletion


@dataclass
class ResolvedModule:
    name: str  # module name
    path: Path  # absolute path to .iml file
    content: str  # file content with imports stripped
    imports: list[ImlImport]  # parsed imports


def _module_name_from_path(path: str) -> str:
    """Derive module name from file path: 'path/to/file.iml' -> 'File'."""
    stem = Path(path).stem
    return stem[0].upper() + stem[1:]


def parse_imports(code: str) -> list[ImlImport]:
    """Parse all import statements from IML source code."""
    from iml_query.tree_sitter_utils import get_parser

    parser = get_parser()
    tree = parser.parse(bytes(code, encoding='utf8'))
    node = tree.root_node

    captures_map = run_queries(
        {
            'named_path': IMPORT_NAMED_PATH_QUERY_SRC,
            'named_path_extraction': IMPORT_NAMED_PATH_EXTRACTION_QUERY_SRC,
            'path_only': IMPORT_PATH_ONLY_QUERY_SRC,
        },
        node=node,
    )

    imports: dict[int, ImlImport] = {}  # keyed by start_byte to deduplicate

    # IMPORT_2 first (3-arg form) — so IMPORT_1 won't overwrite with less info
    for capture in captures_map.get('named_path_extraction', []):
        node = capture['import'][0]
        imports[node.start_byte] = ImlImport(
            module_name=unwrap_bytes(capture['import_name'][0].text).decode(),
            path=unwrap_bytes(capture['import_path'][0].text).decode(),
            extraction_name=unwrap_bytes(
                capture['extraction_name'][0].text
            ).decode(),
            import_node=node,
        )

    # IMPORT_1: [@@@import Mod, "path"] — skip if already matched by IMPORT_2
    for capture in captures_map.get('named_path', []):
        node = capture['import'][0]
        if node.start_byte in imports:
            continue
        imports[node.start_byte] = ImlImport(
            module_name=unwrap_bytes(capture['import_name'][0].text).decode(),
            path=unwrap_bytes(capture['import_path'][0].text).decode(),
            extraction_name=None,
            import_node=node,
        )

    # IMPORT_3: [@@@import "path"]
    for capture in captures_map.get('path_only', []):
        node = capture['import'][0]
        path = unwrap_bytes(capture['import_path'][0].text).decode()
        imports[node.start_byte] = ImlImport(
            module_name=_module_name_from_path(path),
            path=path,
            extraction_name=None,
            import_node=node,
        )

    # Sort by position in source for deterministic ordering
    return sorted(imports.values(), key=lambda i: i.import_node.start_byte)


def resolve_modules(entry_path: Path) -> list[ResolvedModule]:
    """
    Resolve all modules starting from an entry file.

    Returns a topologically sorted list of ResolvedModule (leaves first,
    entry module last), suitable for mk_monolith_iml.

    Raises:
        ModuleNotFoundError: if an imported file doesn't exist
        CircularImportError: if a circular import is detected
        NotImplementedImportError: for findlib:/dune: imports

    """
    entry_path = entry_path.resolve()
    if not entry_path.exists():
        raise ModuleNotFoundError(str(entry_path))

    result: list[ResolvedModule] = []
    visited: set[Path] = set()
    in_progress: set[Path] = set()

    def _resolve(file_path: Path, module_name: str) -> None:
        file_path = file_path.resolve()

        if file_path in visited:
            return
        if file_path in in_progress:
            raise CircularImportError(str(file_path))

        in_progress.add(file_path)

        content = file_path.read_text()
        imports = parse_imports(content)

        base_dir = file_path.parent

        # Recursively resolve dependencies first (DFS)
        for imp in imports:
            if imp.path.startswith('findlib:') or imp.path.startswith('dune:'):
                raise NotImplementedImportError(imp.path)

            dep_path = (base_dir / imp.path).resolve()
            if not dep_path.exists():
                raise ModuleNotFoundError(imp.path)

            _resolve(dep_path, imp.module_name)

        # Strip import nodes from content
        stripped_content, _ = delete_nodes(
            content, nodes=[imp.import_node for imp in imports]
        )

        result.append(
            ResolvedModule(
                name=module_name,
                path=file_path,
                content=stripped_content.strip(),
                imports=imports,
            )
        )

        in_progress.discard(file_path)
        visited.add(file_path)

    entry_name = _module_name_from_path(entry_path.name)
    _resolve(entry_path, entry_name)
    return result


def mk_monolith_iml(modules: list[ResolvedModule]) -> str:
    """
    Generate a monolith IML file from topologically sorted modules.

    Each dependency module is wrapped in `module Name = struct ... end`.
    The entry module (last in list) is emitted unwrapped.
    """
    parts: list[str] = []

    for i, mod in enumerate(modules):
        is_entry = i == len(modules) - 1
        if is_entry:
            parts.append(mod.content)
        else:
            indented = '\n'.join(
                f'  {line}' if line.strip() else ''
                for line in mod.content.splitlines()
            )
            parts.append(f'module {mod.name} = struct\n{indented}\nend')

    return '\n\n'.join(parts) + '\n'

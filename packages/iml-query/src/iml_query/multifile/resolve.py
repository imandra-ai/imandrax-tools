"""Core module resolution logic for multi-file IML projects."""

from __future__ import annotations

from dataclasses import dataclass
from functools import cached_property
from pathlib import Path
from typing import TYPE_CHECKING, Self

from iml_query.queries import (
    IMPORT_NAMED_PATH_QUERY_SRC,
    IMPORT_PATH_ONLY_QUERY_SRC,
    ImportCapture,
)
from iml_query.tree_sitter_utils import (
    delete_nodes,
    run_queries,
    unwrap_bytes,
)

if TYPE_CHECKING:
    from tree_sitter import Node


class IMLImportResolutionError(Exception):
    """Base class for all IML import resolution errors."""


class IMLModuleNotFoundError(IMLImportResolutionError):
    """Raised when an imported file does not exist."""


class CircularImportError(IMLImportResolutionError):
    """Raised when a circular import is detected."""


class NotImplementedImportError(IMLImportResolutionError):
    """Raised for unsupported import schemes (findlib:, dune:)."""


@dataclass
class IMLModule:
    """Resolved IML module."""

    path: Path  # absolute path to .iml file
    name: str  # module name
    src: str  # source code
    imports: list[ImportCapture]  # list of parsed imports

    @cached_property
    def content(self) -> str:
        """Module content with imports stripped."""
        content, _ = delete_nodes(
            self.src,
            nodes=[imp.import_stmt for imp in self.imports],
        )
        return content.strip() + '\n'


def parse_imports(code: str) -> list[ImportCapture]:
    """
    Parse all import statements from IML source code.

    Order is not guaranteed.
    """
    queries = {
        'path_only': IMPORT_PATH_ONLY_QUERY_SRC,
        'named': IMPORT_NAMED_PATH_QUERY_SRC,
    }
    matches = run_queries(queries, code=code)
    ts_captures: list[dict[str, list[Node]]] = [
        item for sublist in matches.values() for item in sublist
    ]
    return [
        ImportCapture.from_ts_capture(ts_capture) for ts_capture in ts_captures
    ]


def _module_name_from_path(path: Path) -> str:
    """Derive module name from file path: `path/to/file.iml` -> `File`."""
    return path.stem.capitalize()


type _ResolveState = tuple[
    tuple[IMLModule, ...], frozenset[Path], frozenset[Path]
]


def resolve(
    entry_path: Path,
) -> list[IMLModule] | IMLImportResolutionError:
    """
    Resolve all modules starting from an entry file.

    Returns a topologically sorted list of IMLModule (leaves first,
    entry module last), suitable for mk_monolith_iml.
    On failure, returns the error as a value.
    """
    entry_path = entry_path.resolve()
    if not entry_path.exists():
        return IMLModuleNotFoundError(str(entry_path))

    def loop(
        file_path: Path,
        state: _ResolveState,
    ) -> _ResolveState | IMLImportResolutionError:
        file_path = file_path.resolve()
        result, visited, in_progress = state

        if file_path in visited:
            return state
        if file_path in in_progress:
            return CircularImportError(str(file_path))

        in_progress = in_progress | {file_path}

        iml_src = file_path.read_text()
        imports = parse_imports(iml_src)
        module_name = _module_name_from_path(file_path)

        base_dir = file_path.parent

        # Recursively resolve dependencies first (DFS)
        for imp in imports:
            imp_path: str = str(
                unwrap_bytes(imp.import_path.text), encoding='utf-8'
            )
            if imp_path.startswith('findlib:') or imp_path.startswith('dune:'):
                return NotImplementedImportError(imp_path)

            dep_path = (base_dir / imp_path).resolve()
            if not dep_path.exists():
                return IMLModuleNotFoundError(imp_path)

            inner = loop(dep_path, (result, visited, in_progress))
            if isinstance(inner, IMLImportResolutionError):
                return inner
            result, visited, in_progress = inner

        module = IMLModule(
            name=module_name,
            path=file_path,
            src=iml_src,
            imports=imports,
        )

        return (
            result + (module,),
            visited | {file_path},
            in_progress - {file_path},
        )

    out = loop(entry_path, ((), frozenset(), frozenset()))
    if isinstance(out, IMLImportResolutionError):
        return out
    result, _, _ = out
    return list(result)


class Library:
    """A library is a collection of IML modules. A DAG."""

    def __init__(self, modules: list[IMLModule]):
        self.modules = modules

    @classmethod
    def from_entry(cls, entry_path: Path) -> Self:
        """Create a library from an entry file."""
        result = resolve(entry_path)
        if isinstance(result, IMLImportResolutionError):
            raise result
        return cls(modules=result)

    def to_monolith(self) -> str:
        """Generate a monolith IML file from the library."""
        return mk_monolith_iml(self.modules)


def mk_monolith_iml(modules: list[IMLModule]) -> str:
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

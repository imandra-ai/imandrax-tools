"""Core module resolution logic for multi-file IML projects."""

from __future__ import annotations

from collections.abc import Callable
from pathlib import Path
from typing import TYPE_CHECKING

from iml_query.queries import (
    IMPORT_NAMED_PATH_QUERY_SRC,
    IMPORT_PATH_ONLY_QUERY_SRC,
    ImportCapture,
)
from iml_query.tree_sitter_utils import run_queries, unwrap_bytes

from ._common import (
    CircularImportError,
    IMLImportResolutionError,
    IMLModule,
    IMLModuleNotFoundError,
    NotImplementedImportError,
)

if TYPE_CHECKING:
    from tree_sitter import Node


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
    return [ImportCapture.from_ts_capture(ts_capture) for ts_capture in ts_captures]


def _module_name_from_path(path: Path) -> str:
    """Derive module name from file path: `path/to/file.iml` -> `File`."""
    return path.stem.capitalize()


type _ResolveState = tuple[tuple[IMLModule, ...], frozenset[Path], frozenset[Path]]


def os_path_reader(path: Path) -> str:
    path_ = path.resolve()
    if not path_.exists():
        raise IMLModuleNotFoundError(str(path_))
    return path_.read_text()


type PathReader = Callable[[Path], str]


def resolve_or_error(
    entry_path: Path,
    path_reader: PathReader = os_path_reader,
) -> list[IMLModule] | IMLImportResolutionError:
    """
    Resolve all modules starting from an entry file.

    Returns a topologically sorted list of IMLModule (leaves first,
    entry module last).

    On failure, returns the error as a value.

    Args:
        entry_path: Path to the entry .iml file.
        path_reader: Function mapping a resolved absolute path to its source
            code. Defaults to reading from the filesystem; pass a custom reader
            to resolve against a bundle of pre-loaded modules.

    Returns:
        A topologically sorted list of IMLModule (leaves first,
        entry module last), suitable for `Library`.

        On failure, an `IMLImportResolutionError`.

    """

    def loop(
        file_path: Path,
        state: _ResolveState,
        name_override: str | None = None,
    ) -> _ResolveState | IMLImportResolutionError:
        file_path = file_path.resolve()
        result, visited, in_progress = state

        if file_path in visited:
            return state
        if file_path in in_progress:
            return CircularImportError(str(file_path))

        in_progress = in_progress | {file_path}

        try:
            iml_src = path_reader(file_path)
        except IMLModuleNotFoundError as err:
            return err
        imports = parse_imports(iml_src)
        module_name = name_override or _module_name_from_path(file_path)

        base_dir = file_path.parent

        # Recursively resolve dependencies first (DFS)
        for imp in imports:
            imp_path: str = str(unwrap_bytes(imp.import_path.text), encoding='utf-8')
            if imp_path.startswith(('findlib:', 'dune:')):
                return NotImplementedImportError(imp_path)

            dep_path = (base_dir / imp_path).resolve()

            dep_name: str | None = None
            if imp.import_name is not None:
                dep_name = str(unwrap_bytes(imp.import_name.text), encoding='utf-8')

            inner = loop(dep_path, (result, visited, in_progress), dep_name)
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

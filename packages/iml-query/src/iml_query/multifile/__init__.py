"""Multi-file (multi-module) handling."""

from pathlib import Path

from .resolve import (
    CircularImportError,
    IMLImportResolutionError,
    IMLModule,
    IMLModuleNotFoundError,
    Library,
    NotImplementedImportError,
    PathReader,
    mk_monolith_iml,
    os_path_reader,
    parse_imports,
    resolve,
)

__all__ = (
    'CircularImportError',
    'IMLModule',
    'IMLModuleNotFoundError',
    'NotImplementedImportError',
    'IMLImportResolutionError',
    'mk_monolith_iml',
    'parse_imports',
    'resolve',
    'gather_modules',
)


def gather_modules(
    entry_path: Path,
    path_reader: PathReader = os_path_reader,
) -> tuple[str, str]:
    """Resolve all modules starting from an entry file and generate a monolith."""
    lib = Library.from_entry_path(entry_path, path_reader)
    return lib.to_monolith()

"""Multi-file (multi-module) handling."""

from pathlib import Path

from ._common import (
    CircularImportError,
    IMLImportResolutionError,
    IMLModule,
    IMLModuleNotFoundError,
    Library,
    NotImplementedImportError,
)
from .resolve import PathReader, os_path_reader, parse_imports, resolve_or_error

__all__ = (
    'CircularImportError',
    'IMLImportResolutionError',
    'IMLModule',
    'IMLModuleNotFoundError',
    'Library',
    'NotImplementedImportError',
    'gather_modules',
    'parse_imports',
    'resolve_or_error',
)


def gather_modules(
    entry_path: Path,
    path_reader: PathReader = os_path_reader,
) -> Library:
    """
    Resolve all modules starting from an entry file.

    Raises:
        IMLImportResolutionError: If an import cannot be resolved.

    """
    modules = resolve_or_error(entry_path, path_reader)
    if isinstance(modules, IMLImportResolutionError):
        raise modules
    return Library(modules=tuple(modules))

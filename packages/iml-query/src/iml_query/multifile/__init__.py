"""Multi-file (multi-module) handling."""

from pathlib import Path

from .resolve import (
    CircularImportError,
    IMLImportResolutionError,
    IMLModule,
    IMLModuleNotFoundError,
    Library,
    NotImplementedImportError,
    mk_monolith_iml,
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


def gather_modules(entry_path: Path) -> tuple[str, str]:
    """Resolve all modules starting from an entry file and generate a monolith."""
    lib = Library.from_entry_path(entry_path)
    return lib.to_monolith()

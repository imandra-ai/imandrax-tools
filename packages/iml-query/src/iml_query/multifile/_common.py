"""Types shared across multi-file handling."""

from __future__ import annotations

from dataclasses import dataclass
from functools import cached_property
from pathlib import Path
from typing import NamedTuple

from iml_query.queries import (
    DECOMP_QUERY_SRC,
    INSTANCE_QUERY_SRC,
    VERIFY_QUERY_SRC,
    DecompCapture,
    ImportCapture,
    InstanceCapture,
    VerifyCapture,
)
from iml_query.tree_sitter_utils import blank_nodes, run_queries


class IMLImportResolutionError(Exception):
    pass


class IMLModuleNotFoundError(IMLImportResolutionError):
    pass


class CircularImportError(IMLImportResolutionError):
    pass


class NotImplementedImportError(IMLImportResolutionError):
    """Raised for unsupported import schemes (findlib:, dune:)."""


@dataclass
class IMLModule:
    """Resolved IML module."""

    path: Path  # absolute path to .iml file
    name: str  # module name (Camel case)
    src: str  # source code
    imports: list[ImportCapture]  # list of parsed imports

    @cached_property
    def content(self) -> str:
        """
        Module content with imports, VGs, and decomps blanked out.

        Stripped statements are overwritten with spaces rather than deleted, so
        every remaining line keeps its line number and column.
        """
        matches = run_queries(
            {
                'verify': VERIFY_QUERY_SRC,
                'instance': INSTANCE_QUERY_SRC,
                'decomp': DECOMP_QUERY_SRC,
            },
            code=self.src,
        )
        nodes = [imp.import_stmt for imp in self.imports]
        nodes += [
            VerifyCapture.from_ts_capture(c).verify_statement
            for c in matches.get('verify', [])
        ]
        nodes += [
            InstanceCapture.from_ts_capture(c).instance_statement
            for c in matches.get('instance', [])
        ]
        nodes += [
            DecompCapture.from_ts_capture(c).decomp_attr
            for c in matches.get('decomp', [])
        ]
        return _rstrip_lines(blank_nodes(self.src, nodes))


def _lines(src: str) -> list[str]:
    r"""
    Lines of `src`, split on `\n` only, as ImandraX counts them.

    Unlike `str.splitlines`, `\f`, `\v`, `\u2028` etc. do not end a line.
    """
    return src.removesuffix('\n').split('\n')


def _rstrip_lines(src: str) -> str:
    """Strip trailing whitespace of each line and trailing blank lines."""
    return '\n'.join(line.rstrip() for line in src.rstrip().split('\n')) + '\n'


@dataclass(frozen=True)
class SourceSegment:
    """
    The lines of one module file, as placed in the joined source.

    The joined source is the single IML text that `Library` builds from all
    modules (`Library.iml`), which is what ImandraX evaluates and reports
    positions in.
    """

    path: Path
    """Module file"""
    start_line_in_joined: int
    """1-indexed"""
    end_line_in_joined: int
    """1-indexed, inclusive"""
    join_line_offset: int
    """Lines that joining pushed this file down by."""
    join_col_offset: int
    """Columns that joining pushed this file right by (the `struct` indentation)."""

    def contains(self, line_in_joined: int) -> bool:
        """Whether a line of the joined source belongs to this file."""
        return self.start_line_in_joined <= line_in_joined <= self.end_line_in_joined

    def to_file_pos(self, line_in_joined: int, col_in_joined: int) -> tuple[int, int]:
        """`(line, col)` of the joined source -> `(line, col)` in the original file."""
        return (
            line_in_joined - self.join_line_offset,
            max(col_in_joined - self.join_col_offset, 1),
        )


class _Layout(NamedTuple):
    deps: str
    """Dependency text."""
    entry: str
    """Entry module text, imports blanked."""
    segments: tuple[SourceSegment, ...]
    """Where each module's lines are in the joined source, entry module last."""


@dataclass(frozen=True)
class Library:
    """
    The modules of a project, and the joined source they make.

    The joined source is one IML text holding all modules, which is what gets
    evaluated.

    - Each dependency module is wrapped in `module Name = struct ... end`, indented
    by 2.
    - The entry module (last) is emitted unwrapped.
    - Imports, and the dependencies' VGs and decomps, are replaced with blank lines
    - Lines outside every segment (`module X = struct`, `end`, and the blank lines between modules) belong to no file.
    """

    modules: tuple[IMLModule, ...]
    """Topologically sorted, non-empty."""

    def joined_to_module_loc(self, line: int, col: int) -> tuple[Path, int, int] | None:
        """
        Map a `(line, col)` of the joined source to `(path, line, col)` in a module file.

        Both are 1-indexed. A column inside the `struct` indentation maps to 1.
        Returns `None` on lines in no module file.

        Returns:
            (source-mod, line-in-src, col-in-src)
            or None if input is out of range

        """
        for seg in self._layout.segments:
            if seg.contains(line):
                return (seg.path, *seg.to_file_pos(line, col))
        return None

    @property
    def deps(self) -> str:
        """Dependencies, each wrapped in `module Name = struct ... end`; empty if none."""
        return self._layout.deps

    @property
    def entry(self) -> str:
        """
        Entry module, imports blanked out.

        Invariant:
            - Imports are blanked in place, so line k, column c of entry is line k, column
        c of the entry file. Only the blanked imports and trailing whitespace differ from
        what it was built from.

        """
        return self._layout.entry

    @property
    def iml(self) -> str:
        """The joined source: `deps`, a blank line, then `entry`."""
        return self.deps + '\n\n' + self.entry if self.deps else self.entry

    @cached_property
    def _layout(self) -> _Layout:
        """Build the joined source and its segments in one pass."""
        *deps, entry = self.modules
        parts: list[str] = []
        segments: list[SourceSegment] = []

        line = 1
        for mod in deps:
            content_lines = _lines(mod.content)
            indented = '\n'.join(
                f'  {ln}' if ln.strip() else '' for ln in content_lines
            )
            parts.append(f'module {mod.name} = struct\n{indented}\nend')
            # `module ... = struct` sits on `line`, the content right below it
            segments.append(
                SourceSegment(
                    path=mod.path,
                    start_line_in_joined=line + 1,
                    end_line_in_joined=line + len(content_lines),
                    join_line_offset=line,
                    join_col_offset=2,
                )
            )
            # header + content + `end` + blank separator
            line += len(content_lines) + 3

        entry_src = _rstrip_lines(
            blank_nodes(entry.src, [imp.import_stmt for imp in entry.imports])
        )
        segments.append(
            SourceSegment(
                path=entry.path,
                start_line_in_joined=line,
                end_line_in_joined=line + len(_lines(entry_src)) - 1,
                join_line_offset=line - 1,
                join_col_offset=0,
            )
        )
        return _Layout(
            deps='\n\n'.join(parts), entry=entry_src, segments=tuple(segments)
        )

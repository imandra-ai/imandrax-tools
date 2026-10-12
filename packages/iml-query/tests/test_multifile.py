"""Tests for multi-file module resolution."""

from pathlib import Path
from tempfile import NamedTemporaryFile

import pytest
from iml_query.multifile import (
    CircularImportError,
    IMLModuleNotFoundError,
    Library,
    NotImplementedImportError,
    gather_modules,
    resolve_or_error,
)
from inline_snapshot import snapshot

DATA_DIR = Path(__file__).parent / 'data' / 'multifile'


class TestResolveModules:
    def test_same_level(self):
        modules = resolve_or_error(DATA_DIR / 'same_level' / 'main.iml')
        assert not isinstance(modules, Exception)
        names = [m.name for m in modules]
        assert names == snapshot(['Helpers', 'Main'])

    def test_nested_levels(self):
        modules = resolve_or_error(DATA_DIR / 'nested_levels' / 'main.iml')
        assert not isinstance(modules, Exception)
        names = [m.name for m in modules]
        assert names == snapshot(['Utils', 'Core', 'Main'])

    def test_diamond_deps(self):
        modules = resolve_or_error(DATA_DIR / 'diamond_deps' / 'main.iml')
        assert not isinstance(modules, Exception)
        names = [m.name for m in modules]
        assert names == snapshot(['Base', 'Left', 'Right', 'Main'])

    def test_cycle_detection(self):
        result = resolve_or_error(DATA_DIR / 'cycle' / 'a.iml')
        assert isinstance(result, CircularImportError)

    def test_missing_import(self):
        result = resolve_or_error(DATA_DIR / 'missing_import' / 'main.iml')
        assert isinstance(result, IMLModuleNotFoundError)
        assert 'ghost.iml' in str(result)

    def test_findlib_import(self, tmp_path: Path):
        iml = tmp_path / 'main.iml'
        iml.write_text('[@@@import Foo, "findlib:foo.bar"]\nlet x = 1\n')
        result = resolve_or_error(iml)
        assert isinstance(result, NotImplementedImportError)
        assert 'findlib:' in str(result)

    def test_dune_import(self, tmp_path: Path):
        iml = tmp_path / 'main.iml'
        iml.write_text('[@@@import Foo, "dune:foo.bar"]\nlet x = 1\n')
        result = resolve_or_error(iml)
        assert isinstance(result, NotImplementedImportError)
        assert 'dune:' in str(result)

    def test_imports_stripped_from_content(self):
        modules = resolve_or_error(DATA_DIR / 'same_level' / 'main.iml')
        assert not isinstance(modules, Exception)
        main = modules[-1]
        assert '[@@@import' not in main.content
        assert 'Helpers.double' in main.content


class TestLibraryMonolith:
    def test_no_import(self):
        iml = """
let x = 1

verify (fun x -> x = x)
"""
        with NamedTemporaryFile() as f:
            f.write(iml.encode())
            f.flush()
            m = gather_modules(Path(f.name))
        gathered = (m.deps, m.entry)
        assert gathered == snapshot(
            (
                '',
                """\

let x = 1

verify (fun x -> x = x)
""",
            )
        )

    def test_same_level(self):
        modules = resolve_or_error(DATA_DIR / 'same_level' / 'main.iml')
        assert not isinstance(modules, Exception)
        lib = Library(modules=tuple(modules))
        result = (lib.deps, lib.entry)
        assert result == snapshot(
            (
                """\
module Helpers = struct
  let double (x : int) : int = x * 2

  let triple (x : int) : int = x * 3
end\
""",
                """\


let six : int = Helpers.double 3

let nine : int = Helpers.triple 3
""",
            )
        )

    def test_nested_levels(self):
        modules = resolve_or_error(DATA_DIR / 'nested_levels' / 'main.iml')
        assert not isinstance(modules, Exception)
        lib = Library(modules=tuple(modules))
        result = (lib.deps, lib.entry)
        assert result == snapshot(
            (
                """\
module Utils = struct
  let add (x : int) (y : int) : int = x + y
end

module Core = struct


  let sum_three (a : int) (b : int) (c : int) : int =
    Utils.add (Utils.add a b) c
end\
""",
                """\


let result : int = Core.sum_three 1 2 3
""",
            )
        )

    def test_diamond_deps(self):
        modules = resolve_or_error(DATA_DIR / 'diamond_deps' / 'main.iml')
        assert not isinstance(modules, Exception)
        lib = Library(modules=tuple(modules))
        result = (lib.deps, lib.entry)
        assert result == snapshot(
            (
                """\
module Base = struct
  let zero : int = 0

  let one : int = 1
end

module Left = struct


  let left_val : int = Base.one
end

module Right = struct


  let right_val : int = Base.zero
end\
""",
                """\



let result : int = Left.left_val + Right.right_val
""",
            )
        )

    def test_with_vgs(self):
        modules = resolve_or_error(DATA_DIR / 'with_vg' / 'main.iml')
        assert not isinstance(modules, Exception)
        lib = Library(modules=tuple(modules))
        result = (lib.deps, lib.entry)
        assert result == snapshot(
            (
                """\
module Base = struct
  let x = 1
  let f x = x + 1
end\
""",
                """\


let y = Base.f 1

eval y

theorem eval_y = y = 2
""",
            )
        )

    def test_alias(self):
        modules = resolve_or_error(DATA_DIR / 'alias' / 'main.iml')
        assert not isinstance(modules, Exception)
        lib = Library(modules=tuple(modules))
        result = (lib.deps, lib.entry)
        assert result == snapshot(
            (
                """\
module Base = struct
  (* Intentional illegal module name *)
  type a = string
end\
""",
                """\


open Base

type b = a
""",
            )
        )


class TestMonolithSourceMap:
    # ai-disclosure: ai-generated

    @pytest.mark.parametrize(
        'case', ['same_level', 'nested_levels', 'diamond_deps', 'with_vg', 'alias']
    )
    def test_lines_map_back(self, case: str):
        """Lines of the joined source map back to their file's lines, in order from line 1."""
        m = gather_modules(DATA_DIR / case / 'main.iml')
        mapped: dict[Path, list[int]] = {}
        for line, joined in enumerate(m.iml.split('\n'), 1):
            loc = m.locate(line, 1)
            if loc is None:
                continue
            path, orig_line, _ = loc
            orig = path.read_text().split('\n')[orig_line - 1]
            # Same text, or blank where an import / VG / decomp was stripped
            assert joined.strip() in (orig.strip(), '')
            mapped.setdefault(path, []).append(orig_line)
        assert set(mapped) == {mod.path for mod in m.modules}
        for lines in mapped.values():
            assert lines == list(range(1, len(lines) + 1))

    def test_entry_and_wrappers(self):
        m = gather_modules(DATA_DIR / 'with_vg' / 'main.iml')
        assert m.iml == m.deps + '\n\n' + m.entry
        entry_path = (DATA_DIR / 'with_vg' / 'main.iml').resolve()
        assert m.modules[-1].path == entry_path
        lines = m.iml.split('\n')
        # `module Base = struct` and `end` belong to no file
        assert lines[0] == 'module Base = struct'
        assert m.locate(1, 1) is None
        end_line = lines.index('end') + 1
        assert m.locate(end_line, 1) is None
        # `let y = Base.f 1` is line 3 of main.iml
        y_line = lines.index('let y = Base.f 1') + 1
        assert m.locate(y_line, 5) == (entry_path, 3, 5)
        # `let f x = x + 1` is line 2 of base.iml, shifted by the indentation
        f_line = lines.index('  let f x = x + 1') + 1
        base = (DATA_DIR / 'with_vg' / 'base.iml').resolve()
        assert m.locate(f_line, 7) == (base, 2, 5)

    def test_stripped_vg_keeps_lines(self, tmp_path: Path):
        """A multi-line `verify` in a dependency does not shift the lines below it."""
        (tmp_path / 'lib.iml').write_text(
            'let a = 1\n\nverify (fun x ->\n  x + 0 = x)\n\nlet b = 2\n'
        )
        (tmp_path / 'main.iml').write_text('[@@@import "lib.iml"]\nlet c = Lib.b\n')
        m = gather_modules(tmp_path / 'main.iml')
        lines = m.iml.split('\n')
        b_line = lines.index('  let b = 2') + 1
        assert m.locate(b_line, 3) == ((tmp_path / 'lib.iml').resolve(), 6, 1)
        c_line = lines.index('let c = Lib.b') + 1
        assert m.locate(c_line, 1) == ((tmp_path / 'main.iml').resolve(), 2, 1)

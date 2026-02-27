"""Tests for multi-file module resolution."""

from pathlib import Path
from tempfile import NamedTemporaryFile

from inline_snapshot import snapshot

from iml_query.multifile import (
    CircularImportError,
    IMLModuleNotFoundError,
    NotImplementedImportError,
    gather_modules,
    mk_monolith_iml,
    resolve,
)

DATA_DIR = Path(__file__).parent / 'data' / 'multifile'


class TestResolveModules:
    def test_same_level(self):
        modules = resolve(DATA_DIR / 'same_level' / 'main.iml')
        assert not isinstance(modules, Exception)
        names = [m.name for m in modules]
        assert names == snapshot(['Helpers', 'Main'])

    def test_nested_levels(self):
        modules = resolve(DATA_DIR / 'nested_levels' / 'main.iml')
        assert not isinstance(modules, Exception)
        names = [m.name for m in modules]
        assert names == snapshot(['Utils', 'Core', 'Main'])

    def test_diamond_deps(self):
        modules = resolve(DATA_DIR / 'diamond_deps' / 'main.iml')
        assert not isinstance(modules, Exception)
        names = [m.name for m in modules]
        assert names == snapshot(['Base', 'Left', 'Right', 'Main'])

    def test_cycle_detection(self):
        result = resolve(DATA_DIR / 'cycle' / 'a.iml')
        assert isinstance(result, CircularImportError)

    def test_missing_import(self):
        result = resolve(DATA_DIR / 'missing_import' / 'main.iml')
        assert isinstance(result, IMLModuleNotFoundError)
        assert 'ghost.iml' in str(result)

    def test_findlib_import(self, tmp_path: Path):
        iml = tmp_path / 'main.iml'
        iml.write_text('[@@@import Foo, "findlib:foo.bar"]\nlet x = 1\n')
        result = resolve(iml)
        assert isinstance(result, NotImplementedImportError)
        assert 'findlib:' in str(result)

    def test_dune_import(self, tmp_path: Path):
        iml = tmp_path / 'main.iml'
        iml.write_text('[@@@import Foo, "dune:foo.bar"]\nlet x = 1\n')
        result = resolve(iml)
        assert isinstance(result, NotImplementedImportError)
        assert 'dune:' in str(result)

    def test_imports_stripped_from_content(self):
        modules = resolve(DATA_DIR / 'same_level' / 'main.iml')
        assert not isinstance(modules, Exception)
        main = modules[-1]
        assert '[@@@import' not in main.content
        assert 'Helpers.double' in main.content


class TestMkMonolithIml:
    def test_no_import(self):
        iml = """
let x = 1
"""
        with NamedTemporaryFile() as f:
            f.write(iml.encode())
            f.flush()
            gathered = gather_modules(Path(f.name))
        assert gathered == snapshot("""\
let x = 1

""")

    def test_same_level(self):
        modules = resolve(DATA_DIR / 'same_level' / 'main.iml')
        assert not isinstance(modules, Exception)
        result = mk_monolith_iml(modules)
        assert result == snapshot("""\
module Helpers = struct
  let double (x : int) : int = x * 2

  let triple (x : int) : int = x * 3
end

let six : int = Helpers.double 3

let nine : int = Helpers.triple 3

""")

    def test_nested_levels(self):
        modules = resolve(DATA_DIR / 'nested_levels' / 'main.iml')
        assert not isinstance(modules, Exception)
        result = mk_monolith_iml(modules)
        assert result == snapshot("""\
module Utils = struct
  let add (x : int) (y : int) : int = x + y
end

module Core = struct
  let sum_three (a : int) (b : int) (c : int) : int =
    Utils.add (Utils.add a b) c
end

let result : int = Core.sum_three 1 2 3

""")

    def test_diamond_deps(self):
        modules = resolve(DATA_DIR / 'diamond_deps' / 'main.iml')
        assert not isinstance(modules, Exception)
        result = mk_monolith_iml(modules)
        assert result == snapshot("""\
module Base = struct
  let zero : int = 0

  let one : int = 1
end

module Left = struct
  let left_val : int = Base.one
end

module Right = struct
  let right_val : int = Base.zero
end

let result : int = Left.left_val + Right.right_val

""")

    def test_with_vgs(self):
        modules = resolve(DATA_DIR / 'with_vg' / 'main.iml')
        assert not isinstance(modules, Exception)
        result = mk_monolith_iml(modules)
        assert result == snapshot("""\
module Base = struct
  let x = 1
  let f x = x + 1
end

let y = Base.f 1

eval y

theorem eval_y = y = 2

""")

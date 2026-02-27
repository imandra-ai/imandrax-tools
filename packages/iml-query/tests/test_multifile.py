"""Tests for multi-file module resolution."""

from pathlib import Path

import pytest
from inline_snapshot import snapshot

from iml_query.multifile import (
    CircularImportError,
    ModuleNotFoundError,
    NotImplementedImportError,
    mk_monolith_iml,
    parse_imports,
    resolve_modules,
)

DATA_DIR = Path(__file__).parent / 'data' / 'multifile'


class TestParseImports:
    def test_import_1_named_path(self):
        """[@@@import Mod, "path/to/file.iml"]"""
        imports = parse_imports('[@@@import Helpers, "helpers.iml"]')
        assert len(imports) == 1
        assert imports[0].module_name == 'Helpers'
        assert imports[0].path == 'helpers.iml'
        assert imports[0].extraction_name is None

    def test_import_2_with_extraction(self):
        """[@@@import Mod, "path", Mod2]"""
        imports = parse_imports('[@@@import Foo, "lib/foo.iml", Bar]')
        assert len(imports) == 1
        assert imports[0].module_name == 'Foo'
        assert imports[0].path == 'lib/foo.iml'
        assert imports[0].extraction_name == 'Bar'

    def test_import_3_path_only(self):
        """[@@@import "path/to/file.iml"]"""
        imports = parse_imports('[@@@import "utils.iml"]')
        assert len(imports) == 1
        assert imports[0].module_name == 'Utils'
        assert imports[0].path == 'utils.iml'
        assert imports[0].extraction_name is None

    def test_path_only_module_name_derivation(self):
        """Module name derived from filename stem, capitalized."""
        imports = parse_imports('[@@@import "path/to/my_helpers.iml"]')
        assert imports[0].module_name == 'My_helpers'

    def test_multiple_imports(self):
        code = '[@@@import Left, "left.iml"]\n[@@@import Right, "right.iml"]\n'
        imports = parse_imports(code)
        assert len(imports) == 2
        assert imports[0].module_name == 'Left'
        assert imports[1].module_name == 'Right'

    def test_no_imports(self):
        imports = parse_imports('let x : int = 42')
        assert imports == []


class TestResolveModules:
    def test_same_level(self):
        modules = resolve_modules(DATA_DIR / 'same_level' / 'main.iml')
        names = [m.name for m in modules]
        assert names == snapshot(['Helpers', 'Main'])

    def test_nested_levels(self):
        modules = resolve_modules(DATA_DIR / 'nested_levels' / 'main.iml')
        names = [m.name for m in modules]
        # Utils is leaf, Core depends on Utils, Main depends on Core
        assert names == snapshot(['Utils', 'Core', 'Main'])

    def test_diamond_deps(self):
        modules = resolve_modules(DATA_DIR / 'diamond_deps' / 'main.iml')
        names = [m.name for m in modules]
        # Base first (shared dep), then Left and Right, then Main
        assert names == snapshot(['Base', 'Left', 'Right', 'Main'])

    def test_cycle_detection(self):
        with pytest.raises(CircularImportError):
            resolve_modules(DATA_DIR / 'cycle' / 'a.iml')

    def test_missing_import(self):
        with pytest.raises(ModuleNotFoundError, match='ghost.iml'):
            resolve_modules(DATA_DIR / 'missing_import' / 'main.iml')

    def test_findlib_import(self, tmp_path: Path):
        iml = tmp_path / 'main.iml'
        iml.write_text('[@@@import Foo, "findlib:foo.bar"]\nlet x = 1\n')
        with pytest.raises(NotImplementedImportError, match='findlib:'):
            resolve_modules(iml)

    def test_dune_import(self, tmp_path: Path):
        iml = tmp_path / 'main.iml'
        iml.write_text('[@@@import Foo, "dune:foo.bar"]\nlet x = 1\n')
        with pytest.raises(NotImplementedImportError, match='dune:'):
            resolve_modules(iml)

    def test_imports_stripped_from_content(self):
        modules = resolve_modules(DATA_DIR / 'same_level' / 'main.iml')
        main = modules[-1]
        assert '[@@@import' not in main.content
        assert 'Helpers.double' in main.content


# --- mk_monolith_iml tests ---


class TestMkMonolithIml:
    def test_same_level(self):
        modules = resolve_modules(DATA_DIR / 'same_level' / 'main.iml')
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
        modules = resolve_modules(DATA_DIR / 'nested_levels' / 'main.iml')
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
        modules = resolve_modules(DATA_DIR / 'diamond_deps' / 'main.iml')
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

"""Tests for IML import statement parsing."""

from iml_query.multifile import parse_imports


def test_named_path_import():
    """[@@@import Mod, "path/to/file.iml"]"""
    imports = parse_imports('[@@@import Helpers, "helpers.iml"]')
    assert len(imports) == 1
    assert imports[0].module_name == 'Helpers'
    assert imports[0].path == 'helpers.iml'
    assert imports[0].extraction_name is None


def test_named_path_with_extraction():
    """[@@@import Mod, "path", Mod2]"""
    imports = parse_imports('[@@@import Foo, "lib/foo.iml", Bar]')
    assert len(imports) == 1
    assert imports[0].module_name == 'Foo'
    assert imports[0].path == 'lib/foo.iml'
    assert imports[0].extraction_name == 'Bar'


def test_path_only_import():
    """[@@@import "path/to/file.iml"]"""
    imports = parse_imports('[@@@import "utils.iml"]')
    assert len(imports) == 1
    assert imports[0].module_name == 'Utils'
    assert imports[0].path == 'utils.iml'
    assert imports[0].extraction_name is None


def test_path_only_module_name_derivation():
    """Module name derived from filename stem, capitalized."""
    imports = parse_imports('[@@@import "path/to/my_helpers.iml"]')
    assert imports[0].module_name == 'My_helpers'


def test_multiple_imports():
    code = '[@@@import Left, "left.iml"]\n[@@@import Right, "right.iml"]\n'
    imports = parse_imports(code)
    assert len(imports) == 2
    assert imports[0].module_name == 'Left'
    assert imports[1].module_name == 'Right'


def test_no_imports():
    imports = parse_imports('let x : int = 42')
    assert imports == []


def test_findlib_path():
    imports = parse_imports('[@@@import Foo, "findlib:foo.bar"]')
    assert len(imports) == 1
    assert imports[0].module_name == 'Foo'
    assert imports[0].path == 'findlib:foo.bar'


def test_dune_path():
    imports = parse_imports('[@@@import Foo, "dune:foo.bar"]')
    assert len(imports) == 1
    assert imports[0].module_name == 'Foo'
    assert imports[0].path == 'dune:foo.bar'


def test_mixed_import_forms():
    code = """\
[@@@import Foo, "foo.iml"]
[@@@import Bar, "bar.iml", Baz]
[@@@import "qux.iml"]
"""
    imports = parse_imports(code)
    assert len(imports) == 3
    assert imports[0].module_name == 'Foo'
    assert imports[0].extraction_name is None
    assert imports[1].module_name == 'Bar'
    assert imports[1].extraction_name == 'Baz'
    assert imports[2].module_name == 'Qux'
    assert imports[2].extraction_name is None

def test_package_import():
    """Test that the package can be imported."""
    import imandrax_tools  # noqa: F401


def test_api_models_namespace_import():
    """Test that api_models namespace is accessible."""
    from imandrax_tools.api_models import Art, EvalRes  # noqa: F401


def test_iml_query_namespace_import():
    """Test that iml_query namespace is accessible."""
    from imandrax_tools.iml_query import queries  # noqa: F401

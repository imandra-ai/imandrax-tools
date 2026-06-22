def test_package_import():
    import imandrax_tools  # noqa: F401


def test_api_models_namespace_import():
    from imandrax_tools.api_models import Art, EvalRes  # noqa: F401


def test_iml_query_namespace_import():
    from imandrax_tools.iml_query import queries  # noqa: F401

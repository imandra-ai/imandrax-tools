def test_package_import():
    import imandrax_tools  # noqa: F401


def test_api_models_namespace_import():
    from imandrax_tools.api_models import Art, EvalRes  # noqa: F401


def test_iml_query_namespace_import():
    from imandrax_tools.iml_query import queries  # noqa: F401


def test_goal_state_namespace_import():
    """Test that iml_query namespace is accessible."""
    from imandrax_tools.goal_state import format_goal_state_from_bytes  # noqa: F401

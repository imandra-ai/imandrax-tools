"""ImandraX Tools - Common utilities for interacting with ImandraX / IML.

Umbrella package that bundles iml-query and imandrax-api-models.

Usage:
    from imandrax_tools.api_models import ...
    from imandrax_tools.iml_query import ...
"""

import sys

import imandrax_api_models as api_models
import iml_query

sys.modules['imandrax_tools.api_models'] = api_models
sys.modules['imandrax_tools.iml_query'] = iml_query

__all__ = ['api_models', 'iml_query']

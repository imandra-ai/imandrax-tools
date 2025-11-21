# pyright: reportUnknownMemberType=false, reportUnknownVariableType=false
# ruff: noqa: RUF100, F401

from typing import TYPE_CHECKING

import yaml
from pydantic import BaseModel
from yaml import Dumper

if TYPE_CHECKING:
    from yaml import Dumper
else:
    try:
        # Better performance
        from yaml import CDumper as Dumper
    except ImportError:
        from yaml import Dumper

from imandrax_api_models import Art, ModelType


class ImandraXAPIModelDumper(Dumper):
    """
    Custom dumper for ImandraX API models.

    - for mulitline strings, use literal block style
    - for enums, use the enum name
    """


# ====================
# Custom representer definitions
# ====================


def str_representer(dumper: Dumper, data: str):
    """If the string contains newlines, represent it as a literal block."""
    if '\n' in data:
        return dumper.represent_scalar('tag:yaml.org,2002:str', data, style='|')
    return dumper.represent_scalar('tag:yaml.org,2002:str', data)


def model_type_representer(dumper: Dumper, data: ModelType):
    """Represent a ModelType as the enum name."""
    return dumper.represent_scalar('tag:yaml.org,2002:str', data.value)


def artifact_representer(dumper: Dumper, data: Art):
    """Ignore artifact."""
    return dumper.represent_none(None)


def basemodel_representer(dumper: Dumper, data: BaseModel):
    return dumper.represent_dict(data.model_dump())


# ====================
# Register custom representer
# ====================


ImandraXAPIModelDumper.add_representer(str, str_representer)
ImandraXAPIModelDumper.add_representer(ModelType, model_type_representer)
ImandraXAPIModelDumper.add_representer(Art, artifact_representer)
ImandraXAPIModelDumper.add_representer(BaseModel, basemodel_representer)

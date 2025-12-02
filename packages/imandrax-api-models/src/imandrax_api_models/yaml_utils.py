# pyright: reportUnknownMemberType=false, reportUnknownVariableType=false

from enum import Enum
from typing import TYPE_CHECKING

from pydantic import BaseModel

if TYPE_CHECKING:
    from yaml import Dumper
else:
    try:
        # Better performance
        from yaml import CDumper as Dumper
    except ImportError:
        from yaml import Dumper

from imandrax_api_models import ErrorKind, ModelType


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
    """
    If the string contains newlines, represent it as a literal block.

    Note: PyYAML refuses to use literal block style for strings with trailing
    whitespace on any line, so we strip trailing whitespace to enable literal blocks.
    """
    if '\n' in data:
        # Strip trailing whitespace from each line to allow literal block style
        data = '\n'.join(line.rstrip() for line in data.split('\n'))
        return dumper.represent_scalar('tag:yaml.org,2002:str', data, style='|')
    return dumper.represent_scalar('tag:yaml.org,2002:str', data)


def enum_representer(dumper: Dumper, data: Enum):
    return dumper.represent_scalar('tag:yaml.org,2002:str', data.value)


def basemodel_representer(dumper: Dumper, data: BaseModel):
    return dumper.represent_dict(data.model_dump())


# ====================
# Register custom representer
# ====================


ImandraXAPIModelDumper.add_representer(str, str_representer)
ImandraXAPIModelDumper.add_representer(ModelType, enum_representer)
ImandraXAPIModelDumper.add_representer(ErrorKind, enum_representer)
ImandraXAPIModelDumper.add_representer(BaseModel, basemodel_representer)

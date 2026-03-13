"""Aggregate data"""

# pyright: basic
# ruff: noqa: RUF100, F401
from pathlib import Path

import yaml

from error_corpus.schema import Item


def agg() -> list[Item]:
    data_dir = Path(__file__).parent.parent / "data"
    data_paths = list(data_dir.glob("**/data.yaml"))
    all_items = []
    for data_path in data_paths:
        with data_path.open("r") as f:
            items_dict = yaml.safe_load(f)
            items = [Item.model_validate(item) for item in items_dict]
            all_items.extend(items)

    return all_items


def str_representer(dumper: yaml.Dumper, data: str):
    """
    If the string contains newlines, represent it as a literal block.

    Note: PyYAML refuses to use literal block style for strings with trailing
    whitespace on any line, so we strip trailing whitespace to enable literal blocks.
    """
    if "\n" in data:
        # Strip trailing whitespace from each line to allow literal block style
        data = "\n".join(line.rstrip() for line in data.split("\n"))
        return dumper.represent_scalar("tag:yaml.org,2002:str", data, style="|")
    return dumper.represent_scalar("tag:yaml.org,2002:str", data)


yaml.add_representer(str, str_representer)


if __name__ == "__main__":
    items_dict = [i.model_dump(mode="json") for i in agg()]
    print(yaml.dump(items_dict, sort_keys=False))

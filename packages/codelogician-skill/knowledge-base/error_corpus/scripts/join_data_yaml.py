"""Aggregate data"""

# pyright: basic
# ruff: noqa: RUF100, F401
from pathlib import Path

import yaml

from error_corpus.schema import Item


def agg() -> list[Item]:
    data_dir = Path(__file__).parent / "data"
    data_paths = list(data_dir.glob("**/data.yaml"))
    all_items = []
    for data_path in data_paths:
        with data_path.open("r") as f:
            items_dict = yaml.safe_load(f)
            items = [Item.model_validate(item) for item in items_dict]
            all_items.extend(items)

    return all_items


if __name__ == "__main__":
    agg()

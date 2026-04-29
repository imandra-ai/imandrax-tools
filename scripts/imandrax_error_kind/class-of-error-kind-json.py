"""Generate class body of ErrorKind enum from error_kind.json"""
# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "structlog",
# ]
# ///

import json
from pathlib import Path

import structlog

logger = structlog.get_logger(__name__)


def load_data() -> list[dict[str, str]]:
    import sys

    if not sys.stdin.isatty():
        data = json.load(sys.stdin)
        return data
    data_p = Path(__file__).parent / "error_kind.json"
    return json.loads(data_p.read_text())


def main() -> None:
    upper_snake_name_lst_by_row = []
    descriptions = []
    for item in load_data():
        camel_name = item["name"]
        descriptions.append(item.get("description", ""))
        upper_snake_name_lst = []
        buf = ""
        for i, c in enumerate(camel_name):
            if i == 0:
                buf += c
                continue
            prev = camel_name[i - 1]
            nxt = camel_name[i + 1] if i + 1 < len(camel_name) else ""
            # boundary cases:
            #   1. lower -> upper  (end of a regular word: "Term|Model")
            #   2. upper -> upper followed by lower  (end of acronym: "FI|Err")
            split = False
            if c.isupper() and prev.islower():
                split = True
            elif c.isupper() and prev.isupper() and nxt.islower():
                split = True
            if split:
                upper_snake_name_lst.append(buf)
                logger.info(
                    "appending to upper_snake_name_lst",
                    buf=buf,
                    len_lst=len(upper_snake_name_lst),
                )
                buf = c
            else:
                buf += c
        upper_snake_name_lst.append(buf)
        logger.info(f"{camel_name} -> {upper_snake_name_lst}")
        upper_snake_name_lst_by_row.append(upper_snake_name_lst)

    logger.info("done", len=len(upper_snake_name_lst_by_row))
    for name_lst, description in zip(upper_snake_name_lst_by_row, descriptions):
        # s = f"{'_'.join(map(str.upper, name_lst))} = '{''.join(name_lst)}'"
        # one_line = " ".join(description.split())
        # s += f"  # {one_line}"
        # print(" " * 4 + s)

        enum_name = "_".join(map(str.upper, name_lst))
        enum_value_fst = f"'{''.join(name_lst)}'"
        enum_value_snd = f"'{description}'"
        print(" " * 4 + f"{enum_name} = ({enum_value_fst}, {enum_value_snd})")


if __name__ == "__main__":
    import logging
    import os

    log_level = os.environ.get("LOG_LEVEL", "INFO").upper()

    structlog.configure(
        wrapper_class=structlog.make_filtering_bound_logger(
            getattr(logging, log_level, logging.INFO)
        ),
        processors=[
            structlog.dev.ConsoleRenderer(),
        ],
    )
    main()

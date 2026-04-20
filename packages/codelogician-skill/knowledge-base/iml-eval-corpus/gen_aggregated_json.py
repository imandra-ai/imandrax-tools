"""Aggregate per-entry corpus dirs into a single `error_corpus.json`.

Each leaf entry dir contains at minimum `repro.iml`; optional siblings:
`solution.iml`, `explanation.md`, `eval_res.json`, `.is_po_error`.

Entries under the grouping dir `_unknown-id-ocaml-stdlib/<name>/` are
emitted with a flattened name `unknown-id-ocaml-stdlib-<name>` to match
the flat-mirror convention.

Run from this file's directory. Output goes to stdout; dune captures it
into `error_corpus.json`.
"""

# pyright: basic
import json
from pathlib import Path
from typing import Any

GROUP_DIR = "_unknown-id-ocaml-stdlib"
GROUP_FLAT_PREFIX = "unknown-id-ocaml-stdlib"
ERR_MSG_JQ = ".eval_res | (.errors[0] // .po_results[0].errors[0]) | .msg"


def read_text(p: Path) -> str | None:
    return p.read_text() if p.is_file() else None


def extract_err_msg(eval_res_path: Path) -> dict[str, Any] | None:
    if not eval_res_path.is_file():
        return None
    data = json.loads(eval_res_path.read_text())
    eval_res = data.get("eval_res") or {}
    errors = eval_res.get("errors") or []
    if errors:
        return errors[0].get("msg")
    po_results = eval_res.get("po_results") or []
    if po_results:
        po_errors = po_results[0].get("errors") or []
        if po_errors:
            return po_errors[0].get("msg")
    return None


def entry_for(name: str, entry_dir: Path) -> dict[str, Any]:
    item = {
        "name": name,
        "repro": read_text(entry_dir / "repro.iml"),
        ERR_MSG_JQ: extract_err_msg(entry_dir / "eval_res.json"),
        "is_po_err": (entry_dir / ".is_po_error").is_file(),
        "solution": read_text(entry_dir / "solution.iml"),
        "explanation": read_text(entry_dir / "explanation.md"),
    }
    return item


def main() -> None:
    here = Path(__file__).parent
    entries: list[tuple[str, Path]] = []

    for child in sorted(here.iterdir()):
        if not child.is_dir() or child.name.startswith("."):
            continue
        if child.name == GROUP_DIR:
            for sub in sorted(child.iterdir()):
                if sub.is_dir() and (sub / "repro.iml").is_file():
                    entries.append((f"{GROUP_FLAT_PREFIX}-{sub.name}", sub))
            continue
        if child.name.startswith("_"):
            continue
        if (child / "repro.iml").is_file():
            entries.append((child.name, child))

    items = [entry_for(name, path) for name, path in entries]
    out = {"items": items}
    print(json.dumps(out, indent=2))


if __name__ == "__main__":
    main()

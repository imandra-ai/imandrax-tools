#! /usr/bin/env uv run
# /// script
# requires-python = ">=3.13"
# dependencies = ["pyyaml"]
# ///
# pyright: basic
"""Extract tactics documentation from prelude.iml.

Requirements
============

Input
-----
- Source file: prelude.iml (IML/OCaml file containing tactic definitions)
- A tactic is identified by having a `[@@builtin.tac "fn.tac..."]` attribute

Tactic Extraction
-----------------
For each tactic, extract the following information:
- name: the identifier from the let binding (e.g., `skip`, `intros`)
- tac_id: the full tactic ID from the attribute (e.g., "fn.tac.skip")
- signature: the type annotation (e.g., `t`, `string -> t`, `int list -> t`)
- documentation: the OCaml doc comment `(** ... *)` preceding the definition
- documentation_md: markdown version of the documentation

Assertion: the number of extracted tactics must match the number of
`[@@builtin.tac "fn.tac.*"]` attributes found in the source file.

Notation Extraction
-------------------
Additionally, extract PPX notation documentation from tactic docstrings:
- Notations are patterns like `[%skip ...]`, `[%expand ...]`, `[%simplify ...]`
- In OCamldoc, these appear as either:
  - Double brackets: `[[%skip "msg"]]` (OCamldoc escaping for literal brackets)
  - Backticks: `` `[%skip "msg"]` `` (inline code style)
- Extract the containing paragraph (not just the line) to preserve multi-line
  backtick spans that may cross line boundaries
- Deduplicate first: if multiple tactics document the same notation (e.g., `skip`
  and `skip_msg` both mention `[%skip]`), assign to a primary tactic (prefer
  tactic whose name matches the notation name, else first occurrence)
- Then group by source tactic: each tactic key contains all its notations
- For each notation, store:
  - name: the notation name (e.g., "skip", "simplify")
  - patterns: list of concrete pattern examples (e.g., `[%skip]`, `[%skip "msg"]`)
  - description: the extracted paragraph(s) describing the notation
  - description_md: markdown version of the description

Markdown Conversion
-------------------
Convert OCamldoc syntax to markdown:
- `[code]` -> `` `code` `` (code spans)
- `{i text}` -> `*text*` (italic)
- `{b text}` -> `**text**` (bold)
- `{e text}` -> `*text*` (emphasis)
- Handle nested brackets correctly
- Preserve content already inside backticks (don't double-convert)

Output
------
- Print summary to stdout
- Save to tactics.yaml with structure:
  tactics: [...]
  notations: {...}
"""

import re
from pathlib import Path

import yaml


def ocamldoc_to_markdown(doc: str) -> str:
    """Convert OCaml doc comment syntax to markdown.

    - [code] -> `code`
    - {i text} -> *text* (italic)
    - {b text} -> **text** (bold)
    - {e text} -> *text* (emphasis)

    Preserves content already inside backticks.
    """
    if not doc:
        return ""

    result = doc

    # Convert [code] to `code` - handle nested brackets, skip content in backticks
    def replace_code_brackets(text: str) -> str:
        output = []
        i = 0
        while i < len(text):
            if text[i] == "`":
                # Skip content inside backticks
                j = i + 1
                while j < len(text) and text[j] != "`":
                    j += 1
                output.append(text[i : j + 1])
                i = j + 1
            elif text[i] == "[":
                # Find matching ]
                depth = 1
                j = i + 1
                while j < len(text) and depth > 0:
                    if text[j] == "[":
                        depth += 1
                    elif text[j] == "]":
                        depth -= 1
                    j += 1
                if depth == 0:
                    # Found matching bracket
                    inner = text[i + 1 : j - 1]
                    output.append("`" + inner + "`")
                    i = j
                else:
                    output.append(text[i])
                    i += 1
            else:
                output.append(text[i])
                i += 1
        return "".join(output)

    result = replace_code_brackets(result)

    # Convert {i text} to *text*
    result = re.sub(r"\{i\s+([^}]+)\}", r"*\1*", result)

    # Convert {b text} to **text**
    result = re.sub(r"\{b\s+([^}]+)\}", r"**\1**", result)

    # Convert {e text} to *text*
    result = re.sub(r"\{e\s+([^}]+)\}", r"*\1*", result)

    # Replace Unicode ellipsis with ASCII
    result = result.replace("\u2026", "...")

    return result


def count_tactic_attributes(content: str) -> int:
    """Count the number of [@@builtin.tac "fn.tac.*"] attributes in the file."""
    pattern = re.compile(r'\[@@builtin\.tac\s+"fn\.tac\.[^"]+"\]')
    return len(pattern.findall(content))


def extract_notations(tactics: list[dict]) -> dict[str, dict]:
    """Extract notation documentation from tactic docstrings.

    Looks for patterns like [[%skip ...]] or `[%skip ...]` in documentation.

    Process:
    1. First pass: collect all notations with their source tactics
    2. Dedupe: assign each notation to a single "primary" source tactic
       (prefer tactic whose name matches the notation, else first occurrence)
    3. Group by source tactic: each tactic key contains all its notations

    Returns a dict keyed by source tactic name.
    """
    # Patterns to find notations:
    # 1. [[%...]] - double brackets in OCamldoc
    # 2. `[%...]` - backtick style
    double_bracket_pattern = re.compile(r"\[\[%(\w+)([^\]]*)\]\]")
    backtick_pattern = re.compile(r"`\[%(\w+)([^\]`]*)\]`")

    # Step 1: Collect all notations with source info
    # Key: notation_name, Value: {tactic_name: {"patterns": [...], "paragraphs": [...]}}
    all_notations: dict[str, dict[str, dict]] = {}

    for tac in tactics:
        doc = tac["documentation"]
        if not doc:
            continue

        # Split into paragraphs (separated by blank lines) for extraction
        paragraphs = re.split(r"\n\s*\n", doc)

        for para in paragraphs:
            notation_names_in_para: list[str] = []
            patterns_in_para: dict[str, list[str]] = {}

            # Find double-bracket patterns [[%...]]
            for match in double_bracket_pattern.finditer(para):
                notation_name = match.group(1)
                args = match.group(2)
                pattern = f"[%{notation_name}{args}]"
                if notation_name not in notation_names_in_para:
                    notation_names_in_para.append(notation_name)
                if notation_name not in patterns_in_para:
                    patterns_in_para[notation_name] = []
                if pattern not in patterns_in_para[notation_name]:
                    patterns_in_para[notation_name].append(pattern)

            # Find backtick patterns `[%...]`
            for match in backtick_pattern.finditer(para):
                notation_name = match.group(1)
                args = match.group(2)
                pattern = f"[%{notation_name}{args}]"
                if notation_name not in notation_names_in_para:
                    notation_names_in_para.append(notation_name)
                if notation_name not in patterns_in_para:
                    patterns_in_para[notation_name] = []
                if pattern not in patterns_in_para[notation_name]:
                    patterns_in_para[notation_name].append(pattern)

            # Record findings
            if notation_names_in_para:
                cleaned_para = " ".join(para.split())
                for notation_name in notation_names_in_para:
                    if notation_name not in all_notations:
                        all_notations[notation_name] = {}
                    if tac["name"] not in all_notations[notation_name]:
                        all_notations[notation_name][tac["name"]] = {
                            "patterns": [],
                            "paragraphs": [],
                        }
                    entry = all_notations[notation_name][tac["name"]]
                    for p in patterns_in_para.get(notation_name, []):
                        if p not in entry["patterns"]:
                            entry["patterns"].append(p)
                    if cleaned_para not in entry["paragraphs"]:
                        entry["paragraphs"].append(cleaned_para)

    # Step 2: Dedupe - assign each notation to primary source tactic
    # Prefer tactic whose name matches the notation name, else first occurrence
    notation_to_primary: dict[str, str] = {}
    for notation_name, tactic_data in all_notations.items():
        tactic_names = list(tactic_data.keys())
        if notation_name in tactic_names:
            # Prefer tactic with matching name
            notation_to_primary[notation_name] = notation_name
        else:
            # Use first tactic
            notation_to_primary[notation_name] = tactic_names[0]

    # Step 3: Group by source tactic
    notations_by_tactic: dict[str, dict] = {}

    for notation_name, primary_tactic in notation_to_primary.items():
        if primary_tactic not in notations_by_tactic:
            notations_by_tactic[primary_tactic] = {"notations": []}

        # Merge data from all tactics that mention this notation
        all_patterns = []
        all_paragraphs = []
        for tactic_name, data in all_notations[notation_name].items():
            for p in data["patterns"]:
                if p not in all_patterns:
                    all_patterns.append(p)
            for para in data["paragraphs"]:
                if para not in all_paragraphs:
                    all_paragraphs.append(para)

        description = "\n\n".join(all_paragraphs)
        notations_by_tactic[primary_tactic]["notations"].append(
            {
                "name": notation_name,
                "patterns": all_patterns,
                "description": description,
                "description_md": ocamldoc_to_markdown(description),
            }
        )

    return notations_by_tactic


def extract_tactics(content: str) -> list[dict]:
    """Extract all tactics from the prelude.iml content."""
    tactics = []

    # Pattern to find builtin.tac attributes for functions (fn.tac.*)
    # We'll find each [@@builtin.tac "fn.tac.*"] and work backwards to get the definition
    tac_pattern = re.compile(r'\[@@builtin\.tac\s+"(fn\.tac\.[^"]+)"\]')

    lines = content.split("\n")

    for i, line in enumerate(lines):
        match = tac_pattern.search(line)
        if not match:
            continue

        tac_id = match.group(1)  # e.g., "fn.tac.skip"

        # Now we need to find the let binding that this attribute belongs to
        # The attribute can be on the same line as the binding, or on a subsequent line
        # We need to search backwards to find the `let` definition

        # First, check if the let is on the same line
        let_line_idx = i
        combined_lines = line

        # If the let is not on this line, search backwards
        if "let " not in line:
            for j in range(i - 1, max(i - 20, -1), -1):
                combined_lines = lines[j] + "\n" + combined_lines
                if "let " in lines[j]:
                    let_line_idx = j
                    break

        # Extract name and signature from the let binding
        # Pattern: let name : signature = ...
        # The signature can span multiple lines before the = sign
        let_match = re.search(r"let\s+(\w+)\s*:\s*(.+?)\s*=", combined_lines, re.DOTALL)
        if not let_match:
            # Some might not have explicit type annotation
            let_match = re.search(r"let\s+(\w+)\s*=", combined_lines)
            if let_match:
                name = let_match.group(1)
                signature = "(inferred)"
            else:
                continue
        else:
            name = let_match.group(1)
            # Clean up signature - remove newlines and extra spaces
            signature = re.sub(r"\s+", " ", let_match.group(2)).strip()

        # Now search backwards from the let line for doc comments
        doc_lines = []
        in_doc = False
        for j in range(let_line_idx - 1, max(let_line_idx - 50, -1), -1):
            stripped = lines[j].strip()

            # Skip empty lines between doc and let
            if not stripped and not in_doc:
                continue

            # Check if we're in or entering a doc comment
            if stripped.endswith("*)") and "(**" in stripped:
                # Single-line doc comment
                doc_match = re.search(r"\(\*\*\s*(.*?)\s*\*\)", stripped)
                if doc_match:
                    doc_lines.insert(0, doc_match.group(1))
                break
            elif stripped.endswith("*)"):
                in_doc = True
                # Get content before *)
                content_part = stripped[:-2].strip()
                if content_part:
                    doc_lines.insert(0, content_part)
            elif stripped.startswith("(**"):
                # Start of doc comment
                content_part = stripped[3:].strip()
                if content_part:
                    doc_lines.insert(0, content_part)
                break
            elif in_doc:
                doc_lines.insert(0, stripped)
            else:
                # No doc comment found, stop searching
                break

        documentation = "\n".join(doc_lines).strip() if doc_lines else ""

        tactics.append(
            {
                "name": name,
                "tac_id": tac_id,
                "signature": signature,
                "documentation": documentation,
                "documentation_md": ocamldoc_to_markdown(documentation),
            }
        )

    return tactics


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


def main(output_path: Path, prelude_path: Path | None = None, verbose: bool = False):
    if prelude_path is None:
        prelude_path = Path(__file__).parent / "prelude.iml"
    content = prelude_path.read_text()

    # Count expected tactics and extract them
    expected_count = count_tactic_attributes(content)
    tactics = extract_tactics(content)

    # Assert that we extracted all tactics
    assert len(tactics) == expected_count, (
        f"Expected {expected_count} tactics but extracted {len(tactics)}"
    )

    # Extract notations from tactic documentation
    notations = extract_notations(tactics)

    # Print tactics
    # print(f"Found {len(tactics)} tactics:\n")
    # print("-" * 80)

    if verbose:
        for i, tac in enumerate(tactics, 1):
            print(f"{i}. `{tac['name']}`")
            # print(f"Tactic ID: {tac['tac_id']}")
            print(f"- Signature: `{tac['signature']}`")
            if tac["documentation"]:
                print(f"- Doc: {tac['documentation_md']}")
            print()
            # print("-" * 80)

    # Print notations
    # total_notations = sum(len(n["notations"]) for n in notations.values())
    # print(f"\nFound {total_notations} notations across {len(notations)} tactics:\n")
    # print("-" * 80)

    # for tactic_name, data in notations.items():
    #     print(f"Tactic: {tactic_name}")
    #     for notation in data["notations"]:
    #         print(f"  [%{notation['name']}]: {', '.join(notation['patterns'])}")
    #     print("-" * 80)

    # Save as YAML
    output = {
        "tactics": tactics,
        "notations": notations,
    }
    output_path.write_text(yaml.dump(output, default_flow_style=False, sort_keys=False))
    # print(f"\nSaved to {output_path}")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description="Extract tactics documentation from prelude.iml"
    )
    parser.add_argument("prelude", nargs="?", type=Path, help="Path to prelude.iml")
    parser.add_argument("-o", "--output", type=Path, help="Output YAML path")
    parser.add_argument(
        "-v", "--verbose", action="store_true", help="Print each tactic to stdout"
    )
    args = parser.parse_args()

    output_path = args.output or Path(__file__).parent / "tactics.yaml"
    main(output_path, prelude_path=args.prelude, verbose=args.verbose)

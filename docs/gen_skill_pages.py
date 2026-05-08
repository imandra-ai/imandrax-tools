"""Mirror packages/codelogician-skill/skill into the docs site at build time.

Avoids physically copying files into ``docs/`` (so the source stays a single
source of truth) and avoids symlinks (which mkdocs' file walker does not
follow). Run automatically by the ``mkdocs-gen-files`` plugin.
"""

from pathlib import Path

import mkdocs_gen_files

# =====
# Paths
# =====

SKILL_SRC = Path("packages/codelogician-skill/skill")
SKILL_DST_PREFIX = Path("codelogician-skill/contents")

# Files we copy verbatim. Restrict to text/markdown-ish assets so binary blobs
# in subfolders don't bloat the build until we explicitly opt in.
TEXT_SUFFIXES = {".md", ".txt", ".iml", ".py", ".json", ".yaml", ".yml"}


# ----
# Copy
# ----

for src in SKILL_SRC.rglob("*"):
    if not src.is_file():
        continue
    if src.suffix.lower() not in TEXT_SUFFIXES:
        continue
    rel = src.relative_to(SKILL_SRC)
    dst = SKILL_DST_PREFIX / rel
    with mkdocs_gen_files.open(dst, "w") as fh:
        fh.write(src.read_text(encoding="utf-8"))
    # mkdocs only treats markdown as a doc page; setting the edit path on a
    # non-markdown file emits a spurious "unused" warning at build time.
    if src.suffix.lower() == ".md":
        mkdocs_gen_files.set_edit_path(dst, src)

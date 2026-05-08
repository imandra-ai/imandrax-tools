"""Mirror packages/codelogician-skill/skill into the docs site at build time.

Avoids physically copying files into ``docs/`` (so the source stays a single
source of truth) and avoids symlinks (which mkdocs' file walker does not
follow). Run automatically by the ``mkdocs-gen-files`` plugin.

Markdown files are mirrored verbatim. Non-markdown text assets (``.iml``,
``.py``, ``.json``, ``.yaml``, ``.txt``) are wrapped in a generated markdown
page that embeds the file contents as a fenced code block, so they appear
in the nav alongside the prose.

Also emits a ``SUMMARY.md`` consumed by ``mkdocs-literate-nav`` so the
sidebar mirrors the skill's directory tree automatically.
"""

from pathlib import Path

import mkdocs_gen_files

# =====
# Paths
# =====

SKILL_SRC = Path("packages/codelogician-skill/skill")
SKILL_DST_PREFIX = Path("codelogician-skill/contents")

# Files we surface in the docs. Restrict to text/markdown-ish assets so binary
# blobs in subfolders don't bloat the build until we explicitly opt in.
SUFFIX_LANG = {
    ".iml": "ocaml",
    ".py": "python",
    ".json": "json",
    ".yaml": "yaml",
    ".yml": "yaml",
    ".txt": "",
}
TEXT_SUFFIXES = {".md", *SUFFIX_LANG.keys()}


# ================
# Copy + build nav
# ================

nav = mkdocs_gen_files.Nav()

for src in sorted(SKILL_SRC.rglob("*")):
    if not src.is_file():
        continue
    suffix = src.suffix.lower()
    if suffix not in TEXT_SUFFIXES:
        continue
    rel = src.relative_to(SKILL_SRC)

    if suffix == ".md":
        # ----------------
        # Mirror verbatim
        # ----------------
        dst = SKILL_DST_PREFIX / rel
        with mkdocs_gen_files.open(dst, "w") as fh:
            fh.write(src.read_text(encoding="utf-8"))
        mkdocs_gen_files.set_edit_path(dst, src)
        nav[tuple(rel.with_suffix("").parts)] = rel.as_posix()
    else:
        # -----------------------------------------
        # Wrap as a markdown page with a code block
        # -----------------------------------------
        lang = SUFFIX_LANG[suffix]
        wrapper_rel = rel.with_suffix(rel.suffix + ".md")  # foo.iml -> foo.iml.md
        wrapper_dst = SKILL_DST_PREFIX / wrapper_rel
        content = src.read_text(encoding="utf-8")
        body = f"# `{rel.name}`\n\n```{lang}\n{content}\n```\n"
        with mkdocs_gen_files.open(wrapper_dst, "w") as fh:
            fh.write(body)
        mkdocs_gen_files.set_edit_path(wrapper_dst, src)
        nav[tuple(wrapper_rel.with_suffix("").parts)] = wrapper_rel.as_posix()

with mkdocs_gen_files.open(SKILL_DST_PREFIX / "SUMMARY.md", "w") as fh:
    fh.writelines(nav.build_literate_nav())

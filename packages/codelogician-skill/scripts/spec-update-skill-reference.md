# update-skill-reference

Generate a tree view of the `skill/` directory and insert it into `skill/SKILL.md`.

## Input

All `.md` files under `skill/` recursively, excluding `SKILL.md`.

## Output

A tree block appended to (or updated in) `skill/SKILL.md`:

```tree {name: skill-dir-structure}
imandrax/
├── advanced/ # Advanced topics and tips
│   ├── proof-tips.md # Practical tips for writing proofs in IML.
│   └── ...
└── iml-syntax.md # IML syntax guide, ...
```

## Rules

- **Root label** is `imandrax/`, not the filesystem dir name.
- **Files** show original filename (with `.md`). Description comes from frontmatter `description:` field. Files without frontmatter or description show filename only.
- **Directories** get descriptions from a constant table (`dir-descriptions`) at the top of the script. Empty string means no description shown.
- Description separator is ` # `.
- Directories render before files. Both sorted alphabetically.
- Tree connectors: `├──`, `└──`, `│   ` (standard `tree` style).

## dir-descriptions validation

The keys in `dir-descriptions` must exactly match the set of actual subdirectories under `skill/`. On mismatch, the script must exit with an error listing which dirs are missing from the table and which are redundant.

## Code block insertion

- Look for the fenced block `` ```tree {name: skill-dir-structure} `` in `SKILL.md`.
- If found, replace the entire block (up to closing `` ``` ``).
- If not found, append to end of file.

## Portability

The script resolves `skill/` relative to its own filesystem location, so it works regardless of the caller's working directory.

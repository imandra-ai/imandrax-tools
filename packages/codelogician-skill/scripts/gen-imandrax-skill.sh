#!/usr/bin/env bash
# Generate the `imandrax` skill from the codelogician skill.
#
# Usage: scripts/gen-imandrax-skill.sh [src-skill-dir] [out-dir]
set -euo pipefail

SRC="${1:-./skill}"
OUT="${2:-../skills/imandrax}"

rm -rf "${OUT}"
mkdir -p "${OUT}"
cp -R "${SRC}"/. "${OUT}"/
chmod -R u+w "${OUT}"

# Replace frontmatter and prepend a note to SKILL.md
body="$(awk 'BEGIN{n=0} n>=2{print; next} /^---$/{n++}' "${SRC}/SKILL.md")"
cat > "${OUT}/SKILL.md" <<'EOF'
---
name: imandrax
description: Use IML (Imandra Modeling Language) / ImandraX to reason about software engineering (programs and specifications) - formal verification, theorem proving, counter-example generation, region decomposition / test-case generation, etc. Read me whenever you see mentioning of IML / Imandra / ImandraX.
---

> [!Note] This skill is adapted from the CodeLogician skill. Prefer the ImandraX client
> libraries ([client-libraries.md](./client-libraries.md)); most CodeLogician knowledge here
> carries over to them.
EOF
printf '%s\n' "${body}" >> "${OUT}/SKILL.md"

echo "Generated ${OUT}"

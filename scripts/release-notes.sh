#!/usr/bin/env bash
set -euo pipefail

# Print the GitHub release body for a package version to stdout.
#
# Extracts the "## [<version>] - <date>" section from a Keep a Changelog style
# CHANGELOG.md, trims surrounding blank lines, and optionally appends a
# "Full Changelog" compare link. Exits non-zero with a diagnostic if the
# CHANGELOG or the version's section is missing or empty, so a release never
# ships with empty or wrong notes.
#
# Usage:
#   release-notes.sh --changelog PATH --version X.Y.Z [--compare-url URL]

usage() {
  echo "Usage: $0 --changelog PATH --version X.Y.Z [--compare-url URL]" >&2
  exit 2
}

changelog=""
version=""
compare_url=""

while [ $# -gt 0 ]; do
  case "$1" in
    --changelog)   changelog="${2:-}"; shift 2 ;;
    --version)     version="${2:-}"; shift 2 ;;
    --compare-url) compare_url="${2:-}"; shift 2 ;;
    -h|--help)     usage ;;
    *) echo "Unknown argument: $1" >&2; usage ;;
  esac
done

[ -n "$changelog" ] && [ -n "$version" ] || usage

if [ ! -f "$changelog" ]; then
  echo "::error::CHANGELOG not found: $changelog" >&2
  exit 1
fi

# Extract the section body between "## [<version>]" and the next "## " heading.
section=$(awk -v ver="$version" '
  index($0, "## [" ver "]") == 1 { found=1; next }
  found && /^## / { exit }
  found { print }
  END { if (!found) exit 3 }
' "$changelog") || {
  echo "::error::No $changelog section for version $version (expected a '## [$version] - <date>' heading)" >&2
  exit 1
}

# Trim leading/trailing blank lines from the section.
section=$(printf '%s\n' "$section" | awk '
  { l[NR] = $0 }
  END {
    s = 1;  while (s <= NR && l[s] ~ /^[[:space:]]*$/) s++
    e = NR; while (e >= s  && l[e] ~ /^[[:space:]]*$/) e--
    for (i = s; i <= e; i++) print l[i]
  }')

if [ -z "$section" ]; then
  echo "::error::$changelog section for $version is empty" >&2
  exit 1
fi

printf '%s\n' "$section"
if [ -n "$compare_url" ]; then
  printf '\n**Full Changelog**: %s\n' "$compare_url"
fi

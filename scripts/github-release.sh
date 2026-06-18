#!/usr/bin/env bash
set -euo pipefail

# Manually create a GitHub release for a package, mirroring the CI workflows:
# the release body is the package's own CHANGELOG.md section for this version
# (via release-notes.sh), not GitHub's auto-generated notes, which mix every
# package in this monorepo's shared history.
#
# Usage: ./scripts/github-release.sh <working-directory>
# Example: ./scripts/github-release.sh packages/imandrax-codegen

if [ $# -ne 1 ]; then
    echo "Usage: $0 <working-directory>"
    echo "Example: $0 packages/imandrax-codegen"
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKING_DIR="$1"

if [ ! -d "$WORKING_DIR" ]; then
    echo "Error: Directory '$WORKING_DIR' does not exist"
    exit 1
fi

cd "$WORKING_DIR"

echo "Extracting version information..."
version_data=$(uv version --output-format=json)
version=$(echo "$version_data" | jq -r '.version')
package_name=$(echo "$version_data" | jq -r '.package_name')
tag="${package_name}==${version}"

echo "Package: $package_name"
echo "Version: $version"
echo "Tag: $tag"

if [ ! -d "dist" ] || [ -z "$(ls -A dist 2>/dev/null)" ]; then
    echo "Error: dist/ directory is empty or does not exist"
    echo "Run 'uv build' first to create distribution files"
    exit 1
fi

# Find the previous release tag for THIS package, for the "Full Changelog"
# compare link (same-package prefix, not whatever tag was pushed last).
prev_tag=$(
  { git tag --list "${package_name}==*"; echo "$tag"; } \
    | sort -V -u \
    | grep -B1 -x "$tag" \
    | head -n1
)
[ "$prev_tag" = "$tag" ] && prev_tag=""

compare_url=""
if [ -n "$prev_tag" ]; then
    repo_url=$(gh repo view --json url --jq .url)
    compare_url="${repo_url}/compare/${prev_tag}...${tag}"
fi

# Build the release body from the package CHANGELOG.md section for this version.
body=$("$SCRIPT_DIR/release-notes.sh" \
  --changelog CHANGELOG.md \
  --version "$version" \
  --compare-url "$compare_url")

echo ""
echo "Creating GitHub release..."
gh release create "$tag" \
  --title "$package_name $version" \
  --notes "$body" \
  dist/*

echo "✓ GitHub release created successfully!"

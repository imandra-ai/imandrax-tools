#!/usr/bin/env bash
set -euo pipefail

# Usage: ./scripts/github-release.sh <working-directory>
# Example: ./scripts/github-release.sh packages/imandrax-codegen

if [ $# -ne 1 ]; then
    echo "Usage: $0 <working-directory>"
    echo "Example: $0 packages/imandrax-codegen"
    exit 1
fi

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

echo ""
echo "Creating GitHub release..."
gh release create "$tag" \
  --title "$package_name $version" \
  --generate-notes \
  dist/*

echo "✓ GitHub release created successfully!"

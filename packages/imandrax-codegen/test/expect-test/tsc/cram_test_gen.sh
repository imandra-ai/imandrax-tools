#!/usr/bin/env bash
set -euo pipefail

# Script to generate cram test cases for TypeScript validation
# Uses tsc --noEmit to check that generated TypeScript code is valid

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEST_DATA_DIR="$SCRIPT_DIR/../../data/ts"

# Print setup helper
cat <<'EOF'
Setup: Define helper function
  $ run_tsc() { (
  >    cd $DUNE_SOURCEROOT/packages/imandrax-codegen/test/expect-test/tsc && \
  >    pnpm exec tsc "../../data/ts/$1" --noEmit --lib ES2020 --strict 2>&1 || true
  > ); }

EOF

# Find all TypeScript files and generate test cases
find "$TEST_DATA_DIR" -type f -name "*.ts" -not -path "*/node_modules/*" | sort | while read -r ts_file; do
    # Get relative path from TEST_DATA_DIR
    rel_path="${ts_file#$TEST_DATA_DIR/}"

    # Extract test name from relative path (remove .ts extension)
    test_name="${rel_path%.ts}"

    # Generate test case
    cat <<EOF
${test_name}
  \$ run_tsc ${rel_path}
  expected output

EOF
done

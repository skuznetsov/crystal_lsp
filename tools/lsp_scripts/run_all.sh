#!/bin/bash
# Run all LSP regression scripts

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "=== LSP Regression Tests ==="
echo ""

for script in "$SCRIPT_DIR"/*.txt; do
    name=$(basename "$script")
    echo "--- Running: $name ---"
    python3 "$REPO_ROOT/tools/lsp_probe.py" --script "$script" 2>&1
    echo ""
done

echo "=== Done ==="

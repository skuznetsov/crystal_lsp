#!/bin/bash

# Script to count LSP diagnostics on various Crystal files
# Usage: ./count_lsp_diagnostics.sh <file_path>

FILE="$1"
LSP_BIN="../bin/crystal_v2_lsp"

if [ ! -f "$FILE" ]; then
    echo "File not found: $FILE"
    exit 1
fi

echo "=== Testing: $FILE ==="
env CRYSTAL_CACHE_DIR=./.crystal-cache "$LSP_BIN" "$FILE" 2>&1 | head -200 | grep -i "diagnostic" | wc -l

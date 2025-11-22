#!/bin/bash
# Build Crystal V2 LSP server

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "Building Crystal V2 LSP server..."

# Create bin directory if it doesn't exist
mkdir -p bin

# Compile LSP server (debug build, skip OpenSSL/LibreSSL)
crystal build -s -p -t -d src/lsp_main.cr -o bin/crystal_v2_lsp -D without_openssl

echo "✓ LSP server built: bin/crystal_v2_lsp"
echo ""
echo "To test:"
echo "  ./bin/crystal_v2_lsp"

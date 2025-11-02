#!/bin/bash
# Build Crystal V2 LSP server

set -e

echo "Building Crystal V2 LSP server..."

# Create bin directory if it doesn't exist
mkdir -p bin

# Compile LSP server
crystal build src/lsp_main.cr -o bin/crystal_v2_lsp --release

echo "✓ LSP server built: bin/crystal_v2_lsp"
echo ""
echo "To test:"
echo "  ./bin/crystal_v2_lsp"

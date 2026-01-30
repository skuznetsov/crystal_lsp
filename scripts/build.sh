#!/bin/bash
# Build script for Crystal v2 compiler
# Usage: ./scripts/build.sh [debug|release]

set -e

MODE="${1:-debug}"
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN_DIR="$PROJECT_ROOT/bin"
BOOTSTRAP_FAST_FLAG=""

if [[ "${CRYSTAL_V2_BOOTSTRAP_FAST:-}" == "1" || "${CRYSTAL_V2_BOOTSTRAP_FAST:-}" == "true" ]]; then
  BOOTSTRAP_FAST_FLAG="-Dbootstrap_fast"
fi

mkdir -p "$BIN_DIR"

case "$MODE" in
  debug|d)
    echo "Building in DEBUG mode (fast compile)..."
    crystal build "$PROJECT_ROOT/src/crystal_v2.cr" \
      -o "$BIN_DIR/crystal_v2" \
      ${BOOTSTRAP_FAST_FLAG} \
      --no-debug \
      2>&1
    echo "Done: $BIN_DIR/crystal_v2"
    ;;

  release|r)
    echo "Building in RELEASE mode (optimized)..."
    OPT_LEVEL="${CRYSTAL_V2_OPT_LEVEL:-2}"
    echo "Using -O${OPT_LEVEL} (O3 unstable in deep yield inlining)"
    crystal build "$PROJECT_ROOT/src/crystal_v2.cr" \
      -o "$BIN_DIR/crystal_v2" \
      -O"${OPT_LEVEL}" \
      ${BOOTSTRAP_FAST_FLAG} \
      2>&1
    echo "Done: $BIN_DIR/crystal_v2"
    ;;

  *)
    echo "Usage: $0 [debug|release]"
    echo "  debug (d)   - Fast compile, no optimizations (default)"
    echo "  release (r) - Optimized build"
    exit 1
    ;;
esac

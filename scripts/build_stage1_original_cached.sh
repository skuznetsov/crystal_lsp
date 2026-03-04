#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <debug|release> [output_bin] [extra crystal args...]" >&2
  exit 2
fi

MODE="$1"
shift

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC="$ROOT_DIR/src/crystal_v2.cr"

case "$MODE" in
  debug)
    CACHE_DIR="${CRYSTAL_CACHE_DIR_DEBUG:-$ROOT_DIR/.cache/original-crystal-debug}"
    RELEASE_FLAG=()
    DEFAULT_OUT="/tmp/stage1_original_debug_cached"
    ;;
  release)
    CACHE_DIR="${CRYSTAL_CACHE_DIR_RELEASE:-$ROOT_DIR/.cache/original-crystal-release}"
    RELEASE_FLAG=(--release)
    DEFAULT_OUT="/tmp/stage1_original_release_cached"
    ;;
  *)
    echo "Unknown mode: $MODE (expected debug or release)" >&2
    exit 2
    ;;
esac

OUT_BIN="$DEFAULT_OUT"
if [[ $# -gt 0 ]] && [[ "$1" != -* ]]; then
  OUT_BIN="$1"
  shift
fi

mkdir -p "$CACHE_DIR"

echo "[stage1-original] mode=$MODE"
echo "[stage1-original] cache=$CACHE_DIR"
echo "[stage1-original] out=$OUT_BIN"

CRYSTAL_CACHE_DIR="$CACHE_DIR" crystal build "$SRC" "${RELEASE_FLAG[@]}" -o "$OUT_BIN" "$@"

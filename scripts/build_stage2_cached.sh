#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 2 ]]; then
  echo "Usage: $0 <stage1-compiler> <debug|release> [output_bin] [extra stage2 args...]" >&2
  exit 2
fi

STAGE1_COMPILER="$1"
MODE="$2"
shift 2

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC="$ROOT_DIR/src/crystal_v2.cr"

case "$MODE" in
  debug)
    CACHE_DIR="${CRYSTAL_CACHE_DIR_STAGE2_DEBUG:-$ROOT_DIR/.cache/stage2-debug}"
    RELEASE_FLAG=()
    DEFAULT_OUT="/tmp/stage2_debug_cached"
    ;;
  release)
    CACHE_DIR="${CRYSTAL_CACHE_DIR_STAGE2_RELEASE:-$ROOT_DIR/.cache/stage2-release}"
    RELEASE_FLAG=(--release)
    DEFAULT_OUT="/tmp/stage2_release_cached"
    ;;
  *)
    echo "Unknown mode: $MODE (expected debug or release)" >&2
    exit 2
    ;;
esac

if [[ ! -x "$STAGE1_COMPILER" ]]; then
  echo "Stage1 compiler is not executable: $STAGE1_COMPILER" >&2
  exit 2
fi

OUT_BIN="$DEFAULT_OUT"
if [[ $# -gt 0 ]] && [[ "$1" != -* ]]; then
  OUT_BIN="$1"
  shift
fi

PIPELINE_CACHE="${CRYSTAL_V2_PIPELINE_CACHE:-0}"
mkdir -p "$CACHE_DIR"

echo "[stage2] mode=$MODE"
echo "[stage2] stage1=$STAGE1_COMPILER"
echo "[stage2] cache=$CACHE_DIR"
echo "[stage2] pipeline_cache=$PIPELINE_CACHE"
echo "[stage2] out=$OUT_BIN"

CRYSTAL_CACHE_DIR="$CACHE_DIR" \
CRYSTAL_V2_PIPELINE_CACHE="$PIPELINE_CACHE" \
  "$STAGE1_COMPILER" "$SRC" "${RELEASE_FLAG[@]}" -o "$OUT_BIN" "$@"

#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
OUT_DIR="${TMPDIR:-/tmp}/stage1_oror_index_hash_repro"
SRC="$OUT_DIR/repro.cr"
BIN="$OUT_DIR/repro.bin"
BIN_FALLBACK="${SRC%.cr}"
RUN_OUT="$OUT_DIR/run.out"
RUN_ERR="$OUT_DIR/run.err"
BUILD_ERR="$OUT_DIR/build.err"

mkdir -p "$OUT_DIR"

cat > "$SRC" <<'CR'
h = {} of String => Array(Int32)
h["k"] ||= [] of Int32
h["k"] << 7
puts h["k"].size
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$OUT_DIR/build.out" 2>"$BUILD_ERR"
build_rc=$?
set -e

if [ "$build_rc" -ne 0 ]; then
  echo "reproduced (compile failed)"
  echo "build stderr: $BUILD_ERR"
  exit 0
fi

if [ ! -x "$BIN" ] && [ -x "$BIN_FALLBACK" ]; then
  BIN="$BIN_FALLBACK"
fi

if [ ! -x "$BIN" ]; then
  echo "reproduced (binary not produced)"
  exit 0
fi

set +e
"$BIN" >"$RUN_OUT" 2>"$RUN_ERR"
run_rc=$?
set -e

if [ "$run_rc" -ne 0 ]; then
  echo "reproduced (runtime failed)"
  echo "runtime stderr: $RUN_ERR"
  exit 0
fi

if [ "$(cat "$RUN_OUT")" = "1" ]; then
  echo "not reproduced"
  exit 1
fi

echo "reproduced (unexpected output)"
echo "runtime stdout: $RUN_OUT"
exit 0

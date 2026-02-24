#!/bin/bash
# Repro: stage1-generated code can crash on basic Object#to_s dispatch.
# Usage: regression_tests/stage1_object_to_s_crash_repro.sh <compiler>

set -euo pipefail

BIN="${1:-}"
if [ -z "$BIN" ] || [ ! -x "$BIN" ]; then
  echo "usage: $0 <compiler>"
  exit 2
fi

SRC="$(mktemp /tmp/repro_object_to_s.XXXXXX.cr)"
OUT="${SRC%.cr}.bin"
BUILD_LOG="${SRC%.cr}.build.log"
RUN_LOG="${SRC%.cr}.run.log"
trap 'rm -f "$SRC" "$OUT" "$OUT.o" "$OUT.ll" "$BUILD_LOG" "$RUN_LOG"' EXIT

cat > "$SRC" <<'CR'
obj = Object.new
puts obj.to_s
CR

set +e
"$BIN" "$SRC" -o "$OUT" >"$BUILD_LOG" 2>&1
build_status=$?
set -e

if [ $build_status -ne 0 ]; then
  echo "not reproduced (compile failed)"
  cat "$BUILD_LOG"
  exit 1
fi

set +e
scripts/run_safe.sh "$OUT" 5 512 >"$RUN_LOG" 2>&1
run_status=$?
set -e

if [ $run_status -eq 139 ] || rg -q "Segfault \(exit 139\)" "$RUN_LOG"; then
  echo "reproduced: Object#to_s runtime segfault in stage1-generated binary"
  exit 0
fi

echo "not reproduced"
cat "$RUN_LOG"
exit 1

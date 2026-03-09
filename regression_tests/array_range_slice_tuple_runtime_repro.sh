#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
TMP_DIR="$(mktemp -d "$ROOT_DIR/tmp/array-range-slice-tuple.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
LOG="$TMP_DIR/repro.run"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
lines = ["a", "b"]
slice = lines[0..1]
puts slice.size
puts slice[0]
puts slice[1]
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$LOG.compile.out" 2>"$LOG.compile.err"
compile_status=$?
if [[ $compile_status -eq 0 ]]; then
  "$ROOT_DIR/scripts/run_safe.sh" "$BIN" 5 256 >"$LOG"
  run_status=$?
else
  run_status=125
fi
set -e

stdout="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$LOG" 2>/dev/null | tr -d '\r')"

echo "compiler: $COMPILER"
echo "compile_status: $compile_status"
echo "run_status: $run_status"
echo "stdout:"
printf '%s\n' "$stdout"

if [[ $compile_status -eq 0 && $run_status -eq 0 && "$stdout" == $'2\na\nb' ]]; then
  echo "not reproduced"
  exit 1
fi

echo "reproduced: range slice tuple runtime path still mis-lowers tuple scratch storage"
exit 0

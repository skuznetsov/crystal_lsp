#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-./bin/crystal_v2}"
KEEP_TMP="${KEEP_TMP:-0}"

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/array_concat_string.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
COMPILE_OUT="$TMP_DIR/compile.out"
COMPILE_ERR="$TMP_DIR/compile.err"
RUN_OUT="$TMP_DIR/run.out"

cleanup() {
  if [[ "$KEEP_TMP" != "1" ]]; then
    rm -rf "$TMP_DIR"
  fi
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
parse_details = [] of String
parse_details << "user=1.3ms"
parse_details << "files=1"
parse_details << "loaded=1"

parts = ["Stage 1/6 parse", "1.3ms"]
parts.concat(parse_details)

puts parts.join(" ")
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$COMPILE_OUT" 2>"$COMPILE_ERR"
compile_status=$?
set -e

if [[ $compile_status -ne 0 ]]; then
  echo "compile failed"
  echo "compiler: $COMPILER"
  echo "status: $compile_status"
  echo "tmp_dir: $TMP_DIR"
  echo "--- stderr ---"
  cat "$COMPILE_ERR"
  echo "--- stdout ---"
  cat "$COMPILE_OUT"
  exit 2
fi

set +e
./scripts/run_safe.sh "$BIN" 5 256 >"$RUN_OUT"
run_status=$?
set -e

stdout_text="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$RUN_OUT" | tr -d '\r')"
expected='Stage 1/6 parse 1.3ms user=1.3ms files=1 loaded=1'

echo "compiler: $COMPILER"
echo "tmp_dir: $TMP_DIR"
echo "stdout:"
printf '%s\n' "$stdout_text"

if [[ "$stdout_text" == "$expected" ]]; then
  echo "not reproduced"
  exit 1
fi

if [[ $run_status -ne 0 ]] && grep -Fq "[CRASH]" "$RUN_OUT"; then
  echo "reproduced: Array(String)#concat corrupts appended string slots"
  exit 0
fi

echo "reproduced: Array(String)#concat produced unexpected output"
exit 0

#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
OUT_DIR="${TMPDIR:-/tmp}/stage1_while_short_circuit_alias_repro"
SRC="$OUT_DIR/repro.cr"
BIN="$OUT_DIR/repro.bin"
BIN_FALLBACK="${SRC%.cr}"
RUN_OUT="$OUT_DIR/run.out"
RUN_ERR="$OUT_DIR/run.err"
BUILD_ERR="$OUT_DIR/build.err"

mkdir -p "$OUT_DIR"

cat > "$SRC" <<'CR'
def f(h1 : Hash(String, String), h2 : Hash(String, String), name : String)
  resolved = h1[name]? || h2[name]? || name
  depth = 0
  while (next_resolved = h1[resolved]? || h2[resolved]?) && next_resolved != resolved && depth < 10
    resolved = next_resolved
    depth += 1
  end
  puts depth
end

h1 = {"a" => "b", "b" => "a"} of String => String
h2 = {} of String => String
f(h1, h2, "a")
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
scripts/run_safe.sh "$BIN" 4 512 >"$RUN_OUT" 2>"$RUN_ERR"
run_rc=$?
set -e

if [ "$run_rc" -ne 0 ]; then
  echo "reproduced (runtime timeout/crash/leak)"
  echo "runtime stdout: $RUN_OUT"
  echo "runtime stderr: $RUN_ERR"
  exit 0
fi

actual_stdout="$(awk '/^=== STDOUT ===/{capture=1;next}/^=== STDERR ===/{capture=0}capture' "$RUN_OUT" | sed '/^$/d' | tr -d '\r')"
if [ "$actual_stdout" = "10" ]; then
  echo "not reproduced"
  exit 1
fi

echo "reproduced (unexpected output)"
echo "runtime stdout: $RUN_OUT"
echo "runtime stderr: $RUN_ERR"
exit 0

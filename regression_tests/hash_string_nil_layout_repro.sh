#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_ROOT="${TMPDIR:-/tmp}/hash_string_nil_layout_repro"
SRC="$TMP_ROOT/repro.cr"
BIN="$TMP_ROOT/repro_bin"
COMPILE_LOG="$TMP_ROOT/compile.log"
RUN_LOG="$TMP_ROOT/run.log"

mkdir -p "$TMP_ROOT"

cat > "$SRC" <<'CR'
h = {} of String => Nil
h["a"] = nil
h["b"] = nil
h["c"] = nil

ok = true
ok = false unless h.size == 3
ok = false unless h.has_key?("a")
ok = false unless h.has_key?("b")
ok = false unless h.has_key?("c")

keys = h.keys.sort
ok = false unless keys == ["a", "b", "c"]

if ok
  puts "ok size=#{h.size} keys=#{keys.join(",")}"
else
  puts "bad size=#{h.size} keys=#{keys.inspect}"
  exit 9
end
CR

if ! "$COMPILER" "$SRC" -o "$BIN" >"$COMPILE_LOG" 2>&1; then
  echo "FAIL: compile error (compiler: $COMPILER)" >&2
  echo "log: $COMPILE_LOG" >&2
  tail -n 120 "$COMPILE_LOG" >&2
  exit 1
fi

set +e
scripts/run_safe.sh "$BIN" 8 1024 >"$RUN_LOG" 2>&1
run_status=$?
set -e

if [ $run_status -ne 0 ]; then
  echo "FAIL: runtime regression reproduced (exit $run_status)" >&2
  echo "run log: $RUN_LOG" >&2
  tail -n 120 "$RUN_LOG" >&2
  exit $run_status
fi

if ! grep -q "ok size=3 keys=a,b,c" "$RUN_LOG"; then
  echo "FAIL: unexpected output" >&2
  echo "run log: $RUN_LOG" >&2
  tail -n 120 "$RUN_LOG" >&2
  exit 1
fi

echo "PASS: hash(String, Nil) layout repro"

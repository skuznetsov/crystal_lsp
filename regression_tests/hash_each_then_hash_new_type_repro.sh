#!/usr/bin/env bash
# Regression guard for constructor return-type pollution after Hash#each.
#
# Lowering Hash#each can force-lower Hash constructor helper bodies before a
# later top-level Hash(String, Int32).new(0) call. The call-site type must stay
# Hash(String, Int32); if it degrades to Hash(String, Void), later []/[]= calls
# dispatch through the wrong specialization and can segfault in Object#to_s.
#
# Exit contract:
#   0 — fixed: Hash.new after Hash#each remains typed enough for []/[]=.
#   1 — reproduced: binary ran but did not print the fixed marker.
#   2 — invalid invocation (missing compiler arg).
#   >2 — unexpected compile/runtime failure.
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/hash_each_then_hash_new.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
RUN_LOG="$TMP_DIR/run.log"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
h = {} of String => Int32
h["one"] = 1
h.each do |k, v|
end

counts = Hash(String, Int32).new(0)
counts["apple"] = 3

if counts["apple"] == 3
  puts "hash_each_then_hash_new_type_ok"
else
  puts "hash_each_then_hash_new_type_FAIL"
end
CR

compile_cmd=()
if [[ "$(basename "$COMPILER")" == "crystal" ]]; then
  compile_cmd=("$COMPILER" build "$SRC" -o "$BIN")
else
  compile_cmd=("$COMPILER" "$SRC" -o "$BIN")
fi

set +e
"${compile_cmd[@]}" >"$TMP_DIR/compile.out" 2>&1
compile_status=$?
set -e

if [[ $compile_status -ne 0 ]]; then
  echo "unexpected: compile failed with status=$compile_status" >&2
  tail -20 "$TMP_DIR/compile.out" >&2
  exit 3
fi

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$BIN" 5 512 >"$RUN_LOG" 2>&1
run_status=$?
set -e

if grep -qF "hash_each_then_hash_new_type_ok" "$RUN_LOG"; then
  echo "fixed: Hash.new after Hash#each keeps the concrete Hash value type"
  cat "$RUN_LOG"
  exit 0
fi

if [[ $run_status -eq 0 ]]; then
  echo "reproduced: fixed marker missing despite exit 0" >&2
  cat "$RUN_LOG" >&2
  exit 1
fi

echo "unexpected: abnormal exit ($run_status)" >&2
echo "--- run log tail ---" >&2
tail -20 "$RUN_LOG" >&2
exit 4

#!/usr/bin/env bash
# Regression guard for inlined Hash#each block writes to caller locals.
#
# V2 inlines Hash#each_entry_with_index/Hash#each loops. A caller-local write
# inside the user block (for example `total += v`) must feed the enclosing loop
# PHI backedge; otherwise the block executes but the caller-local stays at its
# pre-loop value.
#
# Exit contract:
#   0 — fixed: Hash#each block writeback produced the expected sum.
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
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/hash_each_writeback.XXXXXX")"
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
h["two"] = 2

total = 0
h.each do |k, v|
  total += v
end

if total == 3
  puts "hash_each_block_writeback_ok"
else
  puts "hash_each_block_writeback_FAIL total=#{total}"
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

if grep -qF "hash_each_block_writeback_ok" "$RUN_LOG"; then
  echo "fixed: Hash#each block writeback reaches caller local"
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

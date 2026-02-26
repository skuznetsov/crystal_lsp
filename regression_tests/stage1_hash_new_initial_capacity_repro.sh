#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_ROOT="${TMPDIR:-/tmp}/stage1_hash_new_initial_capacity_repro"
SRC="$TMP_ROOT/repro.cr"
BIN="$TMP_ROOT/repro_bin"
COMPILE_LOG="$TMP_ROOT/compile.log"
RUN_LOG="$TMP_ROOT/run.log"

mkdir -p "$TMP_ROOT"

cat > "$SRC" <<'CR'
class FK
end

class FE
end

h = Hash(FK, FE).new(initial_capacity: 8)
puts h.size
CR

if ! "$COMPILER" "$SRC" -o "$BIN" >"$COMPILE_LOG" 2>&1; then
  echo "reproduced (compile failed)"
  echo "compiler=$COMPILER"
  tail -n 80 "$COMPILE_LOG"
  exit 1
fi

set +e
scripts/run_safe.sh "$BIN" 8 1024 >"$RUN_LOG" 2>&1
run_status=$?
set -e

if [ $run_status -ne 0 ]; then
  echo "reproduced (runtime failed)"
  echo "status=$run_status"
  tail -n 80 "$RUN_LOG"
  exit 1
fi

if grep -q "^0$" "$RUN_LOG"; then
  echo "not reproduced"
  exit 0
fi

echo "reproduced (unexpected output)"
tail -n 80 "$RUN_LOG"
exit 1

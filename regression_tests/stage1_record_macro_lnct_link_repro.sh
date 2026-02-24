#!/bin/bash
# Repro: record macro field getter can degenerate into unresolved `_lnct` symbol.
# Usage: regression_tests/stage1_record_macro_lnct_link_repro.sh <compiler>

set -euo pipefail

BIN="${1:-}"
if [ -z "$BIN" ] || [ ! -x "$BIN" ]; then
  echo "usage: $0 <compiler>"
  exit 2
fi

SRC="$(mktemp /tmp/repro_record_lnct.XXXXXX.cr)"
OUT="${SRC%.cr}.bin"
LOG="${SRC%.cr}.log"
trap 'rm -f "$SRC" "$OUT" "$OUT.o" "$OUT.ll" "$LOG"' EXIT

cat > "$SRC" <<'CR'
record F, lnct : Int32

def foo(format : F)
  format.lnct
end

puts foo(F.new(1))
CR

set +e
"$BIN" "$SRC" -o "$OUT" >"$LOG" 2>&1
status=$?
set -e

if [ $status -ne 0 ] && rg -q "_lnct" "$LOG"; then
  echo "reproduced: unresolved _lnct from record field access path"
  exit 0
fi

echo "not reproduced"
cat "$LOG"
exit 1

#!/bin/bash
# No-prelude guard that forces a non-inlined HIR::Yield and verifies MIR
# lower_yield reaches the explicit Proc carrier trace path.
#
# This is intentionally not a behavior test for heap Proc dispatch. Runtime
# yield block params are still raw function pointers, so the inferred yield
# target is expected to trace as ProcCarrier::RawFnptrCallback.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"

if [[ ! -x "$COMPILER" ]]; then
  echo "ERROR: compiler not found: $COMPILER" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/p1_no_prelude_yield_carrier_XXXXXX)"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/min_yield_hir_test.cr"
OUT="$TMP_DIR/min_yield_hir_test"
BIN="$TMP_DIR/min_yield_hir_test_bin"
HIR_LOG="$TMP_DIR/compile_hir.log"
TRACE_LOG="$TMP_DIR/compile_trace.log"

cat >"$SRC" <<'CR'
def reducer(&)
  yield
end

reducer do
end
CR

CRYSTAL_V2_DISABLE_INLINE_YIELD=1 \
  "$COMPILER" "$SRC" --no-prelude --emit hir --no-link -o "$OUT" >"$HIR_LOG" 2>&1

if ! grep -Eq '%[0-9]+ = yield' "$OUT.hir"; then
  echo "p1 no-prelude yield carrier regression: HIR::Yield was not emitted" >&2
  tail -20 "$HIR_LOG" >&2
  exit 1
fi

if ! grep -Eq 'func @reducer\(%[0-9]+: [0-9]+ \[block\]\)' "$OUT.hir"; then
  echo "p1 no-prelude yield carrier regression: HIR block parameter metadata missing" >&2
  grep -n 'func @reducer' "$OUT.hir" >&2 || true
  exit 1
fi

if ! grep -Eq 'func_pointer @__crystal_block_proc_' "$OUT.hir"; then
  echo "p1 no-prelude yield carrier regression: raw block FuncPointer was not emitted" >&2
  exit 1
fi

CRYSTAL_V2_DISABLE_INLINE_YIELD=1 CRYSTAL_V2_PROC_CARRIER_TRACE=1 \
  "$COMPILER" "$SRC" --no-prelude -o "$BIN" >"$TRACE_LOG" 2>&1

if ! grep -Eq '\[PROC_CARRIER\] yield func=reducer target=[0-9]+ carrier=RawFnptrCallback' "$TRACE_LOG"; then
  echo "p1 no-prelude yield carrier regression: lower_yield carrier trace missing" >&2
  tail -20 "$TRACE_LOG" >&2
  exit 1
fi

"$ROOT_DIR/scripts/run_safe.sh" "$BIN" 5 512 >/dev/null

echo "p1_no_prelude_yield_carrier_ok"

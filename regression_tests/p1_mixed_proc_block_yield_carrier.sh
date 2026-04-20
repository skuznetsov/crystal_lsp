#!/bin/bash
# No-prelude compile/trace guard for yield target inference when a non-block
# Proc param appears before the real &block callback. MIR must choose the
# [block] param, not the Proc-typed value param.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"

if [[ ! -x "$COMPILER" ]]; then
  echo "ERROR: compiler not found: $COMPILER" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/p1_mixed_proc_block_yield_XXXXXX)"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/mixed_proc_block_yield.cr"
OUT="$TMP_DIR/mixed_proc_block_yield"
BIN="$TMP_DIR/mixed_proc_block_yield_bin"
HIR_LOG="$TMP_DIR/compile_hir.log"
TRACE_LOG="$TMP_DIR/compile_trace.log"

cat >"$SRC" <<'CR'
def r(p : Int32 -> Nil, &)
  yield
end

r(->(x : Int32) { }, &->{})
CR

CRYSTAL_V2_DISABLE_INLINE_YIELD=1 \
  "$COMPILER" "$SRC" --no-prelude --emit hir --no-link -o "$OUT" >"$HIR_LOG" 2>&1

if ! grep -Eq '%[0-9]+ = yield' "$OUT.hir"; then
  echo "p1 mixed proc/block yield regression: HIR::Yield was not emitted" >&2
  tail -20 "$HIR_LOG" >&2
  exit 1
fi

sig_line="$(grep -E 'func @r[$]Proc_block\(%[0-9]+: [0-9]+, %[0-9]+: [0-9]+ \[block\]\)' "$OUT.hir" | head -1 || true)"
if [[ -z "$sig_line" ]]; then
  echo "p1 mixed proc/block yield regression: expected Proc param followed by [block] param" >&2
  grep -n 'func @r' "$OUT.hir" >&2 || true
  exit 1
fi

if [[ ! "$sig_line" =~ %([0-9]+):[[:space:]][0-9]+,[[:space:]]%([0-9]+):[[:space:]][0-9]+[[:space:]]\[block\] ]]; then
  echo "p1 mixed proc/block yield regression: could not extract param ids" >&2
  echo "$sig_line" >&2
  exit 1
fi
proc_param_id="${BASH_REMATCH[1]}"
block_param_id="${BASH_REMATCH[2]}"

if [[ "$proc_param_id" == "$block_param_id" ]]; then
  echo "p1 mixed proc/block yield regression: Proc and block param ids unexpectedly match" >&2
  echo "$sig_line" >&2
  exit 1
fi

if ! grep -Eq 'make_proc fn=%[0-9]+ env=%[0-9]+ : [0-9]+' "$OUT.hir"; then
  echo "p1 mixed proc/block yield regression: non-block Proc value was not materialized" >&2
  exit 1
fi

CRYSTAL_V2_DISABLE_INLINE_YIELD=1 CRYSTAL_V2_PROC_CARRIER_TRACE=1 \
  "$COMPILER" "$SRC" --no-prelude -o "$BIN" >"$TRACE_LOG" 2>&1

if ! grep -Eq '\[PROC_CARRIER\] yield func=r[$]Proc_block target='"$block_param_id"' carrier=RawFnptrCallback' "$TRACE_LOG"; then
  echo "p1 mixed proc/block yield regression: lower_yield did not select the [block] param" >&2
  grep '\[PROC_CARRIER\]' "$TRACE_LOG" >&2 || true
  exit 1
fi

if grep -Eq '\[PROC_CARRIER\] yield func=r[$]Proc_block target='"$proc_param_id"' carrier=RawFnptrCallback' "$TRACE_LOG"; then
  echo "p1 mixed proc/block yield regression: lower_yield selected the non-block Proc param" >&2
  grep '\[PROC_CARRIER\]' "$TRACE_LOG" >&2 || true
  exit 1
fi

echo "p1_mixed_proc_block_yield_carrier_ok"

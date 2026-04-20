#!/bin/bash
# No-prelude guard for the opt-in diagnostic that documents the current
# implicit &block.call carrier limitation. This is intentionally compile-only:
# it must not change codegen or make the diagnostic fatal.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"

if [[ ! -x "$COMPILER" ]]; then
  echo "ERROR: compiler not found: $COMPILER" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/p1_block_call_diagnostic_XXXXXX)"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/block_call_diagnostic.cr"
OUT="$TMP_DIR/block_call_diagnostic"
QUIET_LOG="$TMP_DIR/quiet.log"
TRACE_LOG="$TMP_DIR/trace.log"

cat >"$SRC" <<'CR'
def each(&block)
  block.call(1)
end

each { |x| x }
CR

"$COMPILER" "$SRC" --no-prelude --emit hir --no-link -o "$OUT.quiet" >"$QUIET_LOG" 2>&1

if grep -Eq '\[CLOSURE_ABI\]' "$QUIET_LOG"; then
  echo "p1 block-call diagnostic regression: diagnostic emitted without opt-in env" >&2
  cat "$QUIET_LOG" >&2
  exit 1
fi

CRYSTAL_V2_BLOCK_CALL_DIAGNOSTIC=1 \
  "$COMPILER" "$SRC" --no-prelude --emit hir --no-link -o "$OUT.trace" >"$TRACE_LOG" 2>&1

if ! grep -Eq '\[CLOSURE_ABI\] implicit &block with \.call in each: raw fnptr and heap Proc carriers are not unified; alias tracking is not implemented\.' "$TRACE_LOG"; then
  echo "p1 block-call diagnostic regression: opt-in diagnostic missing" >&2
  cat "$TRACE_LOG" >&2
  exit 1
fi

if ! grep -Eq 'make_proc fn=%[0-9]+ env=%[0-9]+ : [0-9]+' "$OUT.trace.hir"; then
  echo "p1 block-call diagnostic regression: expected heap Proc caller shape missing" >&2
  exit 1
fi

echo "p1_block_call_diagnostic_ok"

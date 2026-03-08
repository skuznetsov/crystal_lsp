#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_const_assign_hir_env.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
OUT="$TMP_DIR/repro.o"
STDOUT_LOG="$TMP_DIR/stdout.log"
STDERR_LOG="$TMP_DIR/stderr.log"
COMBINED_LOG="$TMP_DIR/combined.log"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
X = 1
CR

set +e
"$COMPILER" "$SRC" --no-prelude --no-link --verbose -o "$OUT" >"$STDOUT_LOG" 2>"$STDERR_LOG"
compile_status=$?
set -e

cat "$STDOUT_LOG" "$STDERR_LOG" >"$COMBINED_LOG"

echo "compiler: $COMPILER"
echo "status: $compile_status"

reqscan_line="$(grep -F '[REQSCAN_DONE]' "$COMBINED_LOG" | tail -n 1 || true)"
if [[ -n "$reqscan_line" ]]; then
  echo "reqscan: $reqscan_line"
fi

if [[ $compile_status -eq 0 ]]; then
  echo "not reproduced"
  exit 1
fi

if [[ $compile_status -eq 139 ]] &&
   grep -Fq '[2/6] Lowering to HIR...' "$COMBINED_LOG" &&
   ! grep -Fq '[3/6] Escape analysis...' "$COMBINED_LOG" &&
   ! grep -Fq '[4/6] Lowering to MIR...' "$COMBINED_LOG"; then
  echo "reproduced: stage2 dies on the old no-prelude const-assign HIR env-cache blocker"
  exit 0
fi

if [[ $compile_status -eq 139 ]] &&
   grep -Fq '[4/6] Lowering to MIR...' "$COMBINED_LOG"; then
  echo "not reproduced: old HIR env-cache blocker removed, later MIR crash remains"
  exit 1
fi

echo "reproduced: unexpected failure signature"
echo "--- stdout ---"
cat "$STDOUT_LOG"
echo "--- stderr ---"
cat "$STDERR_LOG"
exit 0

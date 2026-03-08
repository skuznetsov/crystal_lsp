#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_require_lowering.XXXXXX")"
SRC="$TMP_DIR/main.cr"
DEP="$TMP_DIR/dep.cr"
OUT="$TMP_DIR/repro.o"
STDOUT_LOG="$TMP_DIR/stdout.log"
STDERR_LOG="$TMP_DIR/stderr.log"
COMBINED_LOG="$TMP_DIR/combined.log"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$DEP" <<'CR'
X = 1
CR

cat >"$SRC" <<'CR'
require "./dep"
puts X
CR

set +e
"$COMPILER" "$SRC" --no-prelude --no-link --verbose -o "$OUT" >"$STDOUT_LOG" 2>"$STDERR_LOG"
compile_status=$?
set -e

cat "$STDOUT_LOG" "$STDERR_LOG" >"$COMBINED_LOG"

echo "compiler: $COMPILER"
echo "status: $compile_status"

main_reqscan="$(grep -F '[REQSCAN_DONE]' "$COMBINED_LOG" | grep -F '/main.cr' | tail -n 1 || true)"
if [[ -n "$main_reqscan" ]]; then
  echo "main_reqscan: $main_reqscan"
fi

fallback_line="$(grep -F 'Source require fallback entries=' "$COMBINED_LOG" | head -n 1 || true)"
if [[ -n "$fallback_line" ]]; then
  echo "fallback_line: $fallback_line"
fi

if [[ $compile_status -eq 0 ]] && grep -Fq 'main.cr reqs=1' "$COMBINED_LOG" && ! grep -Fq 'Source require fallback entries=' "$COMBINED_LOG"; then
  echo "not reproduced"
  exit 1
fi

if [[ $compile_status -eq 139 ]] &&
   grep -Fq 'main.cr reqs=1' "$COMBINED_LOG" &&
   ! grep -Fq 'Source require fallback entries=' "$COMBINED_LOG" &&
   grep -Fq '[2/6] Lowering to HIR...' "$COMBINED_LOG" &&
   grep -Fq '[hir-arena] idx=1 size=4 roots=3' "$COMBINED_LOG"; then
  echo "reproduced: later self-hosted require compile survives scan and crashes in HIR lowering"
  exit 0
fi

echo "reproduced: unexpected failure signature"
echo "--- stdout ---"
cat "$STDOUT_LOG"
echo "--- stderr ---"
cat "$STDERR_LOG"
exit 0

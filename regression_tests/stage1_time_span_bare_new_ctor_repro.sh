#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/stage1_time_span_bare_new_ctor.XXXXXX")"
SRC="$TMP_DIR/main.cr"
STDOUT_LOG="$TMP_DIR/stdout.log"
STDERR_LOG="$TMP_DIR/stderr.log"
HIR="$TMP_DIR/main.hir"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
span = Time::Span.new(days: 1, hours: 2, minutes: 3, seconds: 4, nanoseconds: 5)
puts span.nanoseconds
CR

set +e
"$COMPILER" "$SRC" --emit hir >"$STDOUT_LOG" 2>"$STDERR_LOG"
compile_status=$?
set -e

if [[ $compile_status -ne 0 ]]; then
  echo "reproduced (compile failed)"
  echo "compiler=$COMPILER"
  tail -n 80 "$STDERR_LOG"
  exit 1
fi

if [[ ! -f "$HIR" ]]; then
  echo "reproduced (missing HIR output)"
  exit 1
fi

FUNC_BODY="$(
  awk '
    /^func @Time::Span\.new\$Int32_Int32_Int32_Int32_Int32/ {capture=1}
    capture {
      if (seen && /^func @/) exit
      print
      seen=1
    }
  ' "$HIR"
)"

if [[ -z "$FUNC_BODY" ]]; then
  echo "reproduced (missing Time::Span arity5 body)"
  exit 1
fi

if grep -Fq 'call Time::Span.new$Int32_Int32_Int32_Int32_Int32' <<<"$FUNC_BODY"; then
  echo "reproduced (self-recursive arity5 constructor call)"
  exit 1
fi

if grep -Fq 'call Time::Span.new$Int64_Int32' <<<"$FUNC_BODY"; then
  echo "not reproduced"
  exit 0
fi

echo "reproduced (expected 2-arg allocator wrapper call not found)"
echo "--- func body ---"
echo "$FUNC_BODY"
exit 1

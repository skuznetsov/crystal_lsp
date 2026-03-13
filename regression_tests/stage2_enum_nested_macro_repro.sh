#!/usr/bin/env bash
set -euo pipefail

BIN="${1:-/tmp/stage2_release_cached}"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2-enum-nested-macro.XXXXXX")"
SRC="$WORKDIR/repro.cr"
LOG_FILE="$WORKDIR/repro.log"
OUT_BIN="$WORKDIR/repro.bin"

trap 'rm -rf "$WORKDIR"' EXIT

if [[ ! -x "$BIN" ]]; then
  echo "error: compiler binary not found/executable: $BIN" >&2
  exit 2
fi

cat >"$SRC" <<'EOF'
enum E
  {% for value in %w(A B) %}
    {% if true %}
      {{value.id}} = 1
    {% end %}
  {% end %}

  def self.value
    {% if true %}
      1
    {% end %}
  end
end
EOF

set +e
DEBUG_ENUM_PARSE_BODY=E \
PARSER_UNEXPECTED_TRACE=1 \
CRYSTAL_V2_STOP_AFTER_HIR=1 \
  "$BIN" --no-prelude "$SRC" -o "$OUT_BIN" >"$LOG_FILE" 2>&1
RC=$?
set -e

if grep -q '\[ENUM_PARSE_DROP\] enum=E entry=macro invalid=1 current=10:if' "$LOG_FILE" &&
   grep -q '\[ENUM_PARSE_BODY\] enum=E count=0 entries=' "$LOG_FILE"; then
  echo "reproduced: stage2 dropped the outer enum macro body and resumed on the inner if"
  exit 0
fi

if grep -q '\[ENUM_PARSE_APPEND\] enum=E entry=macro' "$LOG_FILE"; then
  echo "not reproduced (compiler kept the enum macro body intact)"
  exit 1
fi

if [[ "$RC" -ne 0 ]]; then
  echo "not reproduced (compiler exited $RC after the old enum nested-macro signature disappeared)"
  exit 1
fi

echo "not reproduced"
exit 1

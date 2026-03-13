#!/usr/bin/env bash
set -euo pipefail

BIN="${1:-/tmp/stage2_release_cached}"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2-enum-method-def-arena.XXXXXX")"
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
DEBUG_DEF_ARENA=E \
DEBUG_SET_FTYPE=E.value \
DEBUG_INFER_BODY_NAME=value \
CRYSTAL_V2_STOP_AFTER_HIR=1 \
  "$BIN" --no-prelude "$SRC" -o "$OUT_BIN" >"$LOG_FILE" 2>&1
RC=$?
set -e

if grep -q '\[DEF_ARENA\] base=E.value' "$LOG_FILE" &&
   grep -q 'fit=true' "$LOG_FILE" &&
   grep -q 'first=CrystalV2::Compiler::Frontend::Node:last=CrystalV2::Compiler::Frontend::Node' "$LOG_FILE" &&
   [[ "$RC" -ne 0 ]]; then
  echo "reproduced: stage2 accepted a corrupt enum method arena with generic body nodes"
  exit 0
fi

if grep -q '\[SET_FTYPE\] name=E.value type=Int32' "$LOG_FILE" &&
   [[ "$RC" -eq 0 ]]; then
  echo "not reproduced (compiler rejected the corrupt enum method arena and typed E.value)"
  exit 1
fi

if [[ "$RC" -eq 0 ]]; then
  echo "not reproduced"
  exit 1
fi

echo "not reproduced (compiler exited $RC after the old enum method arena signature disappeared)"
exit 1

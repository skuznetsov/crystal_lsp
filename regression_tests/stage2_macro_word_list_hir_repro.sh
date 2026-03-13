#!/usr/bin/env bash
set -euo pipefail

BIN="${1:-/tmp/stage2_release_cached}"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2-macro-word-list-hir.XXXXXX")"
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
lldb -b -o 'run' -k 'thread backtrace all' -k 'quit 1' \
  -- "$BIN" --no-prelude "$SRC" -o "$OUT_BIN" >"$LOG_FILE" 2>&1
RC=$?
set -e

if grep -q 'macro_word_list_from_source' "$LOG_FILE"; then
  echo "reproduced: stage2 still crashes in macro_word_list_from_source while expanding enum %w(...)"
  exit 0
fi

if grep -q 'collect_return_types' "$LOG_FILE" &&
   grep -q 'register_enum_methods' "$LOG_FILE"; then
  echo "not reproduced (compiler moved past macro_word_list_from_source into later enum method inference)"
  exit 1
fi

if [[ "$RC" -eq 0 ]]; then
  echo "not reproduced (compiler moved past the old macro_word_list_from_source crash)"
  exit 1
fi

echo "not reproduced (compiler exited $RC after the old macro_word_list_from_source signature disappeared)"
exit 1

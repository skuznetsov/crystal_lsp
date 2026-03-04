#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <stage2_compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_DIR="$(mktemp -d /tmp/stage2_macro_record_heredoc_index_oob.XXXXXX)"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
OUT="$TMP_DIR/out.txt"
ERR="$TMP_DIR/err.txt"

cat >"$SRC" <<'CR'
macro record(__name name, *properties, **kwargs)
  {% raise <<-TXT unless kwargs.empty?
    macro `record` does not accept named arguments
      Did you mean:

      record #{name.stringify}, #{properties.splat}
    TXT
  %}
end
CR

set +e
"$COMPILER" "$SRC" --no-prelude --no-link -o "$BIN" >"$OUT" 2>"$ERR"
status=$?
set -e

if [[ $status -eq 139 ]] || \
   grep -q "Index out of bounds" "$ERR" || grep -q "Index out of bounds" "$OUT" || \
   grep -q "ExprId out of bounds" "$ERR" || grep -q "ExprId out of bounds" "$OUT"; then
  echo "reproduced: stage2 heredoc macro parse fails (segfault/index/exprid signature)"
  echo "compiler: $COMPILER"
  echo "status: $status"
  echo "tmp_dir: $TMP_DIR"
  exit 0
fi

echo "not reproduced"
echo "compiler: $COMPILER"
echo "status: $status"
echo "tmp_dir: $TMP_DIR"
echo "--- stderr ---"
cat "$ERR"
echo "--- stdout ---"
cat "$OUT"
exit 1

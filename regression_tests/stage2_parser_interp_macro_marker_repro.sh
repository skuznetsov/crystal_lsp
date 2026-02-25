#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
if [[ ! -x "$COMPILER" ]]; then
  echo "error: compiler is not executable: $COMPILER" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/stage2_parser_interp_macro_marker.XXXXXX)"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/repro.cr"
LOG="$TMP_DIR/compile.log"

cat > "$SRC" <<'CR'
class ReproInterpMacroString
  def a
    if true
      STDERR.puts "[DBG] empty=#{"x".strip.empty?} has_percent=#{"x".includes?("{%") } size=#{"x".size}"
    end
  end

  def b
    42
  end
end
CR

if ! env DEBUG_METHOD_REGISTER_FILTER=ReproInterpMacroString \
  "$COMPILER" --no-codegen "$SRC" -o "$TMP_DIR/out.bin" >"$LOG" 2>&1; then
  echo "reproduced: compiler failed during parser/check path"
  tail -n 40 "$LOG"
  exit 1
fi

if rg -q "DEBUG_METHOD_REGISTER.*method=b" "$LOG"; then
  echo "not reproduced"
  exit 0
fi

echo "reproduced: method b was not registered (class body swallowed after def a)"
echo "[expected] DEBUG_METHOD_REGISTER ... method=b"
echo "[actual] missing"
tail -n 40 "$LOG"
exit 1

#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-"$ROOT_DIR/bin/crystal_v2"}"

TMPDIR="$(mktemp -d "${TMPDIR:-/tmp}/cv2_typeof_element_type_prefix.XXXXXX")"
cleanup() {
  rm -rf "$TMPDIR"
}
trap cleanup EXIT

SOURCE="$TMPDIR/repro.cr"
OUT="$TMPDIR/repro"
LOG="$TMPDIR/build.log"

cat > "$SOURCE" <<'CR'
module Enumerable
  def self.element_type(x)
  end
end

class Array(T)
end

def needs_elem(x : typeof(Enumerable.element_type(Array(Int32)))) : Nil
end
CR

if ! "$ROOT_DIR/scripts/run_safe.sh" "$COMPILER" 60 2048 "$SOURCE" --no-prelude -o "$OUT" >"$LOG" 2>&1; then
  cat "$LOG" >&2
  echo "p2 typeof element_type regression: compiler failed" >&2
  exit 1
fi

if grep -Eq 'Segmentation fault|EXC_BAD_ACCESS|resolve_element_type_expression|Array\$LString\$R\$Hunsafe_fetch' "$LOG"; then
  cat "$LOG" >&2
  echo "p2 typeof element_type regression: fixed prefix scan used generated-stage2-hostile Array/block path" >&2
  exit 1
fi

echo "p2_typeof_element_type_prefix_no_array_scan_no_prelude_ok"

#!/usr/bin/env bash
# Runtime no-prelude guard: Array(Class)#unsafe_fetch must return the class
# reference, not an Int32-typed abort stub derived from the argument suffix.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"

if [[ ! -x "$COMPILER" ]]; then
  echo "ERROR: compiler not found: $COMPILER" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/p2_array_class_ref_fetch_XXXXXX)"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/array_class_ref_fetch.cr"
OUT="$TMP_DIR/array_class_ref_fetch"
BUILD_LOG="$TMP_DIR/build.log"
RUN_LOG="$TMP_DIR/run.log"

cat >"$SRC" <<'CR'
class Box
  def initialize(@value : Int32)
  end

  def value : Int32
    @value
  end
end

items = [] of Box
items << Box.new(7)
box = items.unsafe_fetch(0)
puts box.value
CR

"$COMPILER" "$SRC" --no-prelude -o "$OUT" >"$BUILD_LOG" 2>&1

"$ROOT_DIR/scripts/run_safe.sh" "$OUT" 5 256 >"$RUN_LOG" 2>&1

if grep -q 'STUB CALLED: Array$LBox$R$Hunsafe_fetch$$Int32' "$RUN_LOG" "$BUILD_LOG"; then
  echo "p2 array class ref unsafe_fetch regression: emitted abort stub" >&2
  cat "$BUILD_LOG" >&2
  cat "$RUN_LOG" >&2
  exit 1
fi

if ! grep -q '^7$' "$RUN_LOG"; then
  echo "p2 array class ref unsafe_fetch regression: expected output 7" >&2
  cat "$BUILD_LOG" >&2
  cat "$RUN_LOG" >&2
  exit 1
fi

echo "p2_array_class_ref_unsafe_fetch_no_prelude_ok"

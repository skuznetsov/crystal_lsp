#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

BIN="$1"
if [[ ! -x "$BIN" ]]; then
  echo "Compiler is not executable: $BIN" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/stage1_macro_value_hash_identity.XXXXXX")"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/repro.cr"
OUT_BIN="$TMP_DIR/repro_bin"
COMPILE_LOG="$TMP_DIR/compile.log"
RUN_LOG="$TMP_DIR/run.log"

cat >"$SRC" <<CR
require "${ROOT_DIR}/src/compiler/semantic/macro_value"

h = {} of String => CrystalV2::Compiler::Semantic::MacroValue
puts "empty=#{h.size}"
h["x"] = CrystalV2::Compiler::Semantic::MacroBoolValue.new(true)
puts "filled=#{h.size}"
v = h["x"]
puts "lookup_is_bool=#{v.is_a?(CrystalV2::Compiler::Semantic::MacroBoolValue)}"
puts "lookup_class=#{v.class.name}"
h.each do |k, mv|
  puts "each=#{k}:#{mv.is_a?(CrystalV2::Compiler::Semantic::MacroBoolValue)}:#{mv.class.name}"
end
CR

set +e
"$BIN" "$SRC" -o "$OUT_BIN" >"$COMPILE_LOG" 2>&1
compile_rc=$?
set -e

if [[ $compile_rc -ne 0 ]]; then
  echo "inconclusive: compiler exited $compile_rc while building the MacroValue hash probe" >&2
  tail -n 40 "$COMPILE_LOG" >&2 || true
  exit 2
fi

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$OUT_BIN" 5 512 >"$RUN_LOG" 2>&1
run_rc=$?
set -e

if rg -Fq 'lookup_is_bool=true' "$RUN_LOG" &&
   rg -Fq 'lookup_class=CrystalV2::Compiler::Semantic::MacroBoolValue' "$RUN_LOG" &&
   rg -Fq 'each=x:true:CrystalV2::Compiler::Semantic::MacroBoolValue' "$RUN_LOG"; then
  echo "not reproduced (generated code preserved MacroValue subclass identity through Hash lookup/each)"
  exit 0
fi

if rg -Fq 'lookup_is_bool=true' "$RUN_LOG" &&
   rg -Fq 'lookup_class=CrystalV2::Compiler::Semantic::MacroValue | String' "$RUN_LOG" &&
   rg -Fq 'each=x:true:CrystalV2::Compiler::Semantic::MacroValue' "$RUN_LOG"; then
  echo "reproduced: generated code degraded Hash(String, MacroValue) lookup/each type identity"
  exit 1
fi

echo "inconclusive: runtime exited $run_rc without the expected MacroValue hash identity signature" >&2
cat "$RUN_LOG" >&2
exit 2

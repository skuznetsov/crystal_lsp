#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <llvm_ir_file> [opt_level]" >&2
  echo "  opt_level: O0|O1|O2|O3 (default: O3)" >&2
  exit 2
fi

IR_FILE="$1"
OPT_LEVEL="${2:-O3}"

if [ ! -f "$IR_FILE" ]; then
  echo "IR file not found: $IR_FILE" >&2
  exit 2
fi

if ! command -v opt >/dev/null 2>&1; then
  echo "opt is required but not found in PATH" >&2
  exit 2
fi

if ! command -v rg >/dev/null 2>&1; then
  echo "rg is required but not found in PATH" >&2
  exit 2
fi

OUT_DIR="${TMPDIR:-/tmp}/stage2_opt_tailrecurse_repro"
mkdir -p "$OUT_DIR"
OPT_LL="$OUT_DIR/opt_${OPT_LEVEL}.ll"

opt "-${OPT_LEVEL}" -S "$IR_FILE" -o "$OPT_LL"

analyze_function() {
  local label="$1"
  local define_pattern_a="$2"
  local define_pattern_b="${3:-}"
  local start end calls tails collapsed

  start="$(rg -n -F "$define_pattern_a" "$OPT_LL" | head -n1 | cut -d: -f1)"
  if [ -z "${start:-}" ] && [ -n "$define_pattern_b" ]; then
    start="$(rg -n -F "$define_pattern_b" "$OPT_LL" | head -n1 | cut -d: -f1)"
  fi
  if [ -z "${start:-}" ]; then
    echo "$label not_found"
    return 0
  fi

  end="$(awk -v s="$start" 'NR > s && /^define / { print NR - 1; exit }' "$OPT_LL")"
  if [ -z "${end:-}" ]; then
    end="$(wc -l < "$OPT_LL")"
  fi

  calls="$(sed -n "${start},${end}p" "$OPT_LL" | rg -c '\bcall\b' || true)"
  tails="$(sed -n "${start},${end}p" "$OPT_LL" | rg -c 'tailrecurse' || true)"
  [ -z "$calls" ] && calls=0
  [ -z "$tails" ] && tails=0

  collapsed=0
  if [ "$calls" -eq 0 ] && [ "$tails" -gt 0 ]; then
    collapsed=1
  fi

  echo "$label calls=$calls tails=$tails collapsed=$collapsed"
}

main_line="$(analyze_function "__crystal_main" "define void @__crystal_main(")"
user_line="$(analyze_function "main_user_code" "define void @\"Crystal\$Dmain_user_code\$\$Int32_Pointer\$LPointer\$LUInt8\$R\$R\"(" "define void @Crystal\$Dmain_user_code\$\$Int32_Pointer\$LPointer\$LUInt8\$R\$R(")"

main_collapsed="$(printf '%s\n' "$main_line" | awk -F'collapsed=' '{print $2}' | awk '{print $1}')"
user_collapsed="$(printf '%s\n' "$user_line" | awk -F'collapsed=' '{print $2}' | awk '{print $1}')"
[ -z "$main_collapsed" ] && main_collapsed=0
[ -z "$user_collapsed" ] && user_collapsed=0

echo "opt_level: $OPT_LEVEL"
echo "ir_file: $IR_FILE"
echo "opt_output: $OPT_LL"
echo "$main_line"
echo "$user_line"

if [ "$main_collapsed" -eq 1 ] && [ "$user_collapsed" -eq 1 ]; then
  echo "reproduced (opt tailrecurse collapse)"
  exit 0
fi

echo "not reproduced"
exit 1

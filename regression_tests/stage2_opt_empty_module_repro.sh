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

if ! command -v llvm-dis >/dev/null 2>&1; then
  echo "llvm-dis is required but not found in PATH" >&2
  exit 2
fi

if ! command -v llc >/dev/null 2>&1; then
  echo "llc is required but not found in PATH" >&2
  exit 2
fi

if ! command -v nm >/dev/null 2>&1; then
  echo "nm is required but not found in PATH" >&2
  exit 2
fi

OUT_DIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_opt_empty_module.XXXXXX")"
OPT_BC="$OUT_DIR/opt_${OPT_LEVEL}.bc"
OPT_LL="$OUT_DIR/opt_${OPT_LEVEL}.ll"
OPT_O="$OUT_DIR/opt_${OPT_LEVEL}.o"

has_pattern() {
  local file="$1"
  local pattern="$2"
  perl -0ne "exit((/${pattern}/s) ? 0 : 1)" "$file"
}

raw_has_main=0
raw_has_crystal_main=0
raw_has_main_user_code=0
if has_pattern "$IR_FILE" 'define\s+.*\@main\(i32 %argc, ptr %argv\)'; then
  raw_has_main=1
fi
if has_pattern "$IR_FILE" 'define\s+.*\@__crystal_main\(i32 %argc, ptr %argv\)'; then
  raw_has_crystal_main=1
fi
if has_pattern "$IR_FILE" 'Crystal\$Dmain_user_code\$\$Int32_Pointer\$LPointer\$LUInt8\$R\$R'; then
  raw_has_main_user_code=1
fi

opt "-${OPT_LEVEL}" "$IR_FILE" -o "$OPT_BC"
llvm-dis "$OPT_BC" -o "$OPT_LL"
llc -filetype=obj "$OPT_BC" -o "$OPT_O"

opt_bc_bytes="$(wc -c < "$OPT_BC" | tr -d ' ')"
opt_ll_bytes="$(wc -c < "$OPT_LL" | tr -d ' ')"
opt_define_count="$(grep -c '^define ' "$OPT_LL" || true)"
[ -z "$opt_define_count" ] && opt_define_count=0

opt_has_main=0
opt_has_crystal_main=0
opt_has_main_user_code=0
if has_pattern "$OPT_LL" 'define\s+.*\@main\(i32 %argc, ptr %argv\)'; then
  opt_has_main=1
fi
if has_pattern "$OPT_LL" 'define\s+.*\@__crystal_main\(i32 %argc, ptr %argv\)'; then
  opt_has_crystal_main=1
fi
if has_pattern "$OPT_LL" 'Crystal\$Dmain_user_code\$\$Int32_Pointer\$LPointer\$LUInt8\$R\$R'; then
  opt_has_main_user_code=1
fi

nm_symbols="$(nm -gU "$OPT_O" || true)"
nm_symbol_count="$(printf '%s\n' "$nm_symbols" | awk 'NF > 0 { c++ } END { print 0 + c }')"

echo "opt_level: $OPT_LEVEL"
echo "ir_file: $IR_FILE"
echo "out_dir: $OUT_DIR"
echo "raw_has_main: $raw_has_main"
echo "raw_has___crystal_main: $raw_has_crystal_main"
echo "raw_has_main_user_code: $raw_has_main_user_code"
echo "opt_bc_bytes: $opt_bc_bytes"
echo "opt_ll_bytes: $opt_ll_bytes"
echo "opt_define_count: $opt_define_count"
echo "opt_has_main: $opt_has_main"
echo "opt_has___crystal_main: $opt_has_crystal_main"
echo "opt_has_main_user_code: $opt_has_main_user_code"
echo "opt_object_symbol_count: $nm_symbol_count"

if [ "$raw_has_main" -eq 1 ] &&
   [ "$raw_has_crystal_main" -eq 1 ] &&
   [ "$raw_has_main_user_code" -eq 1 ] &&
   [ "$opt_has_main" -eq 0 ] &&
   [ "$opt_has_crystal_main" -eq 0 ] &&
   [ "$opt_has_main_user_code" -eq 0 ] &&
   [ "$nm_symbol_count" -eq 0 ]; then
  echo "reproduced (opt empties stage2 module after raw entrypoints exist)"
  exit 0
fi

echo "not reproduced"
exit 1

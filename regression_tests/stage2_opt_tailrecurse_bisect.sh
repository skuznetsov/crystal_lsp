#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <llvm_ir_file> [low_limit] [high_limit]" >&2
  echo "  Finds first -opt-bisect-limit that collapses __crystal_main/main_user_code" >&2
  exit 2
fi

IR_FILE="$1"
LOW_LIMIT="${2:-0}"
HIGH_LIMIT="${3:-200000}"
MAX_LIMIT=2000000

if [ ! -f "$IR_FILE" ]; then
  echo "IR file not found: $IR_FILE" >&2
  exit 2
fi

if ! [[ "$LOW_LIMIT" =~ ^[0-9]+$ ]] || ! [[ "$HIGH_LIMIT" =~ ^[0-9]+$ ]]; then
  echo "low_limit/high_limit must be non-negative integers" >&2
  exit 2
fi

if [ "$LOW_LIMIT" -ge "$HIGH_LIMIT" ]; then
  echo "low_limit must be < high_limit" >&2
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

OUT_DIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_opt_tailrecurse_bisect.XXXXXX")"

OPT_LL="$OUT_DIR/opt.ll"
BISECT_LOG="$OUT_DIR/bisect.log"

analyze_function_collapsed() {
  local ll_file="$1"
  local define_pattern_a="$2"
  local define_pattern_b="${3:-}"
  local start end calls tails

  start="$(rg -n -F "$define_pattern_a" "$ll_file" | head -n1 | cut -d: -f1)"
  if [ -z "${start:-}" ] && [ -n "$define_pattern_b" ]; then
    start="$(rg -n -F "$define_pattern_b" "$ll_file" | head -n1 | cut -d: -f1)"
  fi
  if [ -z "${start:-}" ]; then
    echo 0
    return 0
  fi

  end="$(awk -v s="$start" 'NR > s && /^define / { print NR - 1; exit }' "$ll_file")"
  if [ -z "${end:-}" ]; then
    end="$(wc -l < "$ll_file")"
  fi

  calls="$(sed -n "${start},${end}p" "$ll_file" | rg -c '\bcall\b' || true)"
  tails="$(sed -n "${start},${end}p" "$ll_file" | rg -c 'tailrecurse' || true)"
  [ -z "$calls" ] && calls=0
  [ -z "$tails" ] && tails=0

  if [ "$calls" -eq 0 ] && [ "$tails" -gt 0 ]; then
    echo 1
  else
    echo 0
  fi
}

collapsed_at_limit() {
  local lim="$1"
  opt -O3 -opt-bisect-limit="$lim" -S "$IR_FILE" -o "$OPT_LL" >/dev/null 2>&1 || return 2

  local main_collapsed user_collapsed
  main_collapsed="$(analyze_function_collapsed "$OPT_LL" "define void @__crystal_main(")"
  user_collapsed="$(analyze_function_collapsed "$OPT_LL" "define void @\"Crystal\$Dmain_user_code\$\$Int32_Pointer\$LPointer\$LUInt8\$R\$R\"(" "define void @Crystal\$Dmain_user_code\$\$Int32_Pointer\$LPointer\$LUInt8\$R\$R(")"

  if [ "$main_collapsed" -eq 1 ] && [ "$user_collapsed" -eq 1 ]; then
    return 0
  fi
  return 1
}

# Ensure upper bound reproduces.
while ! collapsed_at_limit "$HIGH_LIMIT"; do
  if [ "$HIGH_LIMIT" -ge "$MAX_LIMIT" ]; then
    echo "Could not reproduce collapse up to limit=$MAX_LIMIT" >&2
    exit 1
  fi
  HIGH_LIMIT=$((HIGH_LIMIT * 2))
  if [ "$HIGH_LIMIT" -gt "$MAX_LIMIT" ]; then
    HIGH_LIMIT="$MAX_LIMIT"
  fi
done

if collapsed_at_limit "$LOW_LIMIT"; then
  echo "Collapse already reproduced at low_limit=$LOW_LIMIT" >&2
  exit 1
fi

lo="$LOW_LIMIT"
hi="$HIGH_LIMIT"
while [ $((lo + 1)) -lt "$hi" ]; do
  mid=$(((lo + hi) / 2))
  if collapsed_at_limit "$mid"; then
    hi="$mid"
  else
    lo="$mid"
  fi
done

FIRST_BAD="$hi"
NEXT_PASS=$((FIRST_BAD + 1))

opt -O3 -opt-bisect-limit="$FIRST_BAD" -disable-output "$IR_FILE" > /dev/null 2>"$BISECT_LOG" || true
LAST_RUN_LINE="$(rg -m1 "running pass \\($FIRST_BAD\\)" "$BISECT_LOG" || true)"
FIRST_SKIP_LINE="$(rg -m1 "NOT running pass \\($NEXT_PASS\\)" "$BISECT_LOG" || true)"

echo "ir_file: $IR_FILE"
echo "out_dir: $OUT_DIR"
echo "first_bad_limit: $FIRST_BAD"
echo "last_good_limit: $lo"
echo "last_running_pass: ${LAST_RUN_LINE:-<not found>}"
echo "first_skipped_pass: ${FIRST_SKIP_LINE:-<not found>}"
echo "bisect_log: $BISECT_LOG"

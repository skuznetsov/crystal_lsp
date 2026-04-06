#!/usr/bin/env bash
# Build crystal_v2 from two git commits in detached worktrees, run the same
# carrier.cr with CRYSTAL_V2_MACRO_BODY_OUTPUT_STATS_DUMP=1, extract JSONL, and
# diff with macro_body_output_stats_tool.py (--span-key if paths differ).
#
# Usage (from repo root):
#   scripts/compare_macro_dump_worktrees.sh COMMIT_A COMMIT_B PATH_TO_CARRIER.cr
#
# Carriers:
#   scripts/macro_dump_heavy_carrier.cr — more macro volume than a one-liner
#   scripts/macro_dump_flag_carrier.cr — top-level {% if flag?(:darwin) %} + {% begin %}
#   /tmp/macro_oracle_carrier.cr — minimal (prelude only)
#
# If an older commit lacks DUMP telemetry, cherry-pick ffa32a87 onto each
# worktree after add (clean on bootstrap-lineage commits; conflicts on tiny main trees).
#
# Requires: crystal, python3, git worktree support, ~30s+ compile per commit.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
COMMIT_A="${1:?first commit (e.g. oracle)}"
COMMIT_B="${2:?second commit (e.g. HEAD)}"
CARRIER="${3:?path to .cr carrier file}"

WT_A="/tmp/macro_cmp_wt_a_$$"
WT_B="/tmp/macro_cmp_wt_b_$$"
BIN_A="/tmp/cv2_macro_cmp_a_$$"
BIN_B="/tmp/cv2_macro_cmp_b_$$"
ERR_A="/tmp/macro_stderr_cmp_a_$$.txt"
ERR_B="/tmp/macro_stderr_cmp_b_$$.txt"
JSON_A="/tmp/macro_dump_cmp_a_$$.jsonl"
JSON_B="/tmp/macro_dump_cmp_b_$$.jsonl"

cleanup() {
  rm -rf "$WT_A" "$WT_B" "$BIN_A" "$BIN_B" "$ERR_A" "$ERR_B" "$JSON_A" "$JSON_B" 2>/dev/null || true
}
trap cleanup EXIT

cd "$REPO_ROOT"
git worktree add -f "$WT_A" "$COMMIT_A" >/dev/null
git worktree add -f "$WT_B" "$COMMIT_B" >/dev/null

echo "[1/4] crystal build $COMMIT_A -> $BIN_A"
( cd "$WT_A" && crystal build src/crystal_v2.cr -o "$BIN_A" --error-trace )

echo "[2/4] crystal build $COMMIT_B -> $BIN_B"
( cd "$WT_B" && crystal build src/crystal_v2.cr -o "$BIN_B" --error-trace )

OUT_A="/tmp/macro_cmp_out_a_$$"
OUT_B="/tmp/macro_cmp_out_b_$$"
echo "[3/4] DUMP compile carrier (both)"
CRYSTAL_V2_MACRO_BODY_OUTPUT_STATS_DUMP=1 "$BIN_A" "$CARRIER" -o "$OUT_A" 2> "$ERR_A"
CRYSTAL_V2_MACRO_BODY_OUTPUT_STATS_DUMP=1 "$BIN_B" "$CARRIER" -o "$OUT_B" 2> "$ERR_B"
rm -f "$OUT_A" "$OUT_B"

echo "[4/4] extract + diff --span-key (ignores macro_file path drift)"
python3 "$REPO_ROOT/scripts/macro_body_output_stats_tool.py" extract "$ERR_A" -o "$JSON_A"
python3 "$REPO_ROOT/scripts/macro_body_output_stats_tool.py" extract "$ERR_B" -o "$JSON_B"
echo "--- diff $COMMIT_A -> $COMMIT_B (left=A right=B) ---"
python3 "$REPO_ROOT/scripts/macro_body_output_stats_tool.py" diff "$JSON_A" "$JSON_B" --span-key --top 40

echo "--- summarize B ---"
python3 "$REPO_ROOT/scripts/macro_body_output_stats_tool.py" summarize "$JSON_B" --top 12

#!/bin/bash
# Source-level guard for the intentional hybrid closure-env ABI boundary.
#
# This guard is deliberately presence-based. It does not prove behavior; it
# prevents accidental partial cleanup of the still-coupled legacy anchors before
# the remaining closure-env ABI cleanup is landed as a coherent behavior change.
#
# Exit semantics:
#   exit 0 — hybrid boundary anchors are still present and documented.
#   exit 1 — an anchor disappeared or docs no longer mark the boundary.

set -u

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
HIR="$ROOT_DIR/src/compiler/hir/ast_to_hir.cr"
PLAN="$ROOT_DIR/docs/closure_env_abi_p1_plan.md"
TODO="$ROOT_DIR/TODO.md"
LOG="$ROOT_DIR/collab_logs/20260419-codex-closure-env-status.md"

fail() {
  echo "hybrid boundary regression: $*" >&2
  exit 1
}

require_file() {
  local path="$1"
  [[ -f "$path" ]] || fail "missing required file: $path"
}

require_pattern() {
  local label="$1"
  local pattern="$2"
  local path="$3"

  grep -Eq "$pattern" "$path" || fail "missing $label in ${path#$ROOT_DIR/}"
}

require_window_pattern() {
  local label="$1"
  local start_pattern="$2"
  local end_pattern="$3"
  local required_pattern="$4"
  local path="$5"

  awk -v start="$start_pattern" -v stop="$end_pattern" -v required="$required_pattern" '
    $0 ~ start { in_window = 1 }
    in_window && $0 ~ required { found = 1 }
    in_window && $0 ~ stop { exit }
    END { exit found ? 0 : 1 }
  ' "$path" || fail "missing $label in ${path#$ROOT_DIR/}"
}

require_file "$HIR"
require_file "$PLAN"
require_file "$TODO"
require_file "$LOG"

require_pattern "proc capture side-map declaration" '@proc_captures_by_value = \{\} of ValueId => Array\(ValueId\)' "$HIR"
require_pattern "closure cell side-map declaration" '@closure_ref_cells = \{\} of String => \{String, String, TypeRef\}' "$HIR"
require_pattern "closure-ref preference set usage" '@closure_ref_prefer_cell' "$HIR"
require_pattern "heap-proc selector" 'block_arg_requires_heap_proc\?' "$HIR"
require_pattern "dual-mode block-to-proc signature" 'private def lower_block_to_proc\(' "$HIR"
require_pattern "heap-proc branch" 'if heap_proc' "$HIR"
require_pattern "legacy raw callback comment" 'Runtime-yield block callbacks are still raw function pointers' "$HIR"
require_pattern "legacy closure-cell initialization" 'emit_capture_cell_init\(ctx, class_name, cell_name, cap_type, parent_vid\)' "$HIR"
require_pattern "legacy closure-cell non-local read fallback" 'if ref_cell = @closure_ref_cells\[name\]\?' "$HIR"
require_pattern "legacy closure-cell assignment write path" 'if ref_cell = @closure_ref_cells\[name\]\?' "$HIR"

require_window_pattern \
  "CallNode Proc#call hidden capture append" \
  'Proc#call intercept: lower receiver' \
  'proc_call = Call\.new' \
  '@proc_captures_by_value\[proc_recv_id\]\?' \
  "$HIR"

require_window_pattern \
  "CallNode Proc#call capture concat" \
  'Proc#call intercept: lower receiver' \
  'proc_call = Call\.new' \
  'proc_call_args[.]concat[(]capture_ids[)]' \
  "$HIR"

require_window_pattern \
  "MemberAccess Proc#call hidden capture append" \
  'Proc#call intercept for zero-arg calls' \
  'proc_call = Call\.new' \
  '@proc_captures_by_value\[proc_recv_id\]\?' \
  "$HIR"

require_window_pattern \
  "MemberAccess Proc#call capture concat" \
  'Proc#call intercept for zero-arg calls' \
  'proc_call = Call\.new' \
  'proc_call_args[.]concat[(]capture_ids[)]' \
  "$HIR"

require_window_pattern \
  "lower_block_to_proc heap path" \
  'private def lower_block_to_proc[(]' \
  'fp = FuncPointer\.new\(ctx\.next_id, proc_type, proc_func_name\)' \
  'return emit_make_proc_value[(]ctx, proc_type, proc_func_name, proc_func, captured_vars[)]' \
  "$HIR"

require_window_pattern \
  "lower_block_to_proc legacy raw pointer path" \
  'private def lower_block_to_proc[(]' \
  'fp\.id' \
  'FuncPointer[.]new[(]ctx[.]next_id, proc_type, proc_func_name[)]' \
  "$HIR"

require_pattern "plan records proc capture tracking removal is future work" '@proc_captures_by_value.*remove' "$PLAN"
require_pattern "plan records closure_ref_cells replacement is future work" '@closure_ref_cells.*replacement' "$PLAN"
require_pattern "plan records dual-mode block-to-proc checkpoint" 'dual-mode; heap path uses env/`MakeProc`, non-heap path still uses legacy closure cells' "$PLAN"
require_pattern "TODO records current hybrid boundary" 'not yet a universal heap-backed block callback ABI' "$TODO"
require_pattern "collab log records live legacy anchors" '@proc_captures_by_value`, `@closure_ref_cells`, and the dual-mode' "$LOG"

echo "p1_hybrid_boundary_ok"

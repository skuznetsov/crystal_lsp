#!/bin/bash
# Source-level guard for the intentional hybrid closure-env ABI boundary.
#
# This guard is deliberately presence-based. It does not prove behavior; it
# prevents accidental partial cleanup of the still-coupled closure-cell anchors
# before the remaining closure-env ABI cleanup is landed as a coherent behavior
# change. The old @proc_captures_by_value side map is intentionally absent: it
# had no producers after heap Proc materialization and was removed separately.
#
# Exit semantics:
#   exit 0 — hybrid boundary anchors are still present and documented.
#   exit 1 — an anchor disappeared or docs no longer mark the boundary.

set -u

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
HIR="$ROOT_DIR/src/compiler/hir/ast_to_hir.cr"
MIR="$ROOT_DIR/src/compiler/mir/hir_to_mir.cr"
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
require_file "$MIR"
require_file "$PLAN"
require_file "$TODO"
require_file "$LOG"

if grep -Eq '@proc_captures_by_value' "$HIR"; then
  fail "dead @proc_captures_by_value side map reappeared"
fi

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

require_window_pattern \
  "MIR Yield raw callback dispatch" \
  'private def lower_yield[(]' \
  'private def infer_yield_type_from_users' \
  'builder[.]call_indirect[(]block_val, args, convert_type[(]yield_type[)][)]' \
  "$MIR"

require_window_pattern \
  "MIR Yield raw callback mode marker" \
  'private def lower_yield[(]' \
  'private def infer_yield_type_from_users' \
  'MIR_YIELD_DISPATCH_MODE: raw_fnptr_only' \
  "$MIR"

require_pattern "MIR Proc carrier enum" 'private enum ProcCarrier' "$MIR"
require_pattern "MIR Proc carrier side map" '@hir_value_carriers : ::Hash\(HIR::ValueId, ProcCarrier\)' "$MIR"
require_pattern "MIR heap Proc carrier marker" 'ProcCarrier::HeapProcObject' "$MIR"
require_pattern "MIR raw callback carrier marker" 'ProcCarrier::RawFnptrCallback' "$MIR"
require_window_pattern \
  "MIR yield block param metadata inference" \
  'private def infer_block_param_id[(]' \
  'private def infer_yield_type_from_users' \
  'param[.]is_block' \
  "$MIR"
if awk '
  /private def infer_block_param_id[(]/ { in_window = 1 }
  in_window && /param[.]is_block/ { saw_is_block = 1 }
  in_window && /TypeKind::Proc/ && !saw_is_block { found = 1 }
  in_window && /private def infer_yield_type_from_users/ { exit }
  END { exit found ? 0 : 1 }
' "$MIR"; then
  fail "MIR infer_block_param_id checks TypeKind::Proc before HIR block metadata"
fi
require_window_pattern \
  "MIR Yield reads explicit Proc carrier marker" \
  'private def lower_yield[(]' \
  'private def infer_yield_type_from_users' \
  'yield_carrier = @hir_value_carriers' \
  "$MIR"

if awk '
  /private def lower_yield[(]/ { in_window = 1 }
  in_window && /call_heap_proc/ { found = 1 }
  in_window && /private def infer_yield_type_from_users/ { exit }
  END { exit found ? 0 : 1 }
' "$MIR"; then
  fail "MIR lower_yield reintroduced type-only heap Proc dispatch"
fi

require_pattern "plan records proc capture tracking is removed" 'Legacy Proc hidden captures.*removed as dead no-op' "$PLAN"
require_pattern "plan records closure_ref_cells replacement is future work" '@closure_ref_cells.*replacement' "$PLAN"
require_pattern "plan records dual-mode block-to-proc checkpoint" 'dual-mode; heap path uses env/`MakeProc`, non-heap path still uses legacy closure cells' "$PLAN"
require_pattern "plan records type-only Yield dispatch refutation" 'explicit carrier/provenance marker, not a type-only `Proc` heuristic' "$PLAN"
require_pattern "TODO records current hybrid boundary" 'not yet a universal heap-backed block callback ABI' "$TODO"
require_pattern "TODO records type-only Yield dispatch refutation" 'do not key MIR `Yield` heap dispatch on `TypeKind::Proc` alone' "$TODO"
require_pattern "collab log records live closure-cell anchors" '@closure_ref_cells` and the dual-mode' "$LOG"
require_pattern "collab log records type-only Yield dispatch refutation" 'TypeKind::Proc` is not a safe MIR `Yield` carrier discriminator' "$LOG"

echo "p1_hybrid_boundary_ok"

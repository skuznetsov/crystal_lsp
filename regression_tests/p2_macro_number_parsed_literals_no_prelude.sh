#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
compiler="$1"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/cv2_macro_number_literals.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT

if ! awk '
  /private def macro_value_for_expr/ { in_macro_value = 1 }
  in_macro_value && /when CrystalV2::Compiler::Frontend::NumberNode/ { in_number = 1 }
  in_number && /expr_node\.parsed_float/ { parsed_float = 1 }
  in_number && /MacroNumberValue\.from_literal\(literal\)/ { reparses = 1 }
  in_number && /when CrystalV2::Compiler::Frontend::CallNode/ {
    if (parsed_float && !reparses) exit 0
    exit 1
  }
  END { if (!parsed_float || reparses) exit 1 }
' "$ROOT_DIR/src/compiler/hir/ast_to_hir.cr"; then
  echo "macro number evaluation must use NumberNode parsed values instead of reparsing literal text" >&2
  exit 1
fi

if ! grep -Fq 'raw == Int64::MIN ? raw : -raw' "$ROOT_DIR/src/compiler/hir/ast_to_hir.cr"; then
  echo "macro unary minus folding must preserve the legal Int64::MIN boundary literal" >&2
  exit 1
fi

cat >"$tmpdir/repro.cr" <<'CR'
private EPS = 2.2204460492503131e-16
private MAX = 1.7976931348623157e+308
private MIN_I64 = -9223372036854775808
private COUNT = 42

COUNT
CR

log="$tmpdir/repro.log"
out="$tmpdir/repro"

"$ROOT_DIR/scripts/run_safe.sh" "$compiler" 30 1024 \
  "$tmpdir/repro.cr" --no-prelude --no-codegen -o "$out" \
  >"$log" 2>&1

if grep -Eq 'Segmentation fault|Bus error|EXC_BAD_ACCESS|\[CRASH\]|\[KILL\] Timeout' "$log"; then
  echo "macro number parsed-literal guard crashed or timed out" >&2
  tail -120 "$log" >&2 || true
  exit 1
fi

echo "p2_macro_number_parsed_literals_no_prelude_ok"

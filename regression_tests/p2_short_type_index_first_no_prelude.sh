#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
compiler="$1"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/cv2_short_type_index_first.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT

if ! grep -Fq 'private def safe_set_first?(set : Set(String)) : String?' "$ROOT_DIR/src/compiler/hir/ast_to_hir.cr"; then
  echo "short type index resolution must use a guarded Set first helper" >&2
  exit 1
fi

if awk '
  /private def fast_resolve_type_name_for_signature/ { in_func = 1 }
  in_func && /candidates\.first/ { bad = 1 }
  in_func && /^    private def / && !/fast_resolve_type_name_for_signature/ { exit bad ? 1 : 0 }
  END { if (!in_func) exit 1 }
' "$ROOT_DIR/src/compiler/hir/ast_to_hir.cr"; then
  :
else
  echo "fast_resolve_type_name_for_signature must not call Set#first directly" >&2
  exit 1
fi

cat >"$tmpdir/repro.cr" <<'CR'
private VALUE = 1
VALUE
CR

log="$tmpdir/repro.log"
out="$tmpdir/repro"

"$ROOT_DIR/scripts/run_safe.sh" "$compiler" 30 1024 \
  "$tmpdir/repro.cr" --no-prelude --emit llvm-ir --no-link -o "$out" \
  >"$log" 2>&1

if grep -Eq 'Segmentation fault|Bus error|EXC_BAD_ACCESS|\[CRASH\]|\[KILL\] Timeout' "$log"; then
  echo "short type index first guard crashed or timed out" >&2
  tail -120 "$log" >&2 || true
  exit 1
fi

echo "p2_short_type_index_first_no_prelude_ok"

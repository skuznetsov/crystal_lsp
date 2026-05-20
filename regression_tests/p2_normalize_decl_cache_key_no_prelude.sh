#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
compiler="$1"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/cv2_normalize_decl_cache.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT

if grep -Fq '@normalize_decl_cache : Hash({' "$ROOT_DIR/src/compiler/hir/ast_to_hir.cr"; then
  echo "normalize_declared_type_name cache key must not use Tuple keys in stage2" >&2
  exit 1
fi

if ! grep -Fq '@normalize_decl_cache : Hash(String, String)' "$ROOT_DIR/src/compiler/hir/ast_to_hir.cr"; then
  echo "normalize_declared_type_name cache must use string keys" >&2
  exit 1
fi

if ! grep -Fq 'cache_key = "#{type_name}\0#{cache_context}\0#{@subst_cache_gen}"' "$ROOT_DIR/src/compiler/hir/ast_to_hir.cr"; then
  echo "normalize_declared_type_name cache must use a string context/subst token" >&2
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
  echo "normalize-decl cache guard crashed or timed out" >&2
  tail -120 "$log" >&2 || true
  exit 1
fi

echo "p2_normalize_decl_cache_key_no_prelude_ok"

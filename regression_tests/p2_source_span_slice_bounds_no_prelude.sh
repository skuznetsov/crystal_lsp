#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
compiler="$1"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/cv2_source_span_bounds.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT

if ! awk '
  /private def slice_source_for_span/ { in_helper = 1 }
  in_helper && /finish > source_bytes/ { found = 1 }
  in_helper && /source_bytes > 100_000_000/ { bounded_source = 1 }
  in_helper && /^    end$/ { if (found && bounded_source) exit 0; exit 1 }
  END { if (!in_helper || !found || !bounded_source) exit 1 }
' "$ROOT_DIR/src/compiler/hir/ast_to_hir.cr"; then
  echo "slice_source_for_span must reject invalid spans and implausible source sizes" >&2
  exit 1
fi

cat >"$tmpdir/repro.cr" <<'CR'
private VALUE = 7
VALUE
CR

log="$tmpdir/repro.log"
out="$tmpdir/repro"

"$ROOT_DIR/scripts/run_safe.sh" "$compiler" 30 1024 \
  "$tmpdir/repro.cr" --no-prelude --emit llvm-ir --no-link -o "$out" \
  >"$log" 2>&1

if [[ ! -s "$out.ll" ]]; then
  echo "source span bounds guard did not emit LLVM IR" >&2
  tail -120 "$log" >&2 || true
  exit 1
fi

if grep -Eq 'Segmentation fault|Bus error|EXC_BAD_ACCESS|\[CRASH\]' "$log"; then
  echo "source span bounds guard crashed during LLVM emission" >&2
  tail -120 "$log" >&2 || true
  exit 1
fi

grep -Eq '@Object__classvar__VALUE[[:space:]]*=[[:space:]]*global i32' "$out.ll"

echo "p2_source_span_slice_bounds_no_prelude_ok"

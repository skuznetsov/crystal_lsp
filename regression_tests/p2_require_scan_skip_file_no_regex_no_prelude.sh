#!/usr/bin/env bash
set -euo pipefail

compiler="${1:-bin/crystal_v2}"
root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmpdir="$(mktemp -d /tmp/cv2_require_scan_skip_file.XXXXXX)"
trap 'rm -rf "$tmpdir"' EXIT

cat > "$tmpdir/repro.cr" <<CR
require "$root_dir/src/stdlib/io/encoding.cr"
CR

log="$tmpdir/repro.log"
set +e
CRYSTAL_V2_STOP_AFTER_PARSE=1 \
CRYSTAL_V2_PIPELINE_CACHE=0 \
CRYSTAL_V2_LLVM_CACHE=0 \
  "$root_dir/scripts/run_safe.sh" "$compiler" 60 2048 \
    "$tmpdir/repro.cr" --no-prelude -o "$tmpdir/repro" \
    >"$log" 2>&1
status=$?
set -e

if [[ $status -ne 0 ]]; then
  echo "p2 require scan skip_file no-regex: compiler failed status=$status" >&2
  tail -120 "$log" >&2 || true
  exit 1
fi

if grep -Eq 'Segmentation fault|EXC_BAD_ACCESS|String\$Hsub|each_macro_literal_raw_text_window' "$log"; then
  echo "p2 require scan skip_file no-regex: old macro raw-text scanner crash signature regressed" >&2
  tail -120 "$log" >&2 || true
  exit 1
fi

echo "p2_require_scan_skip_file_no_regex_no_prelude_ok"

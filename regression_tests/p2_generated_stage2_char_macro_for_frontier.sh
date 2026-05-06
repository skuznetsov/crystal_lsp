#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <generated-stage2-compiler>" >&2
  exit 2
fi

compiler="$1"
if [[ ! -x "$compiler" ]]; then
  echo "compiler is not executable: $compiler" >&2
  exit 2
fi

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp_dir="$(mktemp -d "${TMPDIR:-/tmp}/cv2_char_macro_for.XXXXXX")"
trap 'rm -rf "$tmp_dir"' EXIT

src="$tmp_dir/hello.cr"
out_bin="$tmp_dir/hello_bin"
log="$tmp_dir/trace.log"
printf 'puts 42\n' > "$src"

set +e
(
  cd "$repo_root"
  CRYSTAL_V2_TRACE_CLASS_FRONTIER=1 \
    scripts/run_safe.sh "$compiler" 60 4096 "$src" -o "$out_bin"
) >"$log" 2>&1
status=$?
set -e

if grep -Fq "[KILL] Timeout" "$log"; then
  echo "generated stage2 timed out before the Char macro-for frontier moved" >&2
  cat "$log" >&2
  exit 1
fi

if ! grep -Fq "[CLASS_FRONTIER] concrete_after_body_scan Char" "$log"; then
  echo "generated stage2 did not finish Char class body scan" >&2
  cat "$log" >&2
  exit 1
fi

if ! grep -Fq "[CLASS_FRONTIER] class_with_name_enter Proc" "$log"; then
  echo "generated stage2 did not advance to the Proc frontier after Char" >&2
  cat "$log" >&2
  exit 1
fi

if grep -Fq "primitive_signature_nil" "$log"; then
  echo "generated stage2 fell back to parsing malformed Char primitive macro output" >&2
  cat "$log" >&2
  exit 1
fi

# The next known frontier currently exits non-zero around Proc registration.
# This guard is intentionally a moved-frontier check, not a full `puts 42` oracle.
if [[ "$status" -eq 124 || "$status" -eq 137 ]]; then
  echo "generated stage2 was killed instead of reaching the Proc frontier cleanly" >&2
  cat "$log" >&2
  exit 1
fi

echo "p2_generated_stage2_char_macro_for_frontier_ok"

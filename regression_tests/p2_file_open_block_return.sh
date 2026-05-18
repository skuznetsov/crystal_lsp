#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
compiler="$1"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/cv2_file_open_return.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT

cat >"$tmpdir/repro.cr" <<'CR'
path = "/tmp/cv2_file_open_block_return_guard.txt"
value = File.open(path, "w") do |file|
  file.puts "x"
  "block-result"
end
puts value
CR

compile_log="$tmpdir/compile.log"
run_log="$tmpdir/run.log"
out="$tmpdir/repro"

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$compiler" 120 4096 \
  "$tmpdir/repro.cr" -o "$out" \
  >"$compile_log" 2>&1
compile_rc=$?
set -e

if [[ $compile_rc -ne 0 ]]; then
  echo "File.open block return guard compile failed" >&2
  tail -n 160 "$compile_log" >&2 || true
  exit 1
fi

"$ROOT_DIR/scripts/run_safe.sh" "$out" 5 512 >"$run_log" 2>&1

if ! grep -q '^block-result$' "$run_log"; then
  echo "File.open block return guard produced wrong output" >&2
  cat "$run_log" >&2
  exit 1
fi

echo "p2_file_open_block_return_ok"

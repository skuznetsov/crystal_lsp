#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

compiler="$1"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
workdir="$(mktemp -d "${TMPDIR:-/tmp}/stage1_const_hash_enum_values.XXXXXX")"
trap 'rm -rf "$workdir"' EXIT

src="$workdir/repro.cr"
out_bin="$workdir/repro_bin"
run_log="$workdir/run.log"
stdout_only="$workdir/stdout.txt"

cat >"$src" <<'CR'
enum Kind
  A = 0
  B = 1
end

CONST_MAP = {
  "a" => Kind::A,
  "b" => Kind::B,
}

local_map = {
  "a" => Kind::A,
  "b" => Kind::B,
}

puts "const_a=#{CONST_MAP["a"].value}"
puts "const_b=#{CONST_MAP["b"].value}"
puts "const_eq=#{CONST_MAP["a"] == Kind::A}"
puts "local_a=#{local_map["a"].value}"
puts "local_b=#{local_map["b"].value}"
puts "local_eq=#{local_map["a"] == Kind::A}"
CR

if ! "$compiler" --release "$src" -o "$out_bin" >"$workdir/compile.out" 2>"$workdir/compile.err"; then
  echo "reproduced: compiler failed to build constant enum-value hash oracle" >&2
  echo "--- compiler stderr ---" >&2
  cat "$workdir/compile.err" >&2
  exit 1
fi

set +e
"$repo_root/scripts/run_safe.sh" "$out_bin" 10 1024 >"$run_log" 2>&1
run_status=$?
set -e

if [[ $run_status -ne 0 ]]; then
  echo "reproduced: constant enum-value hash oracle crashed or timed out" >&2
  cat "$run_log" >&2
  exit 1
fi

awk '/^=== STDOUT ===$/ { capture = 1; next } /^=== STDERR ===$/ { capture = 0 } capture { print }' "$run_log" >"$stdout_only"

cat >"$workdir/expected.txt" <<'EOF'
const_a=0
const_b=1
const_eq=true
local_a=0
local_b=1
local_eq=true
EOF

if cmp -s "$stdout_only" "$workdir/expected.txt"; then
  echo "not reproduced: constant enum-value hash literal is stable"
  exit 0
fi

echo "reproduced: constant enum-value hash literal miscompiled or returned wrong values" >&2
cat "$run_log" >&2
exit 1

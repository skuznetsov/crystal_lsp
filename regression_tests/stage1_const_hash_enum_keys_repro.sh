#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

compiler="$1"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
workdir="$(mktemp -d "${TMPDIR:-/tmp}/stage1_const_hash_enum_keys.XXXXXX")"
trap 'rm -rf "$workdir"' EXIT

src="$workdir/repro.cr"
out_bin="$workdir/repro_bin"
run_log="$workdir/run.log"
stdout_only="$workdir/stdout.txt"

cat >"$src" <<'CR'
enum Kind
  EqEq = 83
  Match = 87
  Star = 98
end

MAP = {
  Kind::EqEq => 7,
  Kind::Match => 9,
  Kind::Star => 20,
}

k = Kind::EqEq
hasher = Crystal::Hasher.new(0, 0)
enum_hash = k.hash(hasher).result
int_hash = k.value.hash(Crystal::Hasher.new(0, 0)).result

puts "eq=#{k == Kind::EqEq}"
puts "value=#{k.value}"
puts "enum_hash=#{enum_hash}"
puts "int_hash=#{int_hash}"
puts "const_has_eq=#{MAP.has_key?(Kind::EqEq)}"
puts "const_lookup_eq=#{MAP[Kind::EqEq]? || -1}"

local = {
  Kind::EqEq => 7,
  Kind::Match => 9,
  Kind::Star => 20,
}
puts "has_eq=#{local.has_key?(Kind::EqEq)}"
puts "lookup_eq=#{local[Kind::EqEq]? || -1}"
CR

if ! "$compiler" --release "$src" -o "$out_bin" >"$workdir/compile.out" 2>"$workdir/compile.err"; then
  echo "reproduced: compiler failed to build constant enum-key hash oracle" >&2
  echo "--- compiler stderr ---" >&2
  cat "$workdir/compile.err" >&2
  exit 1
fi

set +e
"$repo_root/scripts/run_safe.sh" "$out_bin" 10 1024 >"$run_log" 2>&1
run_status=$?
set -e

if [[ $run_status -ne 0 ]]; then
  echo "reproduced: constant enum-key hash oracle crashed or timed out" >&2
  cat "$run_log" >&2
  exit 1
fi

awk '/^=== STDOUT ===$/ { capture = 1; next } /^=== STDERR ===$/ { capture = 0 } capture { print }' "$run_log" >"$stdout_only"

expect_line() {
  local needle="$1"
  if ! rg -qx --fixed-strings "$needle" "$stdout_only"; then
    echo "reproduced: missing expected line '$needle'" >&2
    cat "$run_log" >&2
    exit 1
  fi
}

expect_line "eq=true"
expect_line "value=83"
expect_line "const_has_eq=true"
expect_line "const_lookup_eq=7"
expect_line "has_eq=true"
expect_line "lookup_eq=7"

enum_hash="$(awk -F= '/^enum_hash=/{print $2}' "$stdout_only")"
int_hash="$(awk -F= '/^int_hash=/{print $2}' "$stdout_only")"

if [[ -z "$enum_hash" || -z "$int_hash" ]]; then
  echo "reproduced: missing enum_hash/int_hash output" >&2
  cat "$run_log" >&2
  exit 1
fi

if [[ "$enum_hash" != "$int_hash" ]]; then
  echo "reproduced: enum hash diverged from base integer hash" >&2
  cat "$run_log" >&2
  exit 1
fi

echo "not reproduced: constant enum-key hash literal and enum hashing are stable"

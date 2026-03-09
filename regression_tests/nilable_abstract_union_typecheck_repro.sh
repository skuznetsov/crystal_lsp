#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

compiler="$1"
tmp_dir="$(mktemp -d "${TMPDIR:-/tmp}/nilable-abstract-union-typecheck.XXXXXX")"
local_src="$tmp_dir/local.cr"
local_bin="$tmp_dir/local.bin"
local_run="$tmp_dir/local.run"
method_src="$tmp_dir/method.cr"
method_bin="$tmp_dir/method.bin"
method_run="$tmp_dir/method.run"

cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

cat >"$local_src" <<'CR'
abstract class Symb
end

class A < Symb
end

a = A.new
puts a.is_a?(A)
base : Symb = a
puts base.is_a?(A)
maybe : Symb? = a
puts maybe.is_a?(A)
CR

cat >"$method_src" <<'CR'
abstract class Symb
end

class A < Symb
end

def lookup : Symb?
  A.new
end

x = lookup
puts x.is_a?(A)
case x
when A
  puts "case-a"
else
  puts "case-miss"
end
puts x.class.name if x
CR

compile_and_run() {
  local src="$1"
  local bin="$2"
  local run_log="$3"

  "$compiler" "$src" -o "$bin" >"$run_log.compile.out" 2>"$run_log.compile.err"
  ./scripts/run_safe.sh "$bin" 5 256 >"$run_log"
}

compile_and_run "$local_src" "$local_bin" "$local_run"
compile_and_run "$method_src" "$method_bin" "$method_run"

local_stdout="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$local_run" | tr -d '\r')"
method_stdout="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$method_run" | tr -d '\r')"

echo "compiler: $compiler"
echo "local stdout:"
printf '%s\n' "$local_stdout"
echo "method stdout:"
printf '%s\n' "$method_stdout"

if grep -Fxq "true" <<<"$local_stdout" &&
   grep -Fxq "case-a" <<<"$method_stdout" &&
   grep -Fxq "A" <<<"$method_stdout" &&
   [[ "$(printf '%s\n' "$local_stdout" | tail -n1)" == "true" ]] &&
   [[ "$(printf '%s\n' "$method_stdout" | head -n1)" == "true" ]]; then
  echo "not reproduced"
  exit 1
fi

if [[ "$(printf '%s\n' "$local_stdout" | tail -n1)" == "false" ]] ||
   [[ "$(printf '%s\n' "$method_stdout" | head -n1)" == "false" ]] ||
   grep -Fxq "case-miss" <<<"$method_stdout"; then
  echo "reproduced: nilable abstract union typecheck lost concrete runtime type"
  exit 0
fi

echo "unexpected output"
echo "--- local run ---"
cat "$local_run"
echo "--- method run ---"
cat "$method_run"
exit 3

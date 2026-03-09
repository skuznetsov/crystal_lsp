#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

compiler="$1"
tmp_dir="$(mktemp -d "${TMPDIR:-/tmp}/bool-primitive-binary.XXXXXX")"
src="$tmp_dir/main.cr"
bin="$tmp_dir/main.bin"
run_log="$tmp_dir/run.log"

cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

cat >"$src" <<'CR'
puts true == true
puts true == false
puts false == false
puts true != false
puts false != false

value : Bool? = true
puts value == true ? "opt:true" : (value == false ? "opt:false" : "opt:nil")
CR

"$compiler" "$src" -o "$bin" >"$run_log.compile.out" 2>"$run_log.compile.err"
./scripts/run_safe.sh "$bin" 5 256 >"$run_log"

stdout="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$run_log" | tr -d '\r')"
expected=$'true\nfalse\ntrue\ntrue\nfalse\nopt:true'

echo "compiler: $compiler"
echo "stdout:"
printf '%s\n' "$stdout"

if [ "$stdout" = "$expected" ]; then
  echo "not reproduced"
  exit 1
fi

echo "reproduced: Bool primitive binary methods fell back to zero-return stubs"
exit 0

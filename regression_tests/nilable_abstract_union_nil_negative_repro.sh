#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

compiler="$1"
tmp_dir="$(mktemp -d "${TMPDIR:-/tmp}/nilable-abstract-union-nil-negative.XXXXXX")"
src="$tmp_dir/main.cr"
bin="$tmp_dir/main.bin"
run_log="$tmp_dir/run.log"

cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

cat >"$src" <<'CR'
lib LibC
  fun exit(status : Int32) : NoReturn
end

abstract class Symb
end

class A < Symb
end

def lookup(flag : Bool) : Symb?
  flag ? A.new : nil
end

x = lookup(false)
case x
when A
  LibC.exit(1)
else
  LibC.exit(0)
end
CR

set +e
"$compiler" "$src" -o "$bin" >"$run_log.compile.out" 2>"$run_log.compile.err"
compile_status=$?
set -e

if [ $compile_status -eq 139 ]; then
  echo "reproduced: compiler crashed on nil abstract-union negative case"
  exit 0
fi

if [ $compile_status -ne 0 ]; then
  echo "unexpected compile failure: $compile_status" >&2
  cat "$run_log.compile.err" >&2
  exit 3
fi

./scripts/run_safe.sh "$bin" 5 256 >"$run_log"
run_status=$?

echo "compiler: $compiler"
echo "run status: $run_status"

if [ $run_status -eq 0 ]; then
  echo "not reproduced"
  exit 1
fi

if grep -q "\[CRASH\]" "$run_log"; then
  echo "reproduced: nil abstract-union negative runtime path crashed"
  exit 0
fi

echo "unexpected runtime output"
cat "$run_log"
exit 3

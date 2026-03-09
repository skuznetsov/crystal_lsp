#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

compiler="$1"
tmp_dir="$(mktemp -d "${TMPDIR:-/tmp}/super-parent-dispatch.XXXXXX")"
src="$tmp_dir/repro.cr"
bin="$tmp_dir/repro.bin"
run_log="$tmp_dir/repro.run"

cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

cat >"$src" <<'CR'
abstract class Base
  def foo(x : Int32)
    puts x
  end
end

class Sub < Base
  def foo(x : Int32)
    super(x)
  end
end

Sub.new.foo(42)
CR

"$compiler" "$src" -o "$bin" >"$run_log.compile.out" 2>"$run_log.compile.err"
./scripts/run_safe.sh "$bin" 5 256 >"$run_log"

stdout="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$run_log" | tr -d '\r')"

echo "compiler: $compiler"
echo "stdout:"
printf '%s\n' "$stdout"

if [[ "$stdout" == "42" ]]; then
  echo "not reproduced"
  exit 1
fi

if [[ -z "$stdout" ]]; then
  echo "reproduced: ordinary parent super call lost target body"
  exit 0
fi

echo "unexpected output"
cat "$run_log"
exit 3

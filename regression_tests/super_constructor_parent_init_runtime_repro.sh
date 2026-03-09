#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

compiler="$1"
tmp_dir="$(mktemp -d "${TMPDIR:-/tmp}/super-parent-init.XXXXXX")"
src="$tmp_dir/repro.cr"
bin="$tmp_dir/repro.bin"
run_log="$tmp_dir/repro.run"

cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

cat >"$src" <<'CR'
struct ExprId
  getter index : Int32

  def initialize(@index : Int32)
  end
end

abstract class MySymbol
  getter name : String
  property node_id : ExprId

  def initialize(@name : String, @node_id : ExprId)
  end
end

class VariableSymbol < MySymbol
  getter declared_type : String?

  def initialize(name : String, node_id : ExprId, declared_type : String? = nil)
    super(name, node_id)
    @declared_type = declared_type
  end
end

h = {} of String => MySymbol
sym = VariableSymbol.new("x", ExprId.new(934), "Int32")
h["x"] = sym
v = h["x"]
puts typeof(v)
puts v.class.name
case v
when VariableSymbol
  puts v.node_id.index
else
  puts -1
end
CR

"$compiler" "$src" -o "$bin" >"$run_log.compile.out" 2>"$run_log.compile.err"
./scripts/run_safe.sh "$bin" 5 256 >"$run_log"

stdout="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$run_log" | tr -d '\r')"

echo "compiler: $compiler"
echo "stdout:"
printf '%s\n' "$stdout"

last_line="$(printf '%s\n' "$stdout" | tail -n1)"

if [[ "$last_line" == "934" ]]; then
  echo "not reproduced: parent initialize preserved node_id"
  exit 1
fi

if [[ "$last_line" == "0" ]]; then
  echo "reproduced: super initialize lost parent state"
  exit 0
fi

echo "unexpected output"
cat "$run_log"
exit 3

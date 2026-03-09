#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
TMP_DIR="$(mktemp -d "$ROOT_DIR/tmp/module-body-not-nil-exprid.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
COMPILE_STDOUT="$TMP_DIR/compile.stdout"
COMPILE_STDERR="$TMP_DIR/compile.stderr"
RUN_LOG="$TMP_DIR/run.log"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
require "../../src/compiler/frontend/ast"

alias FE = CrystalV2::Compiler::Frontend

body = [FE::ExprId.new(7)]
puts "body0"
puts body.size
puts body[0].index

m = FE::ModuleNode.new(FE::Span.zero, "Foo".to_slice, body, nil)
puts "m0"
puts m.body.nil?

mb = m.body.not_nil!
puts mb.object_id
puts mb.size
puts mb[0].index

if 1 == 2
  c = FE::ClassNode.new(FE::Span.zero, "Bar".to_slice, nil, body, nil, nil, nil, nil)
  puts c.body.nil?
end
CR

set +e
(
  cd "$ROOT_DIR"
  "$COMPILER" "$SRC" -o "$BIN" >"$COMPILE_STDOUT" 2>"$COMPILE_STDERR"
)
compile_status=$?
set -e

echo "compiler: $COMPILER"
echo "compile_status: $compile_status"

if [[ $compile_status -ne 0 ]]; then
  echo "reproduced: compile failed"
  echo "--- stdout ---"
  cat "$COMPILE_STDOUT"
  echo "--- stderr ---"
  cat "$COMPILE_STDERR"
  exit 0
fi

WRAPPER="$TMP_DIR/run.sh"
cat >"$WRAPPER" <<EOF
#!/usr/bin/env bash
set -euo pipefail
exec "$BIN"
EOF
chmod +x "$WRAPPER"

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$WRAPPER" 5 256 >"$RUN_LOG" 2>&1
run_status=$?
set -e

echo "run_status: $run_status"
cat "$RUN_LOG"

mapfile -t stdout_lines < <(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$RUN_LOG" | tr -d '\r')

if [[ $run_status -eq 0 && ${#stdout_lines[@]} -eq 8 &&
      "${stdout_lines[0]}" == "body0" &&
      "${stdout_lines[1]}" == "1" &&
      "${stdout_lines[2]}" == "7" &&
      "${stdout_lines[3]}" == "m0" &&
      "${stdout_lines[4]}" == "false" &&
      "${stdout_lines[5]}" =~ ^[0-9]+$ &&
      "${stdout_lines[6]}" == "1" &&
      "${stdout_lines[7]}" == "7" ]]; then
  echo "not reproduced"
  exit 1
fi

echo "reproduced: module body nilable array kept a raw provisional ExprId union"
exit 0

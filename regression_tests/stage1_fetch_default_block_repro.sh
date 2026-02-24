#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-/tmp/stage1_dbg_env_rootfix}"
TMP_DIR="$(mktemp -d /tmp/stage1_fetch_default.XXXXXX)"
SRC="$TMP_DIR/fetch_default_repro.cr"
BIN="$TMP_DIR/fetch_default_repro"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat > "$SRC" <<'CR'
module M
  def self.fetch(key : String) : String
    fetch(key) { raise "missing: #{key}" }
  end

  def self.fetch(key : String, default : T) : String | T forall T
    fetch(key) { default }
  end

  def self.fetch(key : String, &block : String -> T) : String | T forall T
    if false
      key
    else
      yield key
    end
  end
end

v1 = M.fetch("x", nil)
puts(v1.nil? ? "nil" : "not_nil")
puts M.fetch("x", "fallback")
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$TMP_DIR/compile.out" 2>"$TMP_DIR/compile.err"
compile_status=$?
set -e

if [[ $compile_status -ne 0 ]]; then
  echo "reproduced (compile failed)"
  echo "compiler status=$compile_status"
  head -n 20 "$TMP_DIR/compile.err"
  exit 1
fi

set +e
output="$("$BIN" 2>&1)"
run_status=$?
set -e

if [[ $run_status -ne 0 ]]; then
  echo "reproduced (runtime failed)"
  echo "runtime status=$run_status"
  echo "$output" | head -n 20
  exit 1
fi

if [[ "$output" == $'nil\nfallback' ]]; then
  echo "not reproduced"
  exit 0
fi

echo "reproduced (wrong output)"
echo "$output"
exit 1

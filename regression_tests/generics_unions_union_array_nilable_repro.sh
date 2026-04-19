#!/usr/bin/env bash
# Regression guard for the combined generics/unions frontier.
#
# Covers two V2 union ABI bugs:
#   1. Pointer(T)#[]= into Pointer(Int32 | String) must wrap stored values
#      before writing them into an Array(Int32 | String) buffer.
#   2. UnionWrap of an owned reference into an all-ref union such as Config?
#      must transfer ownership so block-end ARC cleanup does not free the
#      returned object.
#
# Exit contract:
#   0 — fixed: both union array dispatch and nilable reference return work.
#   1 — reproduced: binary ran but did not print the fixed marker.
#   2 — invalid invocation (missing compiler arg).
#   >2 — unexpected compile/runtime failure.
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/generics_unions_union_array_nilable.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
RUN_LOG="$TMP_DIR/run.log"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
alias IntOrString = Int32 | String

def describe_value(v : IntOrString) : String
  case v
  when Int32
    "integer #{v}"
  when String
    "string '#{v}'"
  else
    "unknown"
  end
end

mixed = [] of IntOrString
mixed << 1
mixed << "two"
mixed << 3
mixed << "four"

ok = mixed.map { |v| describe_value(v) }.join("|") ==
  "integer 1|string 'two'|integer 3|string 'four'"

class Config
  getter name : String
  getter debug : Bool

  def initialize(@name : String, @debug : Bool = false)
  end
end

def get_config(name : String) : Config?
  if name == "main"
    Config.new("main", true)
  else
    nil
  end
end

if cfg = get_config("main")
  ok &&= cfg.name == "main"
  ok &&= cfg.debug
else
  ok = false
end

ok &&= get_config("other").nil?

if ok
  puts "generics_unions_union_array_nilable_ok"
else
  puts "generics_unions_union_array_nilable_FAIL"
end
CR

compile_cmd=()
if [[ "$(basename "$COMPILER")" == "crystal" ]]; then
  compile_cmd=("$COMPILER" build "$SRC" -o "$BIN")
else
  compile_cmd=("$COMPILER" "$SRC" -o "$BIN")
fi

set +e
"${compile_cmd[@]}" >"$TMP_DIR/compile.out" 2>&1
compile_status=$?
set -e

if [[ $compile_status -ne 0 ]]; then
  echo "unexpected: compile failed with status=$compile_status" >&2
  tail -20 "$TMP_DIR/compile.out" >&2
  exit 3
fi

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$BIN" 5 512 >"$RUN_LOG" 2>&1
run_status=$?
set -e

if grep -qF "generics_unions_union_array_nilable_ok" "$RUN_LOG"; then
  echo "fixed: union arrays and nilable reference returns work"
  cat "$RUN_LOG"
  exit 0
fi

if [[ $run_status -eq 0 ]]; then
  echo "reproduced: fixed marker missing despite exit 0" >&2
  cat "$RUN_LOG" >&2
  exit 1
fi

echo "unexpected: abnormal exit ($run_status)" >&2
echo "--- run log tail ---" >&2
tail -20 "$RUN_LOG" >&2
exit 4

#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-/tmp/stage2_rel_current}"
TMP_DIR="$(mktemp -d /tmp/stage2_env_optional.XXXXXX)"
SRC="$TMP_DIR/env_optional.cr"
BIN="$TMP_DIR/env_optional"
OUT="$TMP_DIR/out.txt"
ERR="$TMP_DIR/err.txt"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat > "$SRC" <<'CR'
key = "CRYSTAL_V2_MISSING_KEY_REPRO_#{Process.pid}"
val = ENV[key]?
puts(val.nil? ? "nil" : val)
puts ENV.fetch(key, "fallback")
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$OUT" 2>"$ERR"
compile_status=$?
set -e

if [[ $compile_status -ne 0 ]]; then
  echo "reproduced (compile failed)"
  echo "compiler status=$compile_status"
  head -n 20 "$ERR"
  exit 1
fi

set +e
"$BIN" >"$OUT" 2>"$ERR"
run_status=$?
set -e

if [[ $run_status -ne 0 ]]; then
  echo "reproduced (runtime failed)"
  echo "runtime status=$run_status"
  head -n 20 "$ERR"
  exit 1
fi

if [[ "$(cat "$OUT")" == $'nil\nfallback' ]]; then
  echo "not reproduced"
  exit 0
fi

echo "reproduced (wrong output)"
echo "actual output:"
cat "$OUT"
exit 1

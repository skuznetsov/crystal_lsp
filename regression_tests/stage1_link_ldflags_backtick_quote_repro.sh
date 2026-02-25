#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-}"
if [[ -z "$COMPILER" || ! -x "$COMPILER" ]]; then
  echo "usage: $0 <compiler>"
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/stage1_link_ldflags.XXXXXX)"
SRC="$TMP_DIR/link_ssl.cr"
LOG="$TMP_DIR/build.log"
OUT="$TMP_DIR/link_ssl"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat > "$SRC" <<'CR'
require "openssl/lib_ssl"
puts 1
CR

set +e
"$COMPILER" build "$SRC" -o "$OUT" >"$LOG" 2>&1
status=$?
set -e

if [[ $status -eq 0 ]]; then
  echo "not reproduced"
  exit 0
fi

if rg -q "sh: --libs: command not found|library 'libssl' not found" "$LOG"; then
  echo "reproduced (broken Link(ldflags) parsing/execution)"
  head -n 20 "$LOG"
  exit 1
fi

echo "reproduced (compile failed for another reason)"
echo "compiler status=$status"
head -n 20 "$LOG"
exit 1

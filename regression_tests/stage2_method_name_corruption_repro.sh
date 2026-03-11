#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <stage2-compiler>" >&2
  exit 2
fi

BIN="$1"
TMP_ROOT="${TMPDIR:-/tmp}"
OUT_BIN="$TMP_ROOT/stage2_method_name_corruption_bin"
LOG_FILE="$TMP_ROOT/stage2_method_name_corruption.log"

set +e
env DEBUG_METHOD_REGISTER_FILTER=1 "$BIN" regression_tests/basic_sanity.cr -o "$OUT_BIN" >"$LOG_FILE" 2>&1
status=$?
set -e

if grep -a -q '{% end' "$LOG_FILE"; then
  echo "reproduced: self-hosted stage2 still registered corrupted method names"
  echo "compiler: $BIN"
  echo "log: $LOG_FILE"
  grep -a -n '{% end' "$LOG_FILE" | head -n 20
  exit 1
fi

echo "not reproduced: no corrupted '{% end' method names in registration log"
echo "compiler exit status: $status"

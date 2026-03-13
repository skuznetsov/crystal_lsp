#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

BIN="$1"

if [[ ! -x "$BIN" ]]; then
  echo "Compiler is not executable: $BIN" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_no_prelude_lib_min_repro.XXXXXX")"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/repro.cr"
OUT_BIN="$TMP_DIR/repro_bin"
LOG="$TMP_DIR/compile.log"

cat >"$SRC" <<'CR'
lib __MacroContext__
  alias Long = Int64
  alias ULong = UInt64
end
CR

set +e
env STAGE2_DEBUG=1 \
  "$BIN" --no-prelude --no-link "$SRC" -o "$OUT_BIN" >"$LOG" 2>&1
rc=$?
set -e

if [[ $rc -eq 0 ]]; then
  echo "not reproduced (compiler survived the tiny no-prelude lib compile)"
  exit 0
fi

if rg -Fq "[STAGE2_DEBUG] pass3 lowering setup" "$LOG"; then
  echo "reproduced: stage2 crashed after pass3 setup on the tiny no-prelude lib compile"
  exit 1
fi

if rg -Fq "[STAGE2_DEBUG] lib register idx=1/1 name=__MacroContext__" "$LOG"; then
  echo "reproduced: stage2 crashed after registering the tiny no-prelude lib"
  exit 1
fi

echo "inconclusive: compiler exited $rc before the minimal lib frontier markers" >&2
tail -n 40 "$LOG" >&2 || true
exit 2

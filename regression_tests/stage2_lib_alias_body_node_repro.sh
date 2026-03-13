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

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_lib_alias_body_node_repro.XXXXXX")"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/repro.cr"
OUT_BIN="$TMP_DIR/repro_bin"
LOG="$TMP_DIR/compile.log"

cat >"$SRC" <<'CR'
lib L
  alias A = Int32
end
CR

set +e
env DEBUG_LIB_MEMBER=L CRYSTAL_V2_STOP_AFTER_HIR=1 \
  "$BIN" --no-prelude "$SRC" -o "$OUT_BIN" >"$LOG" 2>&1
rc=$?
set -e

if rg -Fq "[LIB_MEMBER] lib=L pass=types idx=1/1 kind=AliasNode detail=A" "$LOG"; then
  echo "not reproduced (compiler kept the lib alias body typed)"
  exit 0
fi

if rg -Fq "[LIB_MEMBER] lib=L pass=types idx=1/1 kind=CrystalV2::Compiler::Frontend::Node detail=A" "$LOG"; then
  echo "reproduced: stage2 parsed the lib alias body into a generic Frontend::Node carrier"
  exit 1
fi

if [[ $rc -eq 0 ]]; then
  echo "inconclusive: compiler exited 0 without the expected lib-member marker" >&2
else
  echo "inconclusive: compiler exited $rc before the expected lib-member marker" >&2
fi
tail -n 40 "$LOG" >&2 || true
exit 2

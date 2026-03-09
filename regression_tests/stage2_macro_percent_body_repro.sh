#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
TMP_DIR="$(mktemp -d "$ROOT_DIR/tmp/macro-percent-body.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
OUT="$TMP_DIR/repro.out"
ERR="$TMP_DIR/repro.err"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
module Iterator(T)
  class Stop
  end

  module IteratorWrapper
    macro wrapped_next
      %value = @iterator.next
      return stop if %value.is_a?(Stop)
      %value
    end
  end
end
CR

set +e
"$COMPILER" "$SRC" --no-prelude --no-codegen >"$OUT" 2>"$ERR"
compile_rc=$?
set -e

echo "compiler: $COMPILER"
echo "compile_rc: $compile_rc"
echo "stderr:"
sed -n '1,20p' "$ERR"

if [[ $compile_rc -eq 0 ]]; then
  echo "not reproduced"
  exit 1
fi

if grep -q 'unexpected 78' "$ERR"; then
  echo "reproduced: macro body %value was rejected before parse_macro_body lowering"
  exit 0
fi

echo "unexpected failure mode"
exit 2

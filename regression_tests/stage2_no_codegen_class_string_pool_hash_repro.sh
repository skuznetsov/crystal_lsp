#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
TMP_DIR="$(mktemp -d "$ROOT_DIR/tmp/stage2-class-nocodegen-hash.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
STDOUT_FILE="$TMP_DIR/out.txt"
STDERR_FILE="$TMP_DIR/err.txt"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
class Outer
end
CR

set +e
"$COMPILER" build "$SRC" --no-codegen >"$STDOUT_FILE" 2>"$STDERR_FILE"
status=$?
set -e

if [[ $status -eq 133 || $status -eq 139 ]]; then
  echo "reproduced: compiler crashed on class/module no-codegen StringPool hash path"
  exit 0
fi

if [[ $status -eq 0 ]]; then
  echo "not reproduced"
  exit 1
fi

echo "--- stdout ---" >&2
cat "$STDOUT_FILE" >&2
echo "--- stderr ---" >&2
cat "$STDERR_FILE" >&2
printf 'unexpected status: %s\n' "$status" >&2
exit 3

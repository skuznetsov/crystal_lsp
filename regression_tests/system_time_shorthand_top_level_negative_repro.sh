#!/usr/bin/env bash
# Top-level `System::Time` must not use the forked `Crystal::System::Time` shorthand
# (that shorthand applies only under `Crystal::*`). Expect compile failure like reference Crystal.
#
# Usage: ./regression_tests/system_time_shorthand_top_level_negative_repro.sh ./bin/crystal_v2
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CC="${1:-"$ROOT_DIR/bin/crystal_v2"}"

if [[ ! -x "$CC" ]]; then
  echo "error: compiler not executable: $CC" >&2
  exit 2
fi

WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/system_time_shorthand_top_level_negative.XXXXXX")"
SRC="$WORKDIR/bad.cr"
OUT="$WORKDIR/out"

cleanup() {
  rm -rf "$WORKDIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
# Negative: at top level, `System::Time` is not the forked stdlib shorthand.
s, n = System::Time.instant
puts s
CR

set +e
# shellcheck disable=SC2090
env -u CRYSTAL_V2_STOP_AFTER_HIR "$CC" "$SRC" -o "$OUT" >/dev/null 2>"$WORKDIR/err.log"
status=$?
set -e

if [[ "$status" -eq 0 ]]; then
  echo "error: expected compile failure for top-level System::Time.instant, got success" >&2
  exit 1
fi

if ! grep -qiE 'System::Time|undefined|unresolved|error|failed' "$WORKDIR/err.log"; then
  echo "error: stderr did not mention failure details (status=$status)" >&2
  cat "$WORKDIR/err.log" >&2
  exit 1
fi

echo "ok: system_time_shorthand_top_level_negative_repro"

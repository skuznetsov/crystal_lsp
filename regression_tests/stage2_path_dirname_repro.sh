#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <compiler> [source]" >&2
  exit 2
fi

COMPILER=$1
SOURCE="${2:-${TMPDIR:-/tmp}/stage2_path_dirname_repro.cr}"
OUT_DIR="${TMPDIR:-/tmp}/stage2_path_dirname_repro"
mkdir -p "$OUT_DIR"

if [ ! -f "$SOURCE" ]; then
  cat > "$SOURCE" <<'CR'
puts Path["/tmp/alpha/beta/gamma.txt"].dirname
CR
fi

run_case() {
  local label="$1"
  shift
  local log="$OUT_DIR/$label.log"
  local status=0
  local start_ts=0
  local end_ts=0
  local elapsed=0

  echo
  echo "[$label]"
  echo "cmd: $*"

  start_ts=$(date +%s)
  set +e
  "$@" > "$log" 2>&1
  status=$?
  end_ts=$(date +%s)
  elapsed=$((end_ts - start_ts))
  set -e

  echo "status: $status (log: $log)"
  echo "elapsed: ${elapsed}s"
  if [ $status -eq 0 ]; then
    echo "  !! regression did not reproduce (expected crash)"
  else
    echo "  expected failure reproduced"
  fi
  tail -n 10 "$log"
}

run_case "default" "$COMPILER" --release "$SOURCE" -o "$OUT_DIR/path_dirname.bin"
run_case "no_ast_cache" "$COMPILER" --release --no-ast-cache "$SOURCE" -o "$OUT_DIR/path_dirname_no_ast_cache.bin"

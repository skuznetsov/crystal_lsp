#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <stage2-compiler> [source]" >&2
  echo "If source is omitted, a tiny 'puts 1' source is used."
  exit 2
fi

COMPILER=$1
SOURCE="${2:-/tmp/stage2_repro_small.cr}"
OUT_DIR="${TMPDIR:-/tmp}/stage2_bootstrap_minimal_repro"
mkdir -p "$OUT_DIR"

if [ ! -f "$SOURCE" ]; then
  cat > /tmp/stage2_repro_small.cr <<'CR'
puts 1
CR
  SOURCE="/tmp/stage2_repro_small.cr"
fi

unexpected_successes=0
run_case=0

run_case_fn() {
  local label="$1"
  local cmd_desc="$2"
  shift 2

  run_case=$((run_case + 1))
  local log="$OUT_DIR/$label.log"

  echo
  echo "[$run_case] $label"
  echo "    cmd: $cmd_desc"

  local status=0
  local start_ts=0
  local end_ts=0
  local elapsed=0
  start_ts=$(date +%s)
  set +e
  "$@" "$COMPILER" "$SOURCE" -o "$OUT_DIR/$label.bin" > "$log" 2>&1
  status=$?
  end_ts=$(date +%s)
  elapsed=$((end_ts - start_ts))
  set -e

  echo "    status: $status (log: $log)"
  echo "    elapsed: ${elapsed}s"
  if [ $status -eq 0 ]; then
    echo "    !! regression disappeared (expected crash did not occur)"
    unexpected_successes=$((unexpected_successes + 1))
  else
    echo "    expected failure reproduced"
  fi
  tail -n 12 "$log"
}

run_case_fn "default" "$COMPILER --release" "$COMPILER" --release
run_case_fn "no_ast_cache" "STAGE2_BOOTSTRAP_TRACE=1 $COMPILER --release --no-ast-cache" env STAGE2_BOOTSTRAP_TRACE=1 "$COMPILER" --release --no-ast-cache
run_case_fn "no_codegen" "STAGE2_BOOTSTRAP_TRACE=1 $COMPILER --release --no-ast-cache --no-codegen" env STAGE2_BOOTSTRAP_TRACE=1 "$COMPILER" --release --no-ast-cache --no-codegen
run_case_fn "parser_stub" "STAGE2_PARSER_STUB=1 STAGE2_BOOTSTRAP_TRACE=1 $COMPILER --release --no-ast-cache --no-codegen" env STAGE2_PARSER_STUB=1 STAGE2_BOOTSTRAP_TRACE=1 "$COMPILER" --release --no-ast-cache --no-codegen

if [ $unexpected_successes -gt 0 ]; then
  echo
  echo "FAILED: at least one case exited 0; this likely means parser crash regressed."
  exit 1
fi

echo
echo "OK: all cases still reproduce the regression as expected."

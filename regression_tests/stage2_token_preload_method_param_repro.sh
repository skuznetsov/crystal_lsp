#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler> [source]" >&2
  exit 2
fi

COMPILER="$1"
SOURCE="${2:-"$(dirname "$0")/stage2_token_preload_method_param_repro.cr"}"
ATTEMPTS=5
REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

for attempt in $(seq 1 "$ATTEMPTS"); do
  log_dir=$(mktemp -d "${TMPDIR:-/tmp}/stage2_token_preload_method_param_repro.XXXXXX")
  echo "[attempt $attempt/$ATTEMPTS] $COMPILER"

  set +e
  env \
    CRYSTAL_V2_STOP_AFTER_PARSE=1 \
    CRYSTAL_V2_PIPELINE_CACHE=0 \
    CRYSTAL_V2_LLVM_CACHE=0 \
    "$REPO_ROOT/scripts/timeout_sample_lldb.sh" \
    -t 30 -m 2048 -s 1 -l 5 -n 8 --no-series \
    -o "$log_dir" \
    -- "$COMPILER" "$SOURCE" --release --no-prelude -o "$log_dir/out"
  status=$?
  set -e

  case "$status" in
    0)
      ;;
    133|134|138|139)
      echo "reproduced: compiler crashed before STOP_AFTER_PARSE on the token-preload method-param repro"
      echo "log: $log_dir"
      exit 1
      ;;
    *)
      echo "inconclusive: unexpected status=$status"
      echo "log: $log_dir"
      exit 2
      ;;
  esac
done

echo "not reproduced: compiler reached STOP_AFTER_PARSE on all $ATTEMPTS token-preload method-param repro attempts"

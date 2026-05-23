#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BACKEND="$ROOT_DIR/src/compiler/mir/llvm_backend.cr"

if ! grep -q 'def current_func_param_index?' "$BACKEND"; then
  echo "p2 backend param index regression: helper missing" >&2
  exit 1
fi

if grep -q '@current_func_params\[i\]\?' "$BACKEND"; then
  echo "p2 backend param index regression: helper reintroduced nilable []? fetch" >&2
  exit 1
fi

if ! grep -q '@current_func_params\.unsafe_fetch(i)' "$BACKEND"; then
  echo "p2 backend param index regression: helper no longer uses size-guarded unsafe_fetch" >&2
  exit 1
fi

echo "p2_backend_param_index_no_nilable_fetch_ok"

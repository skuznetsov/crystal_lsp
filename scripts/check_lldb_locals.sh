#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
compiler="${1:-/tmp/crystal_v2_dwarf_check}"
work_dir="$(mktemp -d "${TMPDIR:-/tmp}/crystal_v2_lldb_locals.XXXXXX")"
trap 'rm -rf "$work_dir"' EXIT

require_contains() {
  local needle="$1"
  local file="$2"

  if ! rg -F -q "$needle" "$file"; then
    echo "missing expected output: $needle" >&2
    echo "--- $file ---" >&2
    sed -n '1,200p' "$file" >&2
    exit 1
  fi
}

compile_sample() {
  local sample="$1"
  local output="$2"

  "$compiler" --debug "$sample" -o "$output"
  opt -passes=verify "$output.ll" -disable-output
}

run_lldb() {
  local binary="$1"
  local source_name="$2"
  local line="$3"
  local output_file="$4"
  shift 4

  local cmd=(lldb --batch
    -o "target create \"$binary\""
    -o "breakpoint set --file $source_name --line $line"
    -o "run")

  while (($#)); do
    cmd+=(-o "$1")
    shift
  done

  "${cmd[@]}" >"$output_file"
}

run_lldb_globals() {
  local binary="$1"
  local output_file="$2"
  shift 2

  local cmd=(lldb --batch
    -o "target create \"$binary\"")

  while (($#)); do
    cmd+=(-o "$1")
    shift
  done

  "${cmd[@]}" >"$output_file"
}

func_bin="$work_dir/lldb_smoke_func_bin"
array_bin="$work_dir/lldb_smoke_array_bin"
block_bin="$work_dir/lldb_smoke_block_scope_bin"
globals_bin="$work_dir/lldb_smoke_globals_bin"
func_info="$work_dir/lldb_smoke_func.debug_info.txt"
array_info="$work_dir/lldb_smoke_array.debug_info.txt"
block_info="$work_dir/lldb_smoke_block_scope.debug_info.txt"
globals_info="$work_dir/lldb_smoke_globals.debug_info.txt"
func_lldb="$work_dir/lldb_smoke_func.lldb.txt"
array_lldb="$work_dir/lldb_smoke_array.lldb.txt"
block_lldb="$work_dir/lldb_smoke_block_scope.lldb.txt"
globals_lldb="$work_dir/lldb_smoke_globals.lldb.txt"

compile_sample "$repo_root/tmp/lldb_smoke_func.cr" "$func_bin"
compile_sample "$repo_root/tmp/lldb_smoke_array.cr" "$array_bin"
compile_sample "$repo_root/scripts/lldb_smoke_block_scope.cr" "$block_bin"
compile_sample "$repo_root/scripts/lldb_smoke_globals.cr" "$globals_bin"

xcrun llvm-dwarfdump --debug-info "$func_bin.o" >"$func_info"
xcrun llvm-dwarfdump --debug-info "$array_bin.o" >"$array_info"
xcrun llvm-dwarfdump --debug-info "$block_bin.o" >"$block_info"
xcrun llvm-dwarfdump --debug-info "$globals_bin.o" >"$globals_info"

require_contains 'DW_AT_name	("local")' "$func_info"
require_contains 'DW_AT_name	("pair")' "$func_info"
require_contains 'DW_AT_name	("total")' "$func_info"
require_contains 'DW_AT_name	("arr")' "$array_info"
require_contains 'DW_AT_name	("local")' "$array_info"
require_contains 'DW_AT_name	("total")' "$array_info"
require_contains 'DW_AT_name	("inner")' "$block_info"
require_contains 'DW_AT_name	("VALUE")' "$globals_info"
require_contains 'DW_AT_linkage_name	("_ZN4Demo5VALUEE")' "$globals_info"

run_lldb "$func_bin" "lldb_smoke_func.cr" 5 "$func_lldb" \
  "frame variable -T local pair total" \
  "quit"

require_contains '(int) local = 22' "$func_lldb"
require_contains '(Tuple(Int32, Int32)) pair = {' "$func_lldb"
require_contains '(int) [0] = 22' "$func_lldb"
require_contains '(int) [1] = 44' "$func_lldb"
require_contains '(int) total = 66' "$func_lldb"

run_lldb "$array_bin" "lldb_smoke_array.cr" 5 "$array_lldb" \
  "frame variable -T arr local total" \
  "frame variable -T *arr" \
  "quit"

require_contains '(Array(Int32) *) arr = ' "$array_lldb"
require_contains '(int) local = 22' "$array_lldb"
require_contains '(int) total = 66' "$array_lldb"
require_contains '(Array(Int32)) *arr = {' "$array_lldb"
require_contains '(int) [0] = 22' "$array_lldb"
require_contains '(int) [1] = 44' "$array_lldb"

run_lldb "$block_bin" "lldb_smoke_block_scope.cr" 5 "$block_lldb" \
  "frame variable a b inner" \
  "quit"

require_contains '(int) inner' "$block_lldb"
if rg -q "undeclared identifier .inner" "$block_lldb"; then
  echo "expected block-local inner to be visible in LLDB at line 5" >&2
  exit 1
fi

run_lldb_globals "$globals_bin" "$globals_lldb" \
  "target variable Demo::VALUE" \
  "quit"

require_contains 'Demo::VALUE = 42' "$globals_lldb"
if rg -q "can't find global variable .Demo::VALUE" "$globals_lldb"; then
  echo "expected LLDB to resolve static constant Demo::VALUE by source name" >&2
  exit 1
fi

echo "LLDB local-variable regression checks passed."

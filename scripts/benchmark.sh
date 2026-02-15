#!/usr/bin/env bash
# Compare compile/runtime speed between original Crystal and Crystal v2.
# For each benchmark:
# 1) compile both (release)
# 2) validate same stdout + same exit code
# 3) measure runtime average on N runs
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

V2_BIN="${V2_BIN:-$ROOT/bin/crystal_v2}"
ORIG_BIN="${ORIG_BIN:-crystal}"
RUNS="${RUNS:-5}"
TMP_DIR="${TMP_DIR:-/tmp/crystal_v2_bench_compare.$$}"
KEEP_BINS=0

# Default flags: disable v2 pipeline caches to benchmark compiler pipeline itself.
V2_FLAGS=("--no-ast-cache" "--no-llvm-cache")
ORIG_FLAGS=()

usage() {
  cat <<'EOF'
Usage:
  scripts/benchmark.sh [options] [CASE...]

CASE format:
  - path/to/source.cr
      Uses same source for both compilers.
  - v2_source.cr:orig_source.cr
      Explicit pair.

Options:
  --v2-bin PATH          Crystal v2 compiler binary (default: ./bin/crystal_v2)
  --orig-bin PATH        Original Crystal binary (default: crystal)
  --runs N               Runtime runs per binary (default: 5)
  --tmp-dir PATH         Directory for produced binaries/logs
  --keep-bins            Do not remove temporary binaries
  --v2-no-cache          Keep default v2 no-cache flags (default behavior)
  --v2-cache             Remove default v2 no-cache flags
  --help                 Show this help

Examples:
  scripts/benchmark.sh examples/bench_fib42_crystal.cr
  scripts/benchmark.sh examples/bench_fib42.cr
  scripts/benchmark.sh examples/bench_fib42.cr:examples/bench_fib42_crystal.cr
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --v2-bin)
      V2_BIN="$2"; shift 2 ;;
    --orig-bin)
      ORIG_BIN="$2"; shift 2 ;;
    --runs)
      RUNS="$2"; shift 2 ;;
    --tmp-dir)
      TMP_DIR="$2"; shift 2 ;;
    --keep-bins)
      KEEP_BINS=1; shift ;;
    --v2-no-cache)
      V2_FLAGS=("--no-ast-cache" "--no-llvm-cache"); shift ;;
    --v2-cache)
      V2_FLAGS=(); shift ;;
    --help|-h)
      usage; exit 0 ;;
    --)
      shift; break ;;
    -*)
      echo "error: unknown option: $1" >&2
      usage
      exit 1 ;;
    *)
      break ;;
  esac
done

if [[ $# -eq 0 ]]; then
  set -- "examples/bench_fib42_crystal.cr"
fi

if [[ ! -x "$V2_BIN" ]]; then
  echo "error: v2 binary not executable: $V2_BIN" >&2
  exit 1
fi

if ! command -v "$ORIG_BIN" >/dev/null 2>&1; then
  echo "error: original Crystal binary not found in PATH: $ORIG_BIN" >&2
  exit 1
fi

mkdir -p "$TMP_DIR"
if [[ $KEEP_BINS -ne 1 ]]; then
  trap 'rm -rf "$TMP_DIR"' EXIT
fi

now_s() {
  perl -MTime::HiRes=time -e 'printf "%.6f", time'
}

elapsed_s() {
  awk -v s="$1" -v e="$2" 'BEGIN { printf "%.6f", (e - s) }'
}

safe_basename() {
  local path="$1"
  local base
  base="$(basename "$path")"
  base="${base%.cr}"
  printf '%s\n' "${base//[^a-zA-Z0-9_.-]/_}"
}

resolve_pair() {
  local spec="$1"
  local v2_src orig_src
  if [[ "$spec" == *:* ]]; then
    v2_src="${spec%%:*}"
    orig_src="${spec#*:}"
  else
    v2_src="$spec"
    orig_src="$v2_src"
  fi
  printf '%s\n%s\n' "$v2_src" "$orig_src"
}

compile_with_timing() {
  local log_file="$1"; shift
  local start end elapsed rc
  start="$(now_s)"
  set +e
  "$@" >"$log_file" 2>&1
  rc=$?
  set -e
  end="$(now_s)"
  elapsed="$(elapsed_s "$start" "$end")"
  printf '%s\n%s\n' "$rc" "$elapsed"
}

runtime_measure() {
  local bin="$1"
  local runs="$2"
  local expected_stdout="$3"
  local expected_status="$4"
  local out_file="$TMP_DIR/runtime.out"
  local err_file="$TMP_DIR/runtime.err"
  local i status start end t sum min max stdout

  sum="0.0"
  min=""
  max="0.0"

  for ((i = 1; i <= runs; i++)); do
    start="$(now_s)"
    set +e
    "$bin" >"$out_file" 2>"$err_file"
    status=$?
    set -e
    end="$(now_s)"
    t="$(elapsed_s "$start" "$end")"
    stdout="$(cat "$out_file")"

    if [[ "$status" -ne "$expected_status" || "$stdout" != "$expected_stdout" ]]; then
      echo "RUNTIME_MISMATCH"
      return
    fi

    sum="$(awk -v a="$sum" -v b="$t" 'BEGIN { printf "%.6f", a + b }')"
    if [[ -z "$min" ]]; then
      min="$t"
    else
      min="$(awk -v a="$min" -v b="$t" 'BEGIN { printf "%.6f", (a < b ? a : b) }')"
    fi
    max="$(awk -v a="$max" -v b="$t" 'BEGIN { printf "%.6f", (a > b ? a : b) }')"
  done

  awk -v s="$sum" -v n="$runs" -v min="$min" -v max="$max" \
    'BEGIN { printf "OK\n%.6f\n%.6f\n%.6f\n", s / n, min, max }'
}

printf 'case,compile_orig_s,compile_v2_s,compile_ratio_v2_over_orig,run_orig_avg_s,run_v2_avg_s,run_ratio_orig_over_v2,run_orig_min_s,run_v2_min_s,run_min_ratio_orig_over_v2,status\n'

for spec in "$@"; do
  mapfile -t pair < <(resolve_pair "$spec")
  v2_src="${pair[0]}"
  orig_src="${pair[1]}"

  if [[ ! -f "$v2_src" ]]; then
    echo "error: missing v2 source: $v2_src" >&2
    exit 1
  fi
  if [[ ! -f "$orig_src" ]]; then
    echo "error: missing original source: $orig_src" >&2
    exit 1
  fi

  stem="$(safe_basename "$v2_src")"
  v2_out="$TMP_DIR/${stem}.v2"
  orig_out="$TMP_DIR/${stem}.orig"

  mapfile -t orig_compile < <(compile_with_timing "$TMP_DIR/${stem}.orig.compile.log" "$ORIG_BIN" build --release "${ORIG_FLAGS[@]}" "$orig_src" -o "$orig_out")
  orig_rc="${orig_compile[0]}"
  orig_compile_s="${orig_compile[1]}"
  if [[ "$orig_rc" -ne 0 ]]; then
    printf '%s,NA,NA,NA,NA,NA,NA,NA,NA,NA,orig_compile_failed\n' "$spec"
    continue
  fi

  mapfile -t v2_compile < <(compile_with_timing "$TMP_DIR/${stem}.v2.compile.log" "$V2_BIN" --release "${V2_FLAGS[@]}" "$v2_src" -o "$v2_out")
  v2_rc="${v2_compile[0]}"
  v2_compile_s="${v2_compile[1]}"
  if [[ "$v2_rc" -ne 0 ]]; then
    printf '%s,%s,NA,NA,NA,NA,NA,NA,NA,NA,v2_compile_failed\n' "$spec" "$orig_compile_s"
    continue
  fi

  out_file="$TMP_DIR/check.out"
  err_file="$TMP_DIR/check.err"

  set +e
  "$orig_out" >"$out_file" 2>"$err_file"
  orig_status=$?
  set -e
  orig_stdout="$(cat "$out_file")"

  set +e
  "$v2_out" >"$out_file" 2>"$err_file"
  v2_status=$?
  set -e
  v2_stdout="$(cat "$out_file")"

  compile_ratio="$(awk -v o="$orig_compile_s" -v v="$v2_compile_s" 'BEGIN { if (o == 0) print "NA"; else printf "%.4f", v / o }')"

  if [[ "$orig_status" -ne "$v2_status" || "$orig_stdout" != "$v2_stdout" ]]; then
    printf '%s,%s,%s,%s,NA,NA,NA,NA,NA,NA,output_mismatch\n' "$spec" "$orig_compile_s" "$v2_compile_s" "$compile_ratio"
    continue
  fi

  mapfile -t orig_run < <(runtime_measure "$orig_out" "$RUNS" "$orig_stdout" "$orig_status")
  if [[ "${orig_run[0]}" != "OK" ]]; then
    printf '%s,%s,%s,%s,NA,NA,NA,NA,NA,NA,orig_runtime_mismatch\n' "$spec" "$orig_compile_s" "$v2_compile_s" "$compile_ratio"
    continue
  fi

  mapfile -t v2_run < <(runtime_measure "$v2_out" "$RUNS" "$orig_stdout" "$orig_status")
  if [[ "${v2_run[0]}" != "OK" ]]; then
    printf '%s,%s,%s,%s,%s,NA,NA,%s,NA,NA,v2_runtime_mismatch\n' "$spec" "$orig_compile_s" "$v2_compile_s" "$compile_ratio" "${orig_run[1]}" "${orig_run[2]}"
    continue
  fi

  run_ratio="$(awk -v o="${orig_run[1]}" -v v="${v2_run[1]}" 'BEGIN { if (v == 0) print "NA"; else printf "%.4f", o / v }')"
  min_ratio="$(awk -v o="${orig_run[2]}" -v v="${v2_run[2]}" 'BEGIN { if (v == 0) print "NA"; else printf "%.4f", o / v }')"
  printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,ok\n' "$spec" "$orig_compile_s" "$v2_compile_s" "$compile_ratio" "${orig_run[1]}" "${v2_run[1]}" "$run_ratio" "${orig_run[2]}" "${v2_run[2]}" "$min_ratio"
done

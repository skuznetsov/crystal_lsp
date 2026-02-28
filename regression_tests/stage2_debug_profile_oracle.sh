#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <stage2_compiler> [timeout_sec] [mode] [n_csv]" >&2
  echo "  mode: release|debug (default: release)" >&2
  echo "  n_csv: comma-separated concrete-type counts (default: 1200,4000)" >&2
  exit 2
fi

COMPILER="$1"
TIMEOUT_SEC="${2:-240}"
MODE="${3:-release}"
N_CSV="${4:-1200,4000}"
OUT_ROOT="${TMPDIR:-/tmp}/stage2_debug_profile_oracle"
SUMMARY_TSV="$OUT_ROOT/summary.tsv"

mkdir -p "$OUT_ROOT"
printf "n\tstatus\ttotal_ms\tprobe_dir\n" > "$SUMMARY_TSV"

case "$MODE" in
  release)
    COMPILER_FLAGS=(--release)
    ;;
  debug)
    COMPILER_FLAGS=()
    ;;
  *)
    echo "unknown mode: $MODE (expected release|debug)" >&2
    exit 2
    ;;
esac

make_source() {
  local src="$1"
  local n="$2"

  cat > "$src" <<'CR'
class Box(T)
  @value : T
  def initialize(@value : T)
  end

  def each_twice
    yield @value
    yield @value
  end
end
CR

  local i=0
  while [ "$i" -lt "$n" ]; do
    cat >> "$src" <<CR
struct S${i}
  getter v : Int32
  def initialize(@v : Int32)
  end
end
CR
    i=$((i + 1))
  done

  cat >> "$src" <<'CR'
sum = 0
CR

  i=0
  while [ "$i" -lt "$n" ]; do
    cat >> "$src" <<CR
b${i} = Box(S${i}).new(S${i}.new(${i}))
b${i}.each_twice { |x| sum += x.v }
CR
    i=$((i + 1))
  done

  cat >> "$src" <<'CR'
sum
CR
}

overall_status=0
IFS=',' read -r -a N_VALUES <<< "$N_CSV"

for raw_n in "${N_VALUES[@]}"; do
  n="$(printf '%s' "$raw_n" | tr -d '[:space:]')"
  if [ -z "$n" ]; then
    continue
  fi
  if ! [[ "$n" =~ ^[0-9]+$ ]]; then
    echo "invalid N value: $n" >&2
    exit 2
  fi

  case_dir="$OUT_ROOT/n${n}"
  probe_dir="$case_dir/probe"
  src="$case_dir/yield_scan_stress.cr"
  bin="$case_dir/yield_scan_stress.bin"
  mkdir -p "$case_dir"
  make_source "$src" "$n"

  echo "== N=$n mode=$MODE timeout=${TIMEOUT_SEC}s =="
  set +e
  scripts/timeout_sample_lldb.sh \
    --timeout "$TIMEOUT_SEC" \
    --sample 6 \
    --top 12 \
    --series-start 30 \
    --series-interval 60 \
    --series-duration 6 \
    --out "$probe_dir" -- \
    /usr/bin/time -p "$COMPILER" "${COMPILER_FLAGS[@]}" --debug-profile "$src" -o "$bin"
  st=$?
  set -e

  cmd_log="$probe_dir/command.log"
  timing_line=""
  debug_line=""
  total_ms="NA"

  if [ -f "$cmd_log" ]; then
    timing_line="$(rg -m1 '^Timing \(ms\):' "$cmd_log" || true)"
    debug_line="$(rg -m1 '^DebugProfile:' "$cmd_log" || true)"
    parsed_total="$(printf '%s\n' "$timing_line" | sed -n 's/.* total=\([0-9.]*\).*/\1/p')"
    if [ -n "$parsed_total" ]; then
      total_ms="$parsed_total"
    fi
  fi

  echo "status: $st"
  [ -n "$timing_line" ] && echo "$timing_line"
  [ -n "$debug_line" ] && echo "$debug_line"
  echo "probe: $probe_dir"
  printf "%s\t%s\t%s\t%s\n" "$n" "$st" "$total_ms" "$probe_dir" >> "$SUMMARY_TSV"

  if [ "$st" -ne 0 ] && [ "$st" -ne 124 ]; then
    overall_status=1
  fi
done

echo "summary: $SUMMARY_TSV"
exit "$overall_status"


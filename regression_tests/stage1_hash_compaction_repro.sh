#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_ROOT="${TMPDIR:-/tmp}/stage1_hash_compaction_repro"
SRC="$TMP_ROOT/repro.cr"
BIN="$TMP_ROOT/repro_bin"
COMPILE_LOG="$TMP_ROOT/compile.log"
RUN_LOG="$TMP_ROOT/run.log"

mkdir -p "$TMP_ROOT"

cat > "$SRC" <<'CR'
n = 65
h = {} of String => String

n.times do |i|
  h["k#{i}"] = i.to_s
end

deleted = 0
n.times do |i|
  if i.even?
    h.delete("k#{i}")
    deleted += 1
  end
end

deleted.times do |i|
  h["x#{i}"] = (i + 100_000).to_s
end

ok = true
sum = 0_i64
n.times do |i|
  v = h["k#{i}"]?
  if i.even?
    ok = false unless v.nil?
  else
    if v.nil?
      ok = false
    else
      sum += v.to_i64
    end
  end
end

deleted.times do |i|
  v = h["x#{i}"]?
  if v.nil?
    ok = false
  else
    sum += v.to_i64
  end
end

if ok
  puts "ok n=#{n} deleted=#{deleted} size=#{h.size} sum=#{sum}"
else
  puts "bad n=#{n} deleted=#{deleted} size=#{h.size} sum=#{sum}"
  exit 9
end
CR

if ! "$COMPILER" "$SRC" -o "$BIN" >"$COMPILE_LOG" 2>&1; then
  echo "FAIL: compile error (compiler: $COMPILER)" >&2
  echo "log: $COMPILE_LOG" >&2
  tail -n 120 "$COMPILE_LOG" >&2
  exit 1
fi

set +e
scripts/run_safe.sh "$BIN" 8 1024 >"$RUN_LOG" 2>&1
run_status=$?
set -e

if [ $run_status -ne 0 ]; then
  echo "FAIL: runtime regression reproduced (exit $run_status)" >&2
  echo "run log: $RUN_LOG" >&2
  tail -n 120 "$RUN_LOG" >&2
  exit $run_status
fi

if ! grep -q "ok n=65" "$RUN_LOG"; then
  echo "FAIL: unexpected output" >&2
  echo "run log: $RUN_LOG" >&2
  tail -n 120 "$RUN_LOG" >&2
  exit 1
fi

echo "PASS: hash compaction repro"

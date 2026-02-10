#!/bin/bash
# Safe runner for Crystal V2 test binaries
# Prevents FD leaks and memory exhaustion from freezing the machine
# Usage: scripts/run_safe.sh <binary> [timeout_sec] [max_mem_mb]
BINARY="$1"
TIMEOUT="${2:-5}"
MAX_MEM="${3:-512}"

if [ -z "$BINARY" ]; then
  echo "Usage: $0 <binary> [timeout_sec=5] [max_mem_mb=512]"
  exit 1
fi

STDOUT_TMP=$(mktemp /tmp/run_safe_stdout.XXXXXX)
STDERR_TMP=$(mktemp /tmp/run_safe_stderr.XXXXXX)
trap "rm -f $STDOUT_TMP $STDERR_TMP" EXIT

# Run in background, capture output
"$BINARY" > "$STDOUT_TMP" 2> "$STDERR_TMP" &
PID=$!

# Monitor loop (0.5s granularity)
HALF_SECS=0
MAX_HALF_SECS=$((TIMEOUT * 2))
while [ $HALF_SECS -lt $MAX_HALF_SECS ]; do
  if ! kill -0 $PID 2>/dev/null; then
    wait $PID
    EXIT=$?
    echo "=== STDOUT ==="
    cat "$STDOUT_TMP"
    echo "=== STDERR ==="
    cat "$STDERR_TMP"
    if [ $EXIT -eq 139 ]; then echo "[CRASH] Segfault (exit 139)"; fi
    if [ $EXIT -eq 134 ]; then echo "[CRASH] Abort (exit 134)"; fi
    SECS=$((HALF_SECS / 2))
    echo "[EXIT: $EXIT] after ~${SECS}s"
    exit $EXIT
  fi

  # Check FD count (macOS lsof)
  FD_COUNT=$(lsof -p $PID 2>/dev/null | wc -l | tr -d ' ')
  # Check RSS in KB
  RSS=$(ps -o rss= -p $PID 2>/dev/null | tr -d ' ')

  if [ -n "$FD_COUNT" ] && [ "$FD_COUNT" -gt 1000 ]; then
    SECS=$((HALF_SECS / 2))
    echo "[KILL] FD leak detected: $FD_COUNT FDs after ~${SECS}s"
    kill -9 $PID 2>/dev/null; wait $PID 2>/dev/null
    echo "=== STDOUT ===" ; cat "$STDOUT_TMP"
    echo "=== STDERR ===" ; cat "$STDERR_TMP"
    exit 1
  fi

  if [ -n "$RSS" ] && [ "$RSS" -gt $((MAX_MEM * 1024)) ]; then
    SECS=$((HALF_SECS / 2))
    echo "[KILL] Memory limit: ${RSS}KB > ${MAX_MEM}MB after ~${SECS}s"
    kill -9 $PID 2>/dev/null; wait $PID 2>/dev/null
    echo "=== STDOUT ===" ; cat "$STDOUT_TMP"
    echo "=== STDERR ===" ; cat "$STDERR_TMP"
    exit 1
  fi

  sleep 0.5
  HALF_SECS=$((HALF_SECS + 1))
done

# Timeout
FD_COUNT=$(lsof -p $PID 2>/dev/null | wc -l | tr -d ' ')
RSS=$(ps -o rss= -p $PID 2>/dev/null | tr -d ' ')
echo "[KILL] Timeout after ${TIMEOUT}s (FDs: ${FD_COUNT:-?}, RSS: ${RSS:-?}KB)"
kill -9 $PID 2>/dev/null; wait $PID 2>/dev/null
echo "=== STDOUT ===" ; cat "$STDOUT_TMP"
echo "=== STDERR ===" ; cat "$STDERR_TMP"
exit 1

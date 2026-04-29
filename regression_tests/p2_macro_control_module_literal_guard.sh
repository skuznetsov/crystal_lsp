#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"
TMP_DIR="$(mktemp -d /tmp/p2_macro_control_module_literal_XXXXXX)"
SOURCE="$TMP_DIR/kqueue_after_fork.cr"
OUT="$TMP_DIR/kqueue_after_fork"
LOG="$TMP_DIR/compile.log"
BODY="$TMP_DIR/kqueue_after_fork_body.hir"

cleanup() {
  if [[ "${KEEP_TMP:-0}" != "1" ]]; then
    rm -rf "$TMP_DIR"
  else
    echo "[p2_macro_control_module_literal_guard] kept tmp: $TMP_DIR" >&2
  fi
}
trap cleanup EXIT

case "$(uname -s)" in
  Darwin|FreeBSD|OpenBSD|NetBSD|DragonFly)
    ;;
  *)
    echo "p2_macro_control_module_literal_guard_skip: kqueue-only guard"
    exit 0
    ;;
esac

if [[ ! -x "$COMPILER" ]]; then
  echo "p2_macro_control_module_literal_guard_failed: compiler not found: $COMPILER" >&2
  exit 2
fi

cat >"$SOURCE" <<'CR'
Crystal::EventLoop.current.after_fork
CR

CRYSTAL_V2_STOP_AFTER_HIR=1 "$ROOT_DIR/scripts/run_safe.sh" "$COMPILER" 120 2048 \
  "$SOURCE" --emit hir --no-link -o "$OUT" >"$LOG" 2>&1

if [[ ! -f "$OUT.hir" ]]; then
  echo "p2_macro_control_module_literal_guard_failed: missing HIR output" >&2
  tail -80 "$LOG" >&2 || true
  exit 1
fi

awk '/func @Crystal::EventLoop::Kqueue#after_fork\(/ {show=1} show {print} show && /^}$/ {exit}' \
  "$OUT.hir" >"$BODY"

if [[ ! -s "$BODY" ]]; then
  echo "p2_macro_control_module_literal_guard_failed: missing Kqueue#after_fork body" >&2
  rg -n 'after_fork' "$OUT.hir" >&2 || true
  exit 1
fi

if ! grep -q 'LibC.@@EVFILT_USER' "$BODY"; then
  echo "p2_macro_control_module_literal_guard_failed: EVFILT_USER branch missing" >&2
  sed -n '1,120p' "$BODY" >&2
  exit 1
fi

if grep -Eq 'Crystal::System::FileDescriptor\.system_pipe|LibC\.@@EVFILT_READ|Crystal::EventLoop::Polling#pipe' "$BODY"; then
  echo "p2_macro_control_module_literal_guard_failed: fallback pipe branch leaked into Kqueue#after_fork" >&2
  sed -n '1,140p' "$BODY" >&2
  exit 1
fi

echo "p2_macro_control_module_literal_guard_ok"

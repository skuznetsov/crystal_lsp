#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: cleanup_tmp_stage_artifacts.sh [options]

Safely clean Crystal bootstrap/debug artifacts in /tmp by default.
Dry-run is default; pass --yes to actually remove files.

Options:
  --yes                Delete matched entries (default: dry-run only)
  --tmpdir <path>      Temp directory root (default: /tmp)
  --keep <pattern>     Shell glob of basenames to preserve (repeatable)
  --older-than-days N  Only include entries older than N days
  -h, --help           Show this help

Examples:
  scripts/cleanup_tmp_stage_artifacts.sh
  scripts/cleanup_tmp_stage_artifacts.sh --yes
  scripts/cleanup_tmp_stage_artifacts.sh --yes \
    --keep 'stage1_rel_autonomous_*' \
    --keep 'stage2_rel_autonomous_*'
EOF
}

TMP_ROOT="/tmp"
DO_DELETE=0
OLDER_THAN_DAYS=""
KEEP_PATTERNS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --yes)
      DO_DELETE=1
      shift
      ;;
    --tmpdir)
      if [[ $# -lt 2 ]]; then
        echo "error: --tmpdir requires a value" >&2
        exit 2
      fi
      TMP_ROOT="$2"
      shift 2
      ;;
    --keep)
      if [[ $# -lt 2 ]]; then
        echo "error: --keep requires a glob pattern" >&2
        exit 2
      fi
      KEEP_PATTERNS+=("$2")
      shift 2
      ;;
    --older-than-days)
      if [[ $# -lt 2 ]]; then
        echo "error: --older-than-days requires an integer value" >&2
        exit 2
      fi
      if ! [[ "$2" =~ ^[0-9]+$ ]]; then
        echo "error: --older-than-days expects a non-negative integer" >&2
        exit 2
      fi
      OLDER_THAN_DAYS="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "error: unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if [[ ! -d "$TMP_ROOT" ]]; then
  echo "error: tmpdir does not exist: $TMP_ROOT" >&2
  exit 2
fi

stage_patterns=(
  "stage1_*"
  "stage2_*"
  "stage3_*"
  "crystal_cache_original_*"
  "crystal_cache_stage1_*"
  "crystal_cache_stage2_*"
  "crystal_cache_stage3_*"
  "opt_bi_*"
  "opt_bisect_*"
)

is_kept() {
  local base=$1
  local pat
  for pat in "${KEEP_PATTERNS[@]}"; do
    if [[ "$base" == $pat ]]; then
      return 0
    fi
  done
  return 1
}

is_old_enough() {
  local path=$1
  if [[ -z "$OLDER_THAN_DAYS" ]]; then
    return 0
  fi
  # macOS/BSD stat + GNU fallback.
  local now mtime age_days
  now="$(date +%s)"
  if mtime="$(stat -f %m "$path" 2>/dev/null)"; then
    :
  elif mtime="$(stat -c %Y "$path" 2>/dev/null)"; then
    :
  else
    return 1
  fi
  age_days=$(( (now - mtime) / 86400 ))
  [[ "$age_days" -ge "$OLDER_THAN_DAYS" ]]
}

files_to_remove=()
for pat in "${stage_patterns[@]}"; do
  for path in "$TMP_ROOT"/$pat; do
    [[ -e "$path" ]] || continue
    base="$(basename "$path")"
    if is_kept "$base"; then
      continue
    fi
    if ! is_old_enough "$path"; then
      continue
    fi
    files_to_remove+=("$path")
  done
done

if [[ ${#files_to_remove[@]} -eq 0 ]]; then
  echo "No matching artifacts found in $TMP_ROOT"
  exit 0
fi

echo "Matched artifacts in $TMP_ROOT: ${#files_to_remove[@]}"
total_kb=0
for path in "${files_to_remove[@]}"; do
  size_kb="$(du -sk "$path" 2>/dev/null | awk '{print $1}')"
  size_kb="${size_kb:-0}"
  total_kb=$((total_kb + size_kb))
  printf "  %8s KB  %s\n" "$size_kb" "$path"
done
printf "Total: %.2f GB\n" "$(awk "BEGIN {print $total_kb / 1024 / 1024}")"

if [[ "$DO_DELETE" -eq 0 ]]; then
  echo "Dry-run only. Re-run with --yes to delete."
  exit 0
fi

echo "Deleting matched artifacts..."
for path in "${files_to_remove[@]}"; do
  rm -rf "$path"
done
echo "Done."

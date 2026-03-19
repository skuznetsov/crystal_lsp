#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 /path/to/compiler" >&2
  exit 2
fi

compiler=$1
repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
attempts=10

make_source_file() {
  local path=$1
  cat >"$path" <<'EOF'
class Probe
  def touch
  end

  def seed
    x = 1
  end

  private def parse_args_safe : Int32
    status = 0
    opt_level_invalid = false
    z = 1
    z
    i = 0
    while i < 4
      1
      if true
        if i + 1 < 4
          if true
            status = 1
            i += 1
          else
            opt_level_invalid = true
          end
        else
          opt_level_invalid = true
        end
      end
      i += 1
    end

    if status == 0 && opt_level_invalid
      status = 1
    end

    status
  end
end

1
EOF
}

for attempt in $(seq 1 "$attempts"); do
  log_dir=$(mktemp -d "${TMPDIR:-/tmp}/stage2_parse_args_tail_if_repro.XXXXXX")
  source_base=$(mktemp "$repo_root/stage2_parse_args_tail_if_repro.XXXXXX")
  rm -f "$source_base"
  source_file="${source_base}.cr"
  trap 'rm -f "$source_file"' EXIT
  make_source_file "$source_file"
  echo "[attempt $attempt/$attempts] $compiler"

  set +e
  env \
    CRYSTAL_V2_STOP_AFTER_PARSE=1 \
    CRYSTAL_V2_PIPELINE_CACHE=0 \
    CRYSTAL_V2_LLVM_CACHE=0 \
    "$repo_root/scripts/timeout_sample_lldb.sh" \
    -t 120 -m 16384 -s 5 -l 10 -n 8 --no-series \
    -o "$log_dir" \
    -- "$compiler" "$source_file" --release --no-prelude -o "$log_dir/out"
  status=$?
  set -e

  case "$status" in
    0)
      rm -f "$source_file"
      trap - EXIT
      ;;
    133|134|138|139)
      echo "reproduced: compiler crashed before STOP_AFTER_PARSE on the parse_args tail-if parser-shape repro"
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

echo "not reproduced: compiler reached STOP_AFTER_PARSE on all $attempts parse_args tail-if parser-shape repro attempts"

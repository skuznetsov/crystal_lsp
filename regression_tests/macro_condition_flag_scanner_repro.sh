#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

compiler="$1"
tmp_dir="$(mktemp -d "${TMPDIR:-/tmp}/macro-condition-flag-scanner.XXXXXX")"
src="$tmp_dir/main.cr"
bin="$tmp_dir/main.bin"
run_log="$tmp_dir/run.log"

cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

cat >"$src" <<'CR'
require "set"

class MiniFlagScanner
  def initialize(@input : String, @flags : Set(String))
    @index = 0
  end

  def parse : Bool?
    skip_ws
    parse_primary
  end

  private def parse_primary : Bool?
    ident = read_identifier
    return nil unless ident

    case ident
    when "flag?"
      parse_flag_call
    else
      nil
    end
  end

  private def parse_flag_call : Bool?
    skip_ws
    return nil unless peek_char == '('
    advance(1)
    skip_ws
    flag_name = parse_flag_name
    skip_ws
    advance(1) if peek_char == ')'
    return nil unless flag_name
    @flags.includes?(flag_name)
  end

  private def parse_flag_name : String?
    if peek_char == ':'
      advance(1)
    end
    read_identifier
  end

  private def read_identifier : String?
    skip_ws
    start = @index
    while char = peek_char
      break unless char.alphanumeric? || char == '_' || char == '?' || char == '!' || char == ':'
      advance(1)
    end
    return nil if @index == start
    @input.byte_slice(start, @index - start)
  end

  private def skip_ws : Nil
    while char = peek_char
      break unless char.whitespace?
      advance(1)
    end
  end

  private def peek_char : Char?
    return nil if @index >= @input.bytesize
    @input.byte_at(@index).unsafe_chr
  end

  private def advance(count : Int32) : Nil
    @index += count
  end
end

flags = Set(String).new
flags << "aarch64"
result = MiniFlagScanner.new("flag?(:aarch64)", flags).parse
puts result == true ? "true" : (result == false ? "false" : "nil")
CR

"$compiler" "$src" -o "$bin" >"$run_log.compile.out" 2>"$run_log.compile.err"
./scripts/run_safe.sh "$bin" 5 256 >"$run_log"

stdout="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$run_log" | tr -d '\r')"

echo "compiler: $compiler"
echo "stdout:"
printf '%s\n' "$stdout"

if [ "$stdout" = "true" ]; then
  echo "not reproduced"
  exit 1
fi

if [ "$stdout" = "nil" ]; then
  echo "reproduced: macro condition flag scanner lost non-nil identifier path"
  exit 0
fi

echo "unexpected output"
cat "$run_log"
exit 3

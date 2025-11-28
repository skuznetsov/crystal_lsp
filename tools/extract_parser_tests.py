#!/usr/bin/env python3
"""
Extract parser test cases from Crystal's parser_spec.cr.

Reads the original Crystal parser spec file and extracts all `it_parses` test cases,
outputting them in a format suitable for regression testing in crystal_v2.

Usage:
    python3 tools/extract_parser_tests.py ~/Projects/Crystal/crystal/spec/compiler/parser/parser_spec.cr
"""

import re
import sys
import json
from pathlib import Path


def extract_string_literal(s: str, start_pos: int) -> tuple[str, int]:
    """Extract a string literal starting at start_pos. Returns (content, end_pos)."""
    if start_pos >= len(s):
        return None, start_pos

    ch = s[start_pos]

    # Regular double-quoted string
    if ch == '"':
        i = start_pos + 1
        result = []
        while i < len(s):
            if s[i] == '\\' and i + 1 < len(s):
                # Handle escape sequences
                next_ch = s[i + 1]
                if next_ch == 'n':
                    result.append('\n')
                elif next_ch == 't':
                    result.append('\t')
                elif next_ch == 'r':
                    result.append('\r')
                elif next_ch == '"':
                    result.append('"')
                elif next_ch == '\\':
                    result.append('\\')
                elif next_ch == '\n':
                    # Line continuation - skip whitespace on next line
                    i += 2
                    while i < len(s) and s[i] in ' \t':
                        i += 1
                    continue
                else:
                    result.append(s[i:i+2])
                i += 2
            elif s[i] == '"':
                return ''.join(result), i + 1
            else:
                result.append(s[i])
                i += 1
        return None, start_pos

    # %(...) style string with various delimiters
    if ch == '%':
        if start_pos + 1 >= len(s):
            return None, start_pos

        quote_type = s[start_pos + 1]

        # Determine start char and find matching end char
        start_char = None
        end_char = None
        literal_start = start_pos + 2

        if quote_type in 'qQwWiI':
            if start_pos + 2 >= len(s):
                return None, start_pos
            start_char = s[start_pos + 2]
            literal_start = start_pos + 3
        else:
            start_char = quote_type
            literal_start = start_pos + 2

        if start_char == '(':
            end_char = ')'
        elif start_char == '[':
            end_char = ']'
        elif start_char == '{':
            end_char = '}'
        elif start_char == '<':
            end_char = '>'
        else:
            end_char = start_char

        # Find matching end
        i = literal_start
        depth = 1
        result = []
        while i < len(s) and depth > 0:
            if s[i] == '\\' and i + 1 < len(s):
                result.append(s[i:i+2])
                i += 2
            elif s[i] == start_char and start_char != end_char:
                depth += 1
                result.append(s[i])
                i += 1
            elif s[i] == end_char:
                depth -= 1
                if depth > 0:
                    result.append(s[i])
                i += 1
            else:
                result.append(s[i])
                i += 1

        return ''.join(result), i

    return None, start_pos


def extract_it_parses_calls(content: str) -> list[str]:
    """Extract all first arguments to it_parses calls."""
    results = []

    # Pattern to find it_parses calls
    pattern = re.compile(r'\bit_parses\s+')

    for match in pattern.finditer(content):
        pos = match.end()

        # Skip whitespace
        while pos < len(content) and content[pos] in ' \t\n':
            pos += 1

        if pos >= len(content):
            continue

        # Extract the first argument (the source string)
        source, end_pos = extract_string_literal(content, pos)
        if source is not None:
            results.append(source)

    return results


def generate_spec_file(test_cases: list[str], output_path: Path):
    """Generate a Crystal spec file that tests parsing all extracted cases."""
    # Write test cases as JSON for runtime loading
    import json

    json_path = output_path.parent / "parser_test_cases.json"
    json_path.write_text(json.dumps(test_cases, ensure_ascii=False, indent=None))

    spec_content = '''require "spec"
require "json"
require "../../src/compiler/frontend/parser"

# Auto-generated from Crystal's parser_spec.cr
# Tests that our parser can parse all syntax that Crystal's parser accepts

private def parses_without_error(source : String) : Bool
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer, recovery_mode: false)
  program = parser.parse_program
  parser.diagnostics.empty?
end

# Load test cases from JSON
PARSER_TEST_CASES = Array(String).from_json(
  File.read(File.join(__DIR__, "parser_test_cases.json"))
)

describe "Parser compatibility with Crystal" do
  PARSER_TEST_CASES.each_with_index do |source, idx|
    it "parses case #{idx + 1} without errors" do
      result = parses_without_error(source)
      unless result
        # Re-parse to get diagnostic message
        lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
        parser = CrystalV2::Compiler::Frontend::Parser.new(lexer, recovery_mode: false)
        parser.parse_program
        msg = parser.diagnostics.first?.try(&.message) || "unknown error"
        fail "Failed to parse: #{source.inspect[0..100]}... Error: #{msg}"
      end
      result.should be_true
    end
  end
end
'''

    output_path.write_text(spec_content)
    print(f"Generated spec with {len(test_cases)} test cases")
    print(f"  Spec file: {output_path}")
    print(f"  Test data: {json_path}")


def main():
    if len(sys.argv) < 2:
        print("Usage: python3 extract_parser_tests.py <path_to_parser_spec.cr>")
        print("       python3 extract_parser_tests.py --json  # output as JSON")
        sys.exit(1)

    as_json = '--json' in sys.argv
    spec_path = Path([a for a in sys.argv[1:] if not a.startswith('--')][0])

    if not spec_path.exists():
        print(f"Error: {spec_path} not found")
        sys.exit(1)

    content = spec_path.read_text()
    test_cases = extract_it_parses_calls(content)

    print(f"Extracted {len(test_cases)} test cases from {spec_path}")

    if as_json:
        print(json.dumps(test_cases, indent=2))
    else:
        # Generate spec file
        output_path = Path("spec/parser/parser_crystal_compat_spec.cr")
        generate_spec_file(test_cases, output_path)


if __name__ == "__main__":
    main()

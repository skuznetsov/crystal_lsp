require "spec"
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

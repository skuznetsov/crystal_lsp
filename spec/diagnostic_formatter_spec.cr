require "spec"

require "../src/compiler/frontend/diagnostic_formatter"

alias Span = CrystalV2::Compiler::Frontend::Span
alias Diagnostic = CrystalV2::Compiler::Frontend::Diagnostic
alias DiagnosticFormatter = CrystalV2::Compiler::Frontend::DiagnosticFormatter

describe DiagnosticFormatter do
  it "formats single line diagnostic with underline" do
    source = "foo + bar"
    span = Span.new(0, 0, 1, 5, 1, 8)
    diagnostic = Diagnostic.new("unexpected identifier", span)

    formatted = DiagnosticFormatter.format(source, diagnostic)
    formatted.should eq("1:5-1:8 unexpected identifier\n  1 | foo + bar\n    |     ^^^")
  end

  it "formats multi-line diagnostic" do
    source = %(line1\nline2\nline3)
    span = Span.new(0, 0, 2, 1, 3, 3)
    diagnostic = Diagnostic.new("multi-line issue", span)

    formatted = DiagnosticFormatter.format(source, diagnostic)
    formatted.should eq("2:1-3:3 multi-line issue\n  2 | line2\n    | ^^^^^\n  3 | line3\n    | ^^^")
  end

  it "falls back to base string when source unavailable" do
    span = Span.new(0, 0, 1, 1, 1, 1)
    diagnostic = Diagnostic.new("missing context", span)

    formatted = DiagnosticFormatter.format(nil, diagnostic)
    formatted.should eq("1:1-1:1 missing context")
  end
end

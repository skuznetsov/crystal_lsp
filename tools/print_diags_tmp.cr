#!/usr/bin/env crystal
require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

path = ARGV[0]?
abort "usage: print_diags <file>" unless path
source = File.read(path)
lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
tokens = [] of CrystalV2::Compiler::Frontend::Token
lexer.each_token(skip_trivia: false) { |t| tokens << t }

# Recreate parser with pre-tokenized lexer's result
sub_lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(sub_lexer)
parser.parse_program

if ENV["PRINT_TOKENS"]? == "1"
  range = ENV["FILTER_LINES"]?.try do |s|
    parts = s.split("-", 2)
    {parts[0].to_i, (parts[1]? || parts[0]).to_i}
  end
  puts "Tokens (kind/line:col):"
  tokens.each do |t|
    if range
      line = t.span.start_line
      next if line < range[0] || line > range[1]
    end
    puts "#{t.kind} @#{t.span}"
  end
  puts
end
puts "Diagnostics:"
parser.diagnostics.each { |d| puts "#{d.message} @#{d.span}" }

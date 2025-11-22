require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

abort "Usage: parse_one <file>" unless ARGV.size == 1
path = ARGV[0]
source = File.read(path)
lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer, recovery_mode: ENV["CRYSTAL_V2_LSP_RECOVERY"]? == "1")
parser.parse_program
puts "parsed #{path}"

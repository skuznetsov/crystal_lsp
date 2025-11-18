require "./compiler/cli"
require "./compiler/formatter"
require "./compiler/frontend/lexer"
require "./compiler/frontend/parser"

module CrystalV2
  module Compiler
    Lexer = Frontend::Lexer
    Parser = Frontend::Parser
  end
end

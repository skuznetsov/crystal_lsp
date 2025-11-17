require "./symbol_table"
require "../../runtime"

module CrystalV2
  module Compiler
    module Semantic
      class Context
        getter symbol_table : SymbolTable
        getter flags : Set(String)

        def initialize(@symbol_table : SymbolTable, @flags : Set(String) = Runtime.target_flags)
        end

        def child
          Context.new(SymbolTable.new(symbol_table), @flags)
        end
      end
    end
  end
end

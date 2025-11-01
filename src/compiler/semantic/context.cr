require "./symbol_table"

module CrystalV2
  module Compiler
    module Semantic
      class Context
        getter symbol_table : SymbolTable

        def initialize(@symbol_table : SymbolTable)
        end

        def child
          Context.new(SymbolTable.new(symbol_table))
        end
      end
    end
  end
end

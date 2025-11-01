require "./symbol"

module CrystalV2
  module Compiler
    module Semantic
      class SymbolTable
        getter parent : SymbolTable?

        def initialize(@parent : SymbolTable? = nil)
          @symbols = {} of String => Symbol
        end

        def define(name : String, symbol : Symbol)
          if existing = @symbols[name]?
            raise SymbolRedefinitionError.new(name, existing, symbol)
          end
          @symbols[name] = symbol
        end

        def redefine(name : String, symbol : Symbol)
          @symbols[name] = symbol
        end

        def lookup(name : String) : Symbol?
          @symbols[name]? || @parent.try(&.lookup(name))
        end

        def local?(name : String) : Bool
          @symbols.has_key?(name)
        end

        def lookup_local(name : String) : Symbol?
          @symbols[name]?
        end

        def each_local_symbol(&block : String, Symbol ->)
          @symbols.each do |key, value|
            yield key, value
          end
        end

        private getter symbols : Hash(String, Symbol)
      end

      class SymbolRedefinitionError < Exception
        getter name : String
        getter existing : Symbol
        getter new_symbol : Symbol

        def initialize(@name : String, @existing : Symbol, @new_symbol : Symbol)
          super("symbol '#{name}' already defined")
        end
      end
    end
  end
end

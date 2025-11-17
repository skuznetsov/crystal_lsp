require "set"
require "./symbol"

module CrystalV2
  module Compiler
    module Semantic
      class SymbolTable
        getter parent : SymbolTable?

        getter included_modules : Array(ModuleSymbol)

        def initialize(@parent : SymbolTable? = nil)
          @symbols = {} of String => Symbol
          @included_modules = [] of ModuleSymbol
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
          @symbols[name]? || lookup_included(name, Set(SymbolTable).new) || @parent.try(&.lookup(name))
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

        def include_module(symbol : ModuleSymbol)
          unless @included_modules.includes?(symbol)
            @included_modules << symbol
          end
        end

        private getter symbols : Hash(String, Symbol)

        private def lookup_included(name : String, visited : Set(SymbolTable)) : Symbol?
          @included_modules.each do |mod_symbol|
            if result = lookup_in_scope(mod_symbol.scope, name, visited)
              return result
            end
          end
          nil
        end

        private def lookup_in_scope(table : SymbolTable, name : String, visited : Set(SymbolTable)) : Symbol?
          return nil unless visited.add?(table)

          if symbol = table.lookup_local(name)
            return symbol
          end

          table.included_modules.each do |mod_symbol|
            if result = lookup_in_scope(mod_symbol.scope, name, visited)
              return result
            end
          end

          if parent = table.parent
            return lookup_in_scope(parent, name, visited)
          end

          nil
        end
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

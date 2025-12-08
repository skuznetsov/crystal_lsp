require "./type"
require "../symbol"

module CrystalV2
  module Compiler
    module Semantic
      # Represents a virtual type (base class + any subclass)
      #
      # Phase 99: Virtual types for inheritance-aware method dispatch
      #
      # When a variable of type Animal could hold Dog, Cat, or any other
      # subclass, we use VirtualType(Animal) to represent this polymorphism.
      #
      # Key behaviors:
      # - Method calls dispatch to all possible subtypes
      # - Return type is union of all override return types
      # - is_a?(Subclass) narrows from VirtualType to concrete
      #
      # Examples:
      #   animal : Animal = Dog.new  # animal has VirtualType(Animal)
      #   animal.speak              # returns String | Int32 if overrides differ
      class VirtualType < Type
        getter base_class : ClassSymbol

        def initialize(@base_class : ClassSymbol)
        end

        def to_s(io : IO)
          io << @base_class.name
          io << "+"  # Convention: Animal+ means Animal or subclass
        end

        def ==(other : Type) : Bool
          return false unless other.is_a?(VirtualType)
          other.base_class == @base_class
        end

        def hash : UInt64
          @base_class.object_id.hash ^ 0xDEAD_BEEF_u64
        end

        # Check if this virtual type includes a specific type
        #
        # Returns true if type is:
        # - The base class itself
        # - Any subclass of the base class
        def includes_type?(type : Type, global_table : SymbolTable?) : Bool
          type_name = case type
                      when InstanceType then type.class_symbol.name
                      when ClassType    then type.symbol.name
                      when VirtualType  then type.base_class.name
                      else return false
                      end

          # Same as base class?
          return true if type_name == @base_class.name

          # Check if type is subclass of base
          check_is_subclass?(type_name, @base_class.name, global_table)
        end

        # Check if child_name is a subclass of parent_name
        private def check_is_subclass?(child_name : String, parent_name : String, global_table : SymbolTable?) : Bool
          return false unless global_table

          current = child_name
          visited = Set(String).new

          while current && !visited.includes?(current)
            return true if current == parent_name
            visited << current

            if sym = global_table.lookup(current)
              if sym.is_a?(ClassSymbol)
                current = sym.superclass_name
              else
                break
              end
            else
              break
            end
          end

          false
        end
      end
    end
  end
end

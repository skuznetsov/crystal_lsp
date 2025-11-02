require "../frontend/ast"

module CrystalV2
  module Compiler
    module Semantic
      alias ExprId = Frontend::ExprId

      abstract class Symbol
        getter name : String
        getter node_id : ExprId

        def initialize(@name : String, @node_id : ExprId)
        end
      end

      class MacroSymbol < Symbol
        getter body : ExprId
        getter params : Array(String)?

        def initialize(name : String, node_id : ExprId, @body : ExprId, @params : Array(String)? = nil)
          super(name, node_id)
        end
      end

      class MethodSymbol < Symbol
        getter params : Array(Frontend::Parameter)
        getter return_annotation : String?
        getter scope : SymbolTable

        def initialize(name : String, node_id : ExprId, *, params : Array(Frontend::Parameter) = [] of Frontend::Parameter, return_annotation : String? = nil, scope : SymbolTable)
          super(name, node_id)
          @params = params
          @return_annotation = return_annotation
          @scope = scope
        end
      end

      class ClassSymbol < Symbol
        getter scope : SymbolTable
        getter superclass_name : String?
        getter instance_vars : Hash(String, String?)  # name → type annotation
        getter type_parameters : Array(String)?  # Week 1: Generic type params ["T", "U"]

        def initialize(name : String, node_id : ExprId, *, scope : SymbolTable, superclass_name : String? = nil, type_parameters : Array(String)? = nil)
          super(name, node_id)
          @scope = scope
          @superclass_name = superclass_name
          @instance_vars = {} of String => String?
          @type_parameters = type_parameters
        end

        # Phase 5A: Track instance variable declarations
        def add_instance_var(name : String, type_annotation : String? = nil)
          @instance_vars[name] = type_annotation
        end

        def get_instance_var_type(name : String) : String?
          @instance_vars[name]?
        end
      end

      class VariableSymbol < Symbol
        getter declared_type : String?

        def initialize(name : String, node_id : ExprId, declared_type : String? = nil)
          super(name, node_id)
          @declared_type = declared_type
        end
      end

      # Overload set for methods with same name but different signatures
      #
      # Phase 4B: When multiple methods have same name, we collect them in OverloadSet
      # for overload resolution at call site
      class OverloadSetSymbol < Symbol
        getter overloads : Array(MethodSymbol)

        def initialize(name : String, node_id : ExprId, @overloads : Array(MethodSymbol) = [] of MethodSymbol)
          super(name, node_id)
        end

        def add_overload(method : MethodSymbol)
          @overloads << method
        end
      end

      # Test-only placeholder symbol for SymbolTable specs
      class DummySymbol < Symbol
        getter metadata : String

        def initialize(name : String, node_id : ExprId, @metadata : String)
          super(name, node_id)
        end
      end
    end
  end
end

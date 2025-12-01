require "../frontend/ast"

module CrystalV2
  module Compiler
    module Semantic
      alias ExprId = Frontend::ExprId

      abstract class Symbol
        getter name : String
        property node_id : ExprId
        getter file_path : String?

        def initialize(@name : String, @node_id : ExprId, file_path : String? = nil)
          @file_path = file_path
        end

        def file_path=(value : String?)
          @file_path = value
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
        getter type_parameters : Array(String)? # Week 1 Day 2: Generic method type params ["T", "U"]

        def initialize(name : String, node_id : ExprId, *, params : Array(Frontend::Parameter) = [] of Frontend::Parameter, return_annotation : String? = nil, scope : SymbolTable, type_parameters : Array(String)? = nil)
          super(name, node_id)
          @params = params
          @return_annotation = return_annotation
          @scope = scope
          @type_parameters = type_parameters
        end
      end

      class ClassSymbol < Symbol
        getter scope : SymbolTable
        getter class_scope : SymbolTable
        getter superclass_name : String?
        getter instance_vars : Hash(String, String?) # name → type annotation
        getter type_parameters : Array(String)?      # Week 1: Generic type params ["T", "U"]
        # Collected annotations attached to this class (e.g., @[JSON::Serializable::Options])
        getter annotations : Array(AnnotationInfo)
        # Collected annotations per instance variable name (without leading "@")
        getter ivar_annotations : Hash(String, Array(AnnotationInfo))

        def initialize(name : String, node_id : ExprId, *, scope : SymbolTable, class_scope : SymbolTable, superclass_name : String? = nil, type_parameters : Array(String)? = nil)
          super(name, node_id)
          @scope = scope
          @class_scope = class_scope
          @superclass_name = superclass_name
          @instance_vars = {} of String => String?
          @type_parameters = type_parameters
          @annotations = [] of AnnotationInfo
          @ivar_annotations = {} of String => Array(AnnotationInfo)
        end

        # Phase 5A: Track instance variable declarations
        def add_instance_var(name : String, type_annotation : String? = nil)
          @instance_vars[name] = type_annotation
        end

        def get_instance_var_type(name : String) : String?
          @instance_vars[name]?
        end

        # Attach a class-level annotation
        def add_annotation(annotation_info : AnnotationInfo)
          @annotations << annotation_info
        end

        # Attach an annotation to a specific instance variable (name without "@")
        def add_ivar_annotation(name : String, annotation_info : AnnotationInfo)
          list = @ivar_annotations[name]?
          unless list
            list = [] of AnnotationInfo
            @ivar_annotations[name] = list
          end
          list << annotation_info
        end

        # Phase 87B-4B: Get all method names defined in this class (for @type.methods)
        def methods : Array(String)
          result = [] of String
          @scope.each_local_symbol do |name, symbol|
            result << name if symbol.is_a?(MethodSymbol)
          end
          result.sort
        end

        # Phase 87B-4B: Check if class has a method with given name
        def has_method?(name : String) : Bool
          if sym = @scope.lookup_local(name)
            return sym.is_a?(MethodSymbol)
          end
          false
        end
      end

      class ModuleSymbol < Symbol
        getter scope : SymbolTable

        def initialize(name : String, node_id : ExprId, *, scope : SymbolTable)
          super(name, node_id)
          @scope = scope
        end
      end

      class ConstantSymbol < Symbol
        getter value : ExprId

        def initialize(name : String, node_id : ExprId, @value : ExprId)
          super(name, node_id)
        end
      end

      class VariableSymbol < Symbol
        getter declared_type : String?

        def initialize(name : String, node_id : ExprId, declared_type : String? = nil)
          super(name, node_id)
          @declared_type = declared_type
        end
      end

      # Instance variable symbol (e.g., @value inside a class)
      class InstanceVarSymbol < Symbol
        getter declared_type : String?

        def initialize(name : String, node_id : ExprId, declared_type : String? = nil, file_path : String? = nil)
          super(name, node_id, file_path)
          @declared_type = declared_type
        end
      end

      # Class variable symbol (e.g., @@counter)
      class ClassVarSymbol < Symbol
        getter declared_type : String?

        def initialize(name : String, node_id : ExprId, declared_type : String? = nil, file_path : String? = nil)
          super(name, node_id, file_path)
          @declared_type = declared_type
        end
      end

      # Global variable symbol (e.g., $stdout)
      class GlobalVarSymbol < Symbol
        getter declared_type : String?

        def initialize(name : String, node_id : ExprId, declared_type : String? = nil, file_path : String? = nil)
          super(name, node_id, file_path)
          @declared_type = declared_type
        end
      end

      # Lightweight representation of an annotation as seen by the semantic
      # layer and macro expander. It stores the fully-qualified name and
      # direct references to argument expressions in the frontend arena so
      # that later phases can interpret them as needed.
      struct AnnotationInfo
        getter full_name : String
        getter args : Array(ExprId)
        getter named_args : Hash(String, ExprId)

        def initialize(@full_name : String, @args : Array(ExprId), @named_args : Hash(String, ExprId))
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

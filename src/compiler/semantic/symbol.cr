require "../frontend/ast"

module CrystalV2
  module Compiler
    module Semantic
      alias ExprId = Frontend::ExprId

      abstract class Symbol
        getter name : String
        property node_id : ExprId
        getter file_path : String?
        getter generated_origin_node_id : ExprId?
        getter generated_macro_definition_node_id : ExprId?
        getter? direct_declaration_origin : Bool
        getter? generated_declaration_origin : Bool

        def initialize(@name : String, @node_id : ExprId, file_path : String? = nil)
          @file_path = file_path
          @generated_origin_node_id = nil
          @generated_macro_definition_node_id = nil
          @direct_declaration_origin = false
          @generated_declaration_origin = false
        end

        def file_path=(value : String?)
          @file_path = value
        end

        def generated_origin_node_id=(value : ExprId?)
          @generated_origin_node_id = value
        end

        def generated_macro_definition_node_id=(value : ExprId?)
          @generated_macro_definition_node_id = value
        end

        def generated? : Bool
          @generated_declaration_origin
        end

        def mark_direct_declaration_origin : Nil
          @direct_declaration_origin = true
        end

        def mark_generated_declaration_origin : Nil
          @generated_declaration_origin = true
        end

        def merge_declaration_origins_from(other : Symbol) : Nil
          @direct_declaration_origin ||= other.direct_declaration_origin?
          @generated_declaration_origin ||= other.generated_declaration_origin?
        end
      end

      class MacroSymbol < Symbol
        getter body : ExprId
        getter params : Array(String)?
        getter param_decls : Array(Frontend::MacroDefNode::MacroParamDecl)?

        def initialize(
          name : String,
          node_id : ExprId,
          @body : ExprId,
          @params : Array(String)? = nil,
          @param_decls : Array(Frontend::MacroDefNode::MacroParamDecl)? = nil
        )
          super(name, node_id)
        end
      end

      class MethodSymbol < Symbol
        getter params : Array(Frontend::Parameter)
        getter return_annotation : String?
        getter scope : SymbolTable
        getter type_parameters : Array(String)? # Week 1 Day 2: Generic method type params ["T", "U"]
        property? is_class_method : Bool = false # def self.* vs def *

        def initialize(name : String, node_id : ExprId, *, params : Array(Frontend::Parameter) = [] of Frontend::Parameter, return_annotation : String? = nil, scope : SymbolTable, type_parameters : Array(String)? = nil, is_class_method : Bool = false)
          super(name, node_id)
          @params = params
          @return_annotation = return_annotation
          @scope = scope
          @type_parameters = type_parameters
          @is_class_method = is_class_method
        end
      end

      class ClassSymbol < Symbol
        getter scope : SymbolTable
        getter class_scope : SymbolTable
        getter superclass_name : String?
        getter instance_var_infos : Hash(String, InstanceVarInfo) # name → full metadata
        getter type_parameters : Array(String)?      # Week 1: Generic type params ["T", "U"]
        # Collected annotations attached to this class (e.g., @[JSON::Serializable::Options])
        getter annotations : Array(AnnotationInfo)
        # Collected annotations per instance variable name (without leading "@")
        getter ivar_annotations : Hash(String, Array(AnnotationInfo))
        # True if this is a struct (value type) rather than a class (reference type)
        getter? is_struct : Bool
        # True if this class is abstract (cannot be instantiated directly)
        getter? is_abstract : Bool
        getter? explicit_superclass : Bool

        def initialize(name : String, node_id : ExprId, *, scope : SymbolTable, class_scope : SymbolTable, superclass_name : String? = nil, type_parameters : Array(String)? = nil, is_struct : Bool = false, is_abstract : Bool = false, explicit_superclass : Bool = false)
          super(name, node_id)
          @scope = scope
          @class_scope = class_scope
          @superclass_name = superclass_name
          @instance_var_infos = {} of String => InstanceVarInfo
          @type_parameters = type_parameters
          @annotations = [] of AnnotationInfo
          @ivar_annotations = {} of String => Array(AnnotationInfo)
          @is_struct = is_struct
          @is_abstract = is_abstract
          @explicit_superclass = explicit_superclass
        end

        # Phase 5A: Track instance variable declarations with full metadata
        def add_instance_var(name : String, type_annotation : String? = nil, default_value : ExprId? = nil, has_default : Bool = false)
          @instance_var_infos[name] = InstanceVarInfo.new(name, type_annotation, default_value, has_default)
        end

        def get_instance_var_type(name : String) : String?
          @instance_var_infos[name]?.try(&.type_annotation)
        end

        # Legacy accessor for backwards compatibility (returns name → type hash)
        def instance_vars : Hash(String, String?)
          result = {} of String => String?
          @instance_var_infos.each { |name, info| result[name] = info.type_annotation }
          result
        end

        # Get full instance var info
        def get_instance_var_info(name : String) : InstanceVarInfo?
          @instance_var_infos[name]?
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

        def merge_semantic_metadata_from(other : ClassSymbol) : Nil
          other.instance_var_infos.each_value do |info|
            existing = @instance_var_infos[info.name]?
            next if existing && !prefer_merged_instance_var_info?(existing, info)

            @instance_var_infos[info.name] = info
          end

          other.annotations.each do |entry|
            @annotations << entry unless @annotations.includes?(entry)
          end

          other.ivar_annotations.each do |name, entries|
            list = @ivar_annotations[name]?
            unless list
              list = [] of AnnotationInfo
              @ivar_annotations[name] = list
            end
            entries.each do |entry|
              list << entry unless list.includes?(entry)
            end
          end
        end

        private def prefer_merged_instance_var_info?(existing : InstanceVarInfo, candidate : InstanceVarInfo) : Bool
          return true if existing.type_annotation.nil? && !candidate.type_annotation.nil?
          return true if !existing.has_default? && candidate.has_default?
          false
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
        getter instance_includers : Array(Symbol)
        property type_parameters : Array(String)?

        def initialize(name : String, node_id : ExprId, *, scope : SymbolTable, type_parameters : Array(String)? = nil)
          super(name, node_id)
          @scope = scope
          @instance_includers = [] of Symbol
          @type_parameters = type_parameters
        end

        def register_instance_includer(symbol : Symbol) : Nil
          unless @instance_includers.includes?(symbol)
            @instance_includers << symbol
          end
        end
      end

      # Phase 102: Enum symbol with member name → value mapping
      class EnumSymbol < Symbol
        getter scope : SymbolTable                  # Scope for methods defined in enum
        getter members : Hash(String, Int64)        # Member name → integer value
        getter base_type : String                   # Base type (Int32, etc.)
        getter annotations : Array(AnnotationInfo)  # Attached enum annotations (for example @[Flags])

        def initialize(name : String, node_id : ExprId, *, scope : SymbolTable, members : Hash(String, Int64) = {} of String => Int64, base_type : String = "Int32")
          super(name, node_id)
          @scope = scope
          @members = members
          @base_type = base_type
          @annotations = [] of AnnotationInfo
        end

        def add_annotation(annotation_info : AnnotationInfo)
          @annotations << annotation_info
        end

        def flags? : Bool
          @annotations.any? do |info|
            info.full_name == "Flags" || info.full_name.ends_with?("::Flags")
          end
        end
      end

      class ConstantSymbol < Symbol
        getter value : ExprId

        def initialize(name : String, node_id : ExprId, @value : ExprId)
          super(name, node_id)
        end
      end

      class AliasSymbol < Symbol
        getter target : String

        def initialize(name : String, node_id : ExprId, @target : String)
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
        getter default_value : ExprId?
        getter? has_default : Bool

        def initialize(
          name : String,
          node_id : ExprId,
          declared_type : String? = nil,
          @default_value : ExprId? = nil,
          @has_default : Bool = false,
          file_path : String? = nil
        )
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

      # Metadata for instance variables, used by @type.instance_vars in macros.
      # Provides access to ivar.name, ivar.type, ivar.has_default_value?, etc.
      struct InstanceVarInfo
        getter name : String
        getter type_annotation : String?
        getter default_value : ExprId?
        getter? has_default : Bool

        def initialize(@name : String, @type_annotation : String? = nil, @default_value : ExprId? = nil, @has_default : Bool = false)
        end

        # Is the type nilable? (ends with ? or is Nil or union containing Nil)
        def nilable? : Bool
          return false unless type_annotation
          t = type_annotation.not_nil!
          t.ends_with?("?") || t == "Nil" || t.includes?("| Nil") || t.includes?("Nil |")
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

        def generated? : Bool
          super || @overloads.any?(&.generated?)
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

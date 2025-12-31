require "./taint_analysis"
require "./ast_to_hir"

module Crystal::HIR
  # Type info backed by AstToHir class metadata.
  class ClassInfoTypeProvider
    include TypeInfoProvider

    @hir_module : Module
    @class_info : Hash(String, ClassInfo)
    @acyclic_types : Set(String)
    @type_name_cache : Hash(TypeRef, String)

    def initialize(
      @hir_module : Module,
      @class_info : Hash(String, ClassInfo),
      @acyclic_types : Set(String) = Set(String).new
    )
      @type_name_cache = {} of TypeRef => String
    end

    def class_names : Array(String)
      @class_info.keys
    end

    def instance_var_types(class_name : String) : Hash(String, String)
      info = @class_info[class_name]?
      return {} of String => String unless info
      result = {} of String => String
      info.ivars.each do |ivar|
        result[ivar.name] = type_name_for(ivar.type) || "Unknown"
      end
      result
    end

    def acyclic_type_names : Set(String)
      @acyclic_types
    end

    def type_name_for(type_ref : TypeRef) : String?
      @type_name_cache[type_ref]? || begin
        name = primitive_type_name(type_ref) || descriptor_type_name(type_ref)
        @type_name_cache[type_ref] = name if name
        name
      end
    end

    def type_kind_for(type_ref : TypeRef) : TypeKind?
      case type_ref
      when TypeRef::VOID, TypeRef::NIL, TypeRef::BOOL,
           TypeRef::INT8, TypeRef::INT16, TypeRef::INT32, TypeRef::INT64, TypeRef::INT128,
           TypeRef::UINT8, TypeRef::UINT16, TypeRef::UINT32, TypeRef::UINT64, TypeRef::UINT128,
           TypeRef::FLOAT32, TypeRef::FLOAT64, TypeRef::CHAR, TypeRef::SYMBOL
        TypeKind::Primitive
      when TypeRef::POINTER
        TypeKind::Pointer
      else
        @hir_module.get_type_descriptor(type_ref).try(&.kind)
      end
    end

    private def primitive_type_name(type_ref : TypeRef) : String?
      case type_ref
      when TypeRef::VOID    then "Void"
      when TypeRef::NIL     then "Nil"
      when TypeRef::BOOL    then "Bool"
      when TypeRef::INT8    then "Int8"
      when TypeRef::INT16   then "Int16"
      when TypeRef::INT32   then "Int32"
      when TypeRef::INT64   then "Int64"
      when TypeRef::INT128  then "Int128"
      when TypeRef::UINT8   then "UInt8"
      when TypeRef::UINT16  then "UInt16"
      when TypeRef::UINT32  then "UInt32"
      when TypeRef::UINT64  then "UInt64"
      when TypeRef::UINT128 then "UInt128"
      when TypeRef::FLOAT32 then "Float32"
      when TypeRef::FLOAT64 then "Float64"
      when TypeRef::CHAR    then "Char"
      when TypeRef::STRING  then "String"
      when TypeRef::SYMBOL  then "Symbol"
      when TypeRef::POINTER then "Pointer"
      else
        nil
      end
    end

    private def descriptor_type_name(type_ref : TypeRef) : String?
      desc = @hir_module.get_type_descriptor(type_ref)
      return nil unless desc

      return desc.name if desc.type_params.empty?

      params = desc.type_params.map { |param| type_name_for(param) || "Unknown" }
      "#{desc.name}(#{params.join(", ")})"
    end
  end
end

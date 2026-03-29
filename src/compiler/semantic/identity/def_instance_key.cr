# Canonical cache key for typed method instances.
#
# DefInstanceKey identifies a unique instantiation of a method definition
# with specific semantic types at a call site. This is the V2 equivalent
# of original Crystal's DefInstanceKey in types.cr.
#
# Two calls to the same method with the same argument types get the same key.
# Different generic instantiations get different keys.
#
# Rules:
# - No mangled names in the key
# - No HIR::TypeRef in the key
# - No object_id as identity (use DefIdentity)
# - Keyed entirely by semantic identity

require "./semantic_type_id"
require "./def_identity"

module CrystalV2::Compiler::Semantic
  struct DefInstanceKey
    getter def_identity : DefIdentity
    getter receiver_type : SemanticTypeId?
    getter arg_types : Array(SemanticTypeId)
    getter block_type : SemanticTypeId?
    getter named_arg_types : Array({String, SemanticTypeId})?

    def initialize(
      @def_identity : DefIdentity,
      @receiver_type : SemanticTypeId? = nil,
      arg_types : Array(SemanticTypeId) = [] of SemanticTypeId,
      @block_type : SemanticTypeId? = nil,
      named_arg_types : Array({String, SemanticTypeId})? = nil
    )
      # Defensive copy: keys must be immutable once constructed.
      # Caller-owned arrays could be mutated later, invalidating hash/equality.
      @arg_types = arg_types.dup
      @named_arg_types = named_arg_types.try(&.dup)
    end

    def ==(other : DefInstanceKey) : Bool
      @def_identity == other.def_identity &&
        @receiver_type == other.receiver_type &&
        @arg_types == other.arg_types &&
        @block_type == other.block_type &&
        @named_arg_types == other.named_arg_types
    end

    def hash(hasher)
      hasher = @def_identity.hash(hasher)
      hasher = @receiver_type.hash(hasher)
      hasher = @arg_types.hash(hasher)
      hasher = @block_type.hash(hasher)
      hasher = @named_arg_types.hash(hasher)
      hasher
    end

    def to_s(io : IO) : Nil
      io << "DefInst(" << @def_identity
      if recv = @receiver_type
        io << " recv=" << recv
      end
      unless @arg_types.empty?
        io << " args=[" << @arg_types.join(", ") << "]"
      end
      if bt = @block_type
        io << " block=" << bt
      end
      io << ")"
    end
  end
end

# Canonical identity for a method definition (untyped Def AST node).
#
# DefIdentity uniquely identifies a syntactic method definition within
# one compilation. It is a structured type, NOT a hash or XOR encoding.
#
# Used as part of DefInstanceKey for semantic caching.
# Must NOT use mangled names, HIR TypeRef, or DefNode.object_id
# (object_id is implementation detail; DefIdentity is the contract).
# AstToHir's phase0_body_infer_expr_index lookaside cache keys by
# canonical-arena id + structural FNV mix of the DefNode, not by DefNode heap identity.
# Cache hits are verified with def_matches_phase0_body_infer_identity?; misses are not cached as -1.

module CrystalV2::Compiler::Semantic
  # Structured def identity — injective by construction.
  # Two fields uniquely identify a Def within one compilation:
  # - arena_id: object_id of the AstArena containing this Def
  # - expr_index: ExprId.index within that arena
  #
  # This pair is stable within one compilation and cannot collide.
  struct DefIdentity
    getter arena_id : UInt64
    getter expr_index : Int32

    def initialize(@arena_id : UInt64, @expr_index : Int32)
    end

    def ==(other : DefIdentity) : Bool
      @arena_id == other.arena_id && @expr_index == other.expr_index
    end

    def hash(hasher)
      hasher = @arena_id.hash(hasher)
      hasher = @expr_index.hash(hasher)
      hasher
    end

    def to_s(io : IO) : Nil
      io << "Def@" << @arena_id.to_s(16) << ":" << @expr_index
    end
  end
end

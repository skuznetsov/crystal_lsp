# Dry-run tracker for semantic identity layer.
#
# Runs as a side-channel during legacy compile path. Does NOT change
# any compilation behavior. Only observes and reports statistics about
# potential DefInstanceKey cache hits.
#
# Enable with: CRYSTAL_V2_IDENTITY_DRY_RUN=1

require "./semantic_type_id"
require "./def_identity"
require "./def_instance_key"

module CrystalV2::Compiler::Semantic
  class IdentityDryRunTracker
    getter type_intern : SemanticTypeInternTable
    getter total_lookups : Int32 = 0
    getter cache_hits : Int32 = 0
    getter cache_misses : Int32 = 0

    @seen_keys : ::Hash(DefInstanceKey, Int32)

    def initialize
      @type_intern = SemanticTypeInternTable.new
      @seen_keys = {} of DefInstanceKey => Int32
    end

    # Record a body inference attempt. Returns true if this is a cache hit
    # (same key seen before), false if first encounter.
    def record_inference(key : DefInstanceKey) : Bool
      @total_lookups += 1
      count = @seen_keys[key]? || 0
      @seen_keys[key] = count + 1
      if count > 0
        @cache_hits += 1
        true
      else
        @cache_misses += 1
        false
      end
    end

    # Build a DefIdentity from arena + DefNode ExprId
    def def_identity(arena : CrystalV2::Compiler::Frontend::ArenaLike,
                     expr_id : CrystalV2::Compiler::Frontend::ExprId) : DefIdentity
      DefIdentity.new(arena.object_id, expr_id.index)
    end

    # Intern a type name into a SemanticTypeId.
    # Simplified: maps type name string → semantic type.
    # In Phase 2+, this will use proper semantic type resolution.
    def intern_type_name(name : String, kind : TypeKind = TypeKind::Class) : SemanticTypeId
      return @type_intern.primitive("Nil") if name == "Nil" || name == "Void"
      return @type_intern.primitive("Bool") if name == "Bool"
      return @type_intern.primitive("Int32") if name == "Int32"
      return @type_intern.primitive("Int64") if name == "Int64"
      return @type_intern.primitive("UInt32") if name == "UInt32"
      return @type_intern.primitive("UInt64") if name == "UInt64"
      return @type_intern.primitive("Float64") if name == "Float64"
      return @type_intern.primitive("String") if name == "String"
      return @type_intern.primitive("Char") if name == "Char"

      # Check for generic: Array(Int32) → generic("Array", [intern("Int32")])
      if paren = name.index('(')
        base = name[0...paren]
        # For dry-run, treat the whole name as identity (Phase 2 will parse args)
        @type_intern.named(name, kind)
      else
        @type_intern.named(name, kind)
      end
    end

    def dump(io : IO) : Nil
      unique_keys = @seen_keys.size
      duplicate_keys = @seen_keys.count { |_, c| c > 1 }
      hit_rate = @total_lookups > 0 ? (@cache_hits * 100.0 / @total_lookups).round(1) : 0.0
      io.puts "[IDENTITY_DRY_RUN] lookups=#{@total_lookups} hits=#{@cache_hits} misses=#{@cache_misses} hit_rate=#{hit_rate}%"
      io.puts "[IDENTITY_DRY_RUN] unique_keys=#{unique_keys} duplicate_keys=#{duplicate_keys} interned_types=#{@type_intern.size}"
    end
  end
end

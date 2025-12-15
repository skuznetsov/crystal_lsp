# HIR → MIR Lowering
#
# Transforms High-Level IR (HIR) into Mid-Level IR (MIR):
# - Converts HIR values to MIR SSA values
# - Makes memory operations explicit (alloc, load, store)
# - Assigns memory strategy based on escape/taint analysis
# - Lowers closures to struct + function pointer
# - Inserts RC operations for ARC-managed allocations
#
# See docs/codegen_architecture.md Section 4 for specification.

require "./mir"
require "../hir/hir"
require "../hir/memory_strategy"

module Crystal
  module MIR
  # ═══════════════════════════════════════════════════════════════════════════
  # HIR TO MIR LOWERING
  # ═══════════════════════════════════════════════════════════════════════════

  class HIRToMIRLowering
    getter hir_module : HIR::Module
    getter mir_module : Module

    # Mapping from HIR ValueId to MIR ValueId per function
    @value_map : Hash(HIR::ValueId, ValueId)

    # Mapping from HIR BlockId to MIR BlockId
    @block_map : Hash(HIR::BlockId, BlockId)

    # Pending phi nodes that need incoming resolution after all blocks are lowered
    @pending_phis : Array(Tuple(Phi, HIR::Phi))

    # Current function being lowered
    @current_hir_func : HIR::Function?
    @current_mir_func : Function?
    @builder : Builder?

    # Memory strategy (note: we use inline selection, not global assigner)

    # Statistics
    getter stats : LoweringStats = LoweringStats.new

    def initialize(@hir_module : HIR::Module)
      @mir_module = Module.new(@hir_module.name)
      @value_map = {} of HIR::ValueId => ValueId
      @block_map = {} of HIR::BlockId => BlockId
      @pending_phis = [] of Tuple(Phi, HIR::Phi)
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Main Entry Point
    # ─────────────────────────────────────────────────────────────────────────

    def lower : Module
      # Two-pass approach for forward references:
      # Pass 1: Create all function stubs (for call resolution)
      @hir_module.functions.each do |hir_func|
        create_function_stub(hir_func)
      end

      # Pass 2: Lower function bodies
      @hir_module.functions.each do |hir_func|
        lower_function_body(hir_func)
      end

      @mir_module
    end

    # Register class variables as globals
    # Takes array of (global_name, hir_type, initial_value?)
    def register_globals(globals : Array(Tuple(String, HIR::TypeRef, Int64?)))
      globals.each do |global_name, hir_type, initial_value|
        mir_type = convert_type(hir_type)
        @mir_module.add_global(global_name, mir_type, initial_value)
      end
    end

    # Register union types from AstToHir
    # Creates union types in MIR TypeRegistry and stores descriptors for debug info
    def register_union_types(union_descriptors : Hash(MIR::TypeRef, UnionDescriptor))
      union_descriptors.each do |mir_type_ref, descriptor|
        # Register descriptor in MIR module (for debug info / LLVM metadata)
        @mir_module.register_union(mir_type_ref, descriptor)

        # Calculate union size and alignment
        max_variant_size = descriptor.variants.map(&.size).max? || 0
        alignment = descriptor.alignment.to_u32

        # Total size: 4 bytes for type_id + padding + max payload
        payload_offset = ((4 + alignment - 1) // alignment) * alignment
        total_size = payload_offset + max_variant_size

        # Create union type in TypeRegistry with the SAME TypeRef id
        # so that llvm_type lookup finds it
        union_type = @mir_module.type_registry.create_type_with_id(
          mir_type_ref.id,
          TypeKind::Union,
          descriptor.name,
          total_size.to_u64,
          alignment
        )

        # Add each variant as a sub-type
        descriptor.variants.each do |v|
          # Get or create variant type from TypeRegistry
          if variant_type = @mir_module.type_registry.get(v.type_ref)
            union_type.add_variant(variant_type)
          else
            # Variant is a primitive type - create temporary Type for it
            prim_type = Type.new(v.type_ref.id, TypeKind::Struct, v.full_name, v.size.to_u64, v.alignment.to_u32)
            union_type.add_variant(prim_type)
          end
        end
      end
    end

    # Register class/struct types with their fields
    # This allows LLVM backend to generate proper struct types
    def register_class_types(class_infos : Hash(String, Crystal::HIR::ClassInfo))
      class_infos.each do |class_name, info|
        # Convert HIR TypeRef to MIR TypeRef
        mir_type_ref = convert_type(info.type_ref)

        # Determine TypeKind (class = reference type, struct = value type)
        type_kind = info.is_struct ? TypeKind::Struct : TypeKind::Reference

        # Calculate total size (struct: just ivars, class: 8-byte header + ivars)
        total_size = info.size.to_u64

        # Create type in registry, or get existing type by name (for built-in types like String)
        # First check if a type with this name already exists (e.g., String as primitive)
        mir_type = @mir_module.type_registry.get_by_name(class_name)
        unless mir_type
          mir_type = @mir_module.type_registry.create_type_with_id(
            mir_type_ref.id,
            type_kind,
            class_name,
            total_size,
            8_u32  # alignment
          )
        end

        # Add fields (ivars)
        info.ivars.each do |ivar|
          field_type = convert_type(ivar.type)
          mir_type.add_field(ivar.name, field_type, ivar.offset.to_u32)
        end
      end
    end

    # Create function stub with params and return type (no body)
    private def create_function_stub(hir_func : HIR::Function)
      mir_func = @mir_module.create_function(
        hir_func.name,
        convert_type(hir_func.return_type)
      )

      # Add parameter types (needed for call site type checking)
      hir_func.params.each do |param|
        mir_func.add_param(param.name, convert_type(param.type))
      end
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Function Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_function_body(hir_func : HIR::Function)
      # Skip functions that contain yield - they are always inlined
      # and should not be generated as standalone functions
      if function_contains_yield?(hir_func)
        # Remove the stub from module since this function is inline-only
        @mir_module.remove_function(hir_func.name)
        return
      end

      # Get the pre-created function stub
      mir_func = @mir_module.get_function(hir_func.name).not_nil!

      @current_hir_func = hir_func
      @current_mir_func = mir_func
      @value_map.clear
      @block_map.clear
      @pending_phis.clear
      @builder = Builder.new(mir_func)

      # Map HIR params to MIR params (already added in stub)
      hir_func.params.each_with_index do |param, idx|
        # MIR params are value IDs starting from 0
        @value_map[param.id] = idx.to_u32
      end

      # Create all blocks first (for forward references)
      hir_func.blocks.each do |hir_block|
        mir_block_id = mir_func.create_block
        @block_map[hir_block.id] = mir_block_id
      end

      # Fix entry block mapping
      @block_map[hir_func.entry_block] = mir_func.entry_block

      # Lower each block (phi incoming resolution is deferred)
      hir_func.blocks.each do |hir_block|
        lower_block(hir_block)
      end

      # Now resolve all phi incoming values (after all blocks are lowered)
      resolve_pending_phis

      # Compute predecessors for phi resolution
      mir_func.compute_predecessors

      @stats.functions_lowered += 1
    end

    # Resolve phi incoming values after all blocks are lowered
    private def resolve_pending_phis
      @pending_phis.each do |(mir_phi, hir_phi)|
        hir_phi.incoming.each do |(hir_block, hir_value)|
          mir_block = @block_map[hir_block]
          mir_value = get_value(hir_value)
          mir_phi.add_incoming(mir_block, mir_value)
        end
      end
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Block Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_block(hir_block : HIR::Block)
      builder = @builder.not_nil!
      mir_block_id = @block_map[hir_block.id]
      builder.current_block = mir_block_id

      # Lower each instruction
      hir_block.instructions.each do |inst|
        lower_value(inst)
      end

      # Lower terminator
      lower_terminator(hir_block.terminator)

      @stats.blocks_lowered += 1
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Value Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_value(hir_value : HIR::Value)
      mir_id = case hir_value
               when HIR::Literal
                 lower_literal(hir_value)
               when HIR::Local
                 lower_local(hir_value)
               when HIR::Parameter
                 # Function parameters are pre-mapped, block parameters need allocation
                 if existing = @value_map[hir_value.id]?
                   existing
                 else
                   # Block parameter - allocate stack slot for it
                   builder = @builder.not_nil!
                   param_type = convert_type(hir_value.type)
                   alloc = MIR::Alloc.new(builder.next_id, param_type, MIR::MemoryStrategy::Stack, param_type, 0_u64, 8_u32)
                   builder.emit(alloc)
                   @value_map[hir_value.id] = alloc.id
                   alloc.id
                 end
               when HIR::Allocate
                 lower_allocate(hir_value)
               when HIR::FieldGet
                 lower_field_get(hir_value)
               when HIR::FieldSet
                 lower_field_set(hir_value)
               when HIR::IndexGet
                 lower_index_get(hir_value)
               when HIR::IndexSet
                 lower_index_set(hir_value)
               when HIR::Call
                 lower_call(hir_value)
               when HIR::ExternCall
                 lower_hir_extern_call(hir_value)
               when Crystal::HIR::BinaryOperation
                 lower_binary_op(hir_value)
               when Crystal::HIR::UnaryOperation
                 lower_unary_op(hir_value)
               when HIR::Cast
                 lower_cast(hir_value)
               when HIR::IsA
                 lower_is_a(hir_value)
               when HIR::Phi
                 lower_phi(hir_value)
               when HIR::Copy
                 lower_copy(hir_value)
               when HIR::MakeClosure
                 lower_closure(hir_value)
               when HIR::Yield
                 lower_yield(hir_value)
               when HIR::ClassVarGet
                 lower_classvar_get(hir_value)
               when HIR::ClassVarSet
                 lower_classvar_set(hir_value)
               when HIR::UnionWrap
                 lower_union_wrap(hir_value)
               when HIR::UnionUnwrap
                 lower_union_unwrap(hir_value)
               when HIR::UnionTypeId
                 lower_union_type_id(hir_value)
               when HIR::UnionIs
                 lower_union_is(hir_value)
               when HIR::ArrayLiteral
                 lower_array_literal(hir_value)
               when HIR::ArraySize
                 lower_array_size(hir_value)
               when HIR::StringInterpolation
                 lower_string_interpolation(hir_value)
               when HIR::Raise
                 lower_raise(hir_value)
               when HIR::GetException
                 lower_get_exception(hir_value)
               when HIR::TryBegin
                 lower_try_begin(hir_value)
               when HIR::TryEnd
                 lower_try_end(hir_value)
               when HIR::PointerMalloc
                 lower_pointer_malloc(hir_value)
               when HIR::PointerLoad
                 lower_pointer_load(hir_value)
               when HIR::PointerStore
                 lower_pointer_store(hir_value)
               when HIR::PointerAdd
                 lower_pointer_add(hir_value)
               when HIR::PointerRealloc
                 lower_pointer_realloc(hir_value)
               when HIR::AddressOf
                 lower_address_of(hir_value)
               else
                 raise "Unsupported HIR value: #{hir_value.class}"
               end

      @value_map[hir_value.id] = mir_id if mir_id
      @stats.values_lowered += 1
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Literal Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_literal(lit : HIR::Literal) : ValueId
      builder = @builder.not_nil!
      case v = lit.value
      when Int64
        builder.const_int(v, convert_type(lit.type))
      when UInt64
        builder.const_uint(v, convert_type(lit.type))
      when Float64
        builder.const_float(v, convert_type(lit.type))
      when Bool
        builder.const_bool(v)
      when String
        builder.const_string(v)
      when Char
        # Char as i32
        builder.const_int(v.ord.to_i64, TypeRef::CHAR)
      when Nil
        # Preserve original type for nil pointers
        const_type = convert_type(lit.type)
        const_type = TypeRef::NIL if const_type == TypeRef::VOID
        builder.const_nil_typed(const_type)
      else
        builder.const_nil
      end
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Local Variable Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_local(local : HIR::Local) : ValueId
      # In SSA, locals are represented as values
      # If mutable, we allocate on stack and use load/store
      builder = @builder.not_nil!

      if local.mutable
        # Allocate space on stack
        ptr = builder.alloc(MemoryStrategy::Stack, convert_type(local.type))
        @stats.stack_allocations += 1
        ptr
      else
        # Immutable locals are just values - return placeholder
        # (actual value will come from assignment)
        builder.const_nil  # Placeholder, will be replaced
      end
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Allocation Lowering (with Memory Strategy)
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_allocate(alloc : HIR::Allocate) : ValueId
      builder = @builder.not_nil!

      # Determine memory strategy based on escape/taint analysis
      strategy = select_memory_strategy(alloc)

      # Get the MIR type reference and look up size from type registry
      mir_type_ref = convert_type(alloc.type)
      alloc_size = 8_u64  # Default pointer size
      if mir_type = @mir_module.type_registry.get(mir_type_ref)
        alloc_size = mir_type.size
      end

      # Create allocation with proper size
      ptr = builder.alloc(strategy, mir_type_ref, alloc_size)

      # Insert RC increment for ARC allocations
      case strategy
      when MemoryStrategy::ARC
        builder.rc_inc(ptr)
        @stats.arc_allocations += 1
      when MemoryStrategy::AtomicARC
        builder.rc_inc(ptr, atomic: true)
        @stats.arc_allocations += 1
      when MemoryStrategy::Stack
        @stats.stack_allocations += 1
      when MemoryStrategy::Slab
        @stats.slab_allocations += 1
      when MemoryStrategy::GC
        @stats.gc_allocations += 1
      end

      ptr
    end

    private def select_memory_strategy(alloc : HIR::Allocate) : MemoryStrategy
      # If HIR already carries a chosen strategy, honor it.
      if strat = alloc.memory_strategy
        return map_hir_strategy(strat)
      end

      # Struct (value type) always uses stack allocation
      if alloc.is_value_type
        return MemoryStrategy::Stack
      end

      # Determine strategy based on lifetime and taints
      lifetime = alloc.lifetime
      taints = alloc.taints

      # Cyclic types must use GC
      if taints.cyclic?
        return MemoryStrategy::GC
      end

      # Thread-shared requires atomic operations
      if taints.thread_shared?
        return MemoryStrategy::AtomicARC
      end

      # FFI-exposed uses GC for safety
      if taints.ffi_exposed?
        return MemoryStrategy::GC
      end

      # Strategy based on lifetime
      case lifetime
      when HIR::LifetimeTag::StackLocal
        MemoryStrategy::Stack
      when HIR::LifetimeTag::ArgEscape
        MemoryStrategy::Slab
      when HIR::LifetimeTag::HeapEscape
        MemoryStrategy::ARC
      when HIR::LifetimeTag::GlobalEscape
        MemoryStrategy::AtomicARC
      else
        # Unknown/conservative: GC
        MemoryStrategy::GC
      end
    end

    private def map_hir_strategy(strat : HIR::MemoryStrategy) : MemoryStrategy
      case strat
      when HIR::MemoryStrategy::Stack then MemoryStrategy::Stack
      when HIR::MemoryStrategy::Slab then MemoryStrategy::Slab
      when HIR::MemoryStrategy::ARC then MemoryStrategy::ARC
      when HIR::MemoryStrategy::AtomicARC then MemoryStrategy::AtomicARC
      when HIR::MemoryStrategy::GC then MemoryStrategy::GC
      else
        MemoryStrategy::GC
      end
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Field Access Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_field_get(field : HIR::FieldGet) : ValueId
      builder = @builder.not_nil!
      obj_ptr = get_value(field.object)

      # GEP to field address + load
      # field_offset is byte offset from object start
      field_ptr = builder.gep(obj_ptr, [field.field_offset.to_u32], TypeRef::POINTER)
      builder.load(field_ptr, convert_type(field.type))
    end

    private def lower_field_set(field : HIR::FieldSet) : ValueId
      builder = @builder.not_nil!
      obj_ptr = get_value(field.object)
      value = get_value(field.value)

      # GEP to field address + store
      field_ptr = builder.gep(obj_ptr, [field.field_offset.to_u32], TypeRef::POINTER)
      builder.store(field_ptr, value)
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Index Access Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_index_get(idx : HIR::IndexGet) : ValueId
      builder = @builder.not_nil!
      obj_ptr = get_value(idx.object)
      index = get_value(idx.index)

      # Get element type from context (default to INT32 for now)
      element_type = convert_type(idx.type)
      if element_type.id == MIR::TypeRef::VOID.id
        element_type = MIR::TypeRef::INT32
      end

      # Emit ArrayGet instruction
      arr_get = MIR::ArrayGet.new(
        builder.next_id,
        element_type,
        obj_ptr,
        index
      )
      builder.emit(arr_get)
    end

    private def lower_index_set(idx : HIR::IndexSet) : ValueId
      builder = @builder.not_nil!
      obj_ptr = get_value(idx.object)
      index = get_value(idx.index)
      value = get_value(idx.value)

      # Get element type - use type from HIR IndexSet.type or default to INT32
      element_type = convert_type(idx.type)
      if element_type.id == MIR::TypeRef::VOID.id
        element_type = MIR::TypeRef::INT32
      end

      # Emit ArraySet instruction
      arr_set = MIR::ArraySet.new(
        builder.next_id,
        element_type,
        obj_ptr,
        index,
        value
      )
      builder.emit(arr_set)
      arr_set.id
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Call Lowering
    # ─────────────────────────────────────────────────────────────────────────

    # Lower HIR::ExternCall to MIR::ExternCall (direct C function call)
    private def lower_hir_extern_call(extern_call : HIR::ExternCall) : ValueId
      builder = @builder.not_nil!

      # Get arguments
      args = extern_call.args.map { |arg| get_value(arg) }

      # Emit MIR extern_call with the real C function name
      builder.extern_call(extern_call.extern_name, args, convert_type(extern_call.type))
    end

    private def lower_call(call : HIR::Call) : ValueId
      builder = @builder.not_nil!

      # Get arguments
      args = call.args.map { |arg| get_value(arg) }

      # Add receiver as first arg if present
      if recv = call.receiver
        args.unshift(get_value(recv))
      end

      # Check if this is an external/runtime call
      if call.method_name.starts_with?("__crystal_v2_")
        return builder.extern_call(call.method_name, args, convert_type(call.type))
      end

      # Handle built-in print functions
      # Method name may be mangled as "puts$Int32" or "puts:Int32" etc, so extract base name
      # We use $ as separator (: is not valid in LLVM identifiers)
      base_method_name = call.method_name.split(/[:\$]/).first
      if base_method_name == "puts"
        # Determine the actual extern based on argument type
        if args.size == 1
          arg_type = get_arg_type(call.args[0])
          extern_name = case arg_type
                        when TypeRef::INT32, TypeRef::UINT32, TypeRef::CHAR
                          "__crystal_v2_print_int32_ln"
                        when TypeRef::INT64, TypeRef::UINT64
                          "__crystal_v2_print_int64_ln"
                        when TypeRef::STRING, TypeRef::POINTER
                          "__crystal_v2_puts"
                        else
                          # Default to int32 for unknown numeric types
                          "__crystal_v2_print_int32_ln"
                        end
          return builder.extern_call(extern_name, args, TypeRef::VOID)
        end
      end

      # Handle print (without newline)
      if base_method_name == "print"
        if args.size == 1
          arg_type = get_arg_type(call.args[0])
          extern_name = case arg_type
                        when TypeRef::INT32, TypeRef::UINT32, TypeRef::CHAR
                          "__crystal_v2_print_int32"
                        when TypeRef::INT64, TypeRef::UINT64
                          "__crystal_v2_print_int64"
                        else
                          "__crystal_v2_print_int32"
                        end
          return builder.extern_call(extern_name, args, TypeRef::VOID)
        end
      end

      # Handle exit() - call libc exit
      if base_method_name == "exit" && args.size == 1
        return builder.extern_call("exit", args, TypeRef::VOID)
      end

      # Look up function by name - try exact match first, then fuzzy match
      func = @mir_module.get_function(call.method_name)

      # If not found, try fuzzy matching to handle type variations (e.g., String vs String | Nil)
      unless func
        # Only apply fuzzy matching for qualified method names (containing . or #)
        if call.method_name.includes?(".") || call.method_name.includes?("#")
          # Extract base name (before $ type suffix)
          base_name = call.method_name.split("$").first

          # Search for any function that starts with the base name
          func = @mir_module.functions.find do |f|
            f.name.split("$").first == base_name
          end
        end
      end

      callee_id = if func
                    func.id
                  else
                    # Unknown function - emit as extern call
                    return builder.extern_call(call.method_name, args, convert_type(call.type))
                  end

      # Coerce arguments to match parameter types (handle concrete type -> union coercion)
      if func
        if call.method_name.includes?("format_gutter")
          STDERR.puts "[MIR-COERCE] func=#{func.name} params=#{func.params.map(&.type.id).join(",")}"
        end
        coerced_args = coerce_call_args(builder, args, call.args, func)
        return builder.call(callee_id, coerced_args, convert_type(call.type))
      end

      builder.call(callee_id, args, convert_type(call.type))
    end

    # Helper to get the MIR type of a HIR value by finding it in the function
    private def get_arg_type(hir_id : HIR::ValueId) : TypeRef
      if hir_func = @current_hir_func
        # Search through all blocks for the value with this ID
        hir_func.blocks.each do |block|
          block.instructions.each do |inst|
            if inst.id == hir_id
              return convert_type(inst.type)
            end
          end
        end
        # Also check parameters
        hir_func.params.each_with_index do |param, idx|
          if idx.to_u32 == hir_id
            return convert_type(param.type)
          end
        end
      end
      TypeRef::INT32  # Default fallback
    end

    # Coerce call arguments to match function parameter types
    # This handles concrete type -> union type coercion (e.g., Int32 -> Int32 | Nil)
    private def coerce_call_args(
      builder : MIR::Builder,
      mir_args : Array(ValueId),
      hir_args : Array(HIR::ValueId),
      func : MIR::Function
    ) : Array(ValueId)
      params = func.params
      result = [] of ValueId

      mir_args.each_with_index do |mir_arg, idx|
        param = params[idx]?
        unless param
          # More args than params - pass through
          result << mir_arg
          next
        end

        arg_type = if idx < hir_args.size
                     get_arg_type(hir_args[idx])
                   else
                     TypeRef::INT32  # Fallback
                   end
        param_type = param.type

        # Check if coercion needed: different types and param is a union
        is_param_union = is_union_type?(param_type)
        if arg_type != param_type && is_param_union && !is_union_type?(arg_type)
          # Wrap concrete value in union type
          variant_id = get_union_variant_id(arg_type)
          STDERR.puts "[MIR-COERCE] Wrapping arg #{mir_arg} type #{arg_type.id} into union type #{param_type.id} (variant #{variant_id})"
          wrapped = builder.union_wrap(mir_arg, variant_id, param_type)
          result << wrapped
        else
          if func.name.includes?("format_gutter")
            STDERR.puts "[MIR-COERCE] No wrap: arg_type=#{arg_type.id} param_type=#{param_type.id} is_param_union=#{is_param_union}"
          end
          result << mir_arg
        end
      end

      result
    end

    # Check if a type is a union type based on its ID
    private def is_union_type?(type : TypeRef) : Bool
      # First check MIR module's union_descriptors (most authoritative)
      return true if @mir_module.union_descriptors.has_key?(type)

      # Fall back to checking HIR types
      if type.id >= HIR::TypeRef::FIRST_USER_TYPE
        @hir_module.types.each_with_index do |desc, idx|
          if HIR::TypeRef.new(HIR::TypeRef::FIRST_USER_TYPE + idx.to_u32) == type
            return desc.kind == HIR::TypeKind::Union || desc.name.includes?("___")
          end
        end
      end
      false
    end

    # Get the variant ID for a concrete type when wrapping into a union
    private def get_union_variant_id(concrete_type : TypeRef) : Int32
      # Convention: Nil is variant 1, other concrete types are variant 0
      if concrete_type == TypeRef::NIL
        1
      else
        0
      end
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Binary/Unary Operation Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_binary_op(binop : HIR::BinaryOperation) : ValueId
      builder = @builder.not_nil!
      left = get_value(binop.left)
      right = get_value(binop.right)
      result_type = convert_type(binop.type)

      case binop.op
      when HIR::BinaryOp::Add then builder.add(left, right, result_type)
      when HIR::BinaryOp::Sub then builder.sub(left, right, result_type)
      when HIR::BinaryOp::Mul then builder.mul(left, right, result_type)
      when HIR::BinaryOp::Div then builder.div(left, right, result_type)
      when HIR::BinaryOp::Mod then builder.rem(left, right, result_type)
      when HIR::BinaryOp::Eq  then builder.eq(left, right)
      when HIR::BinaryOp::Ne  then builder.ne(left, right)
      when HIR::BinaryOp::Lt  then builder.lt(left, right)
      when HIR::BinaryOp::Le  then builder.le(left, right)
      when HIR::BinaryOp::Gt  then builder.gt(left, right)
      when HIR::BinaryOp::Ge  then builder.ge(left, right)
      when HIR::BinaryOp::BitAnd then builder.bit_and(left, right, result_type)
      when HIR::BinaryOp::BitOr  then builder.bit_or(left, right, result_type)
      when HIR::BinaryOp::BitXor then builder.bit_xor(left, right, result_type)
      when HIR::BinaryOp::Shl then builder.shl(left, right, result_type)
      when HIR::BinaryOp::Shr then builder.shr(left, right, result_type)
      when HIR::BinaryOp::And
        # Logical AND
        builder.bit_and(left, right, TypeRef::BOOL)
      when HIR::BinaryOp::Or
        # Logical OR
        builder.bit_or(left, right, TypeRef::BOOL)
      else
        builder.const_nil  # Fallback
      end
    end

    private def lower_unary_op(unop : HIR::UnaryOperation) : ValueId
      builder = @builder.not_nil!
      operand = get_value(unop.operand)
      result_type = convert_type(unop.type)

      case unop.op
      when HIR::UnaryOp::Neg    then builder.neg(operand, result_type)
      when HIR::UnaryOp::Not    then builder.not(operand)
      when HIR::UnaryOp::BitNot then builder.bit_not(operand, result_type)
      else
        builder.const_nil
      end
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Cast Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_cast(cast : HIR::Cast) : ValueId
      builder = @builder.not_nil!
      value = get_value(cast.value)
      target = convert_type(cast.target_type)

      # Determine cast kind based on source/target types
      # For now, use bitcast as default
      builder.bitcast(value, target)
    end

    private def lower_is_a(isa : HIR::IsA) : ValueId
      builder = @builder.not_nil!
      # Type check would involve runtime type info
      # For now, return bool constant
      builder.const_bool(true)
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Phi Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_phi(hir_phi : HIR::Phi) : ValueId
      builder = @builder.not_nil!
      mir_phi = builder.phi(convert_type(hir_phi.type))

      # Defer incoming resolution until all blocks are lowered
      # This handles forward references from loop bodies
      @pending_phis << {mir_phi, hir_phi}

      mir_phi.id
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Copy/Assignment Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_copy(copy : HIR::Copy) : ValueId
      # In SSA, copy is just value forwarding
      get_value(copy.source)
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Closure Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_closure(closure : HIR::MakeClosure) : ValueId
      builder = @builder.not_nil!

      # Closures become:
      # 1. Struct containing captured variables
      # 2. Function pointer to closure body

      # Determine memory strategy based on taints:
      # - ThreadShared closure → AtomicARC (for thread-safe RC)
      # - Normal closure → ARC (non-atomic, faster)
      strategy = if closure.taints.thread_shared?
                   MemoryStrategy::AtomicARC
                 else
                   MemoryStrategy::ARC
                 end

      # Allocate environment struct
      env_ptr = builder.alloc(strategy, TypeRef::POINTER)

      # Insert RC increment based on strategy
      if strategy == MemoryStrategy::AtomicARC
        builder.rc_inc(env_ptr, atomic: true)
      else
        builder.rc_inc(env_ptr)
      end

      # Store captured values in environment
      closure.captures.each_with_index do |cap, idx|
        cap_value = get_value(cap.value_id)
        field_ptr = builder.gep(env_ptr, [idx.to_u32], TypeRef::POINTER)
        builder.store(field_ptr, cap_value)
      end

      @stats.closures_lowered += 1
      env_ptr
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Yield Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_yield(yld : HIR::Yield) : ValueId
      builder = @builder.not_nil!
      # Yield becomes indirect call through block parameter
      args = yld.args.map { |arg| get_value(arg) }
      # Block pointer would be passed as hidden parameter
      builder.const_nil  # Placeholder
    end

    # Check if a function contains yield instructions (inline-only function)
    private def function_contains_yield?(hir_func : HIR::Function) : Bool
      hir_func.blocks.each do |block|
        block.instructions.each do |inst|
          return true if inst.is_a?(HIR::Yield)
        end
      end
      false
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Class Variable Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_classvar_get(cv : HIR::ClassVarGet) : ValueId
      builder = @builder.not_nil!
      # Generate global name: ClassName_varname
      global_name = "#{cv.class_name}_#{cv.var_name}"
      builder.global_load(global_name, convert_type(cv.type))
    end

    private def lower_classvar_set(cv : HIR::ClassVarSet) : ValueId
      builder = @builder.not_nil!
      value = get_value(cv.value)
      # Generate global name: ClassName_varname
      global_name = "#{cv.class_name}_#{cv.var_name}"
      builder.global_store(global_name, value, convert_type(cv.type))
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Union Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_union_wrap(wrap : HIR::UnionWrap) : ValueId
      builder = @builder.not_nil!
      value = get_value(wrap.value)
      union_type = convert_type(wrap.type)
      variant_type_id = wrap.variant_type_id

      # Create MIR UnionWrap instruction
      mir_wrap = MIR::UnionWrap.new(
        builder.next_id,
        union_type,
        value,
        variant_type_id,
        union_type  # union_type parameter
      )
      builder.emit(mir_wrap)
    end

    private def lower_union_unwrap(unwrap : HIR::UnionUnwrap) : ValueId
      builder = @builder.not_nil!
      union_value = get_value(unwrap.union_value)
      result_type = convert_type(unwrap.type)

      # Create MIR UnionUnwrap instruction
      mir_unwrap = MIR::UnionUnwrap.new(
        builder.next_id,
        result_type,
        union_value,
        unwrap.variant_type_id,
        unwrap.safe
      )
      builder.emit(mir_unwrap)
    end

    private def lower_union_type_id(type_id : HIR::UnionTypeId) : ValueId
      builder = @builder.not_nil!
      union_value = get_value(type_id.union_value)

      # Create MIR UnionTypeIdGet instruction (type is hardcoded to INT32)
      mir_type_id = MIR::UnionTypeIdGet.new(
        builder.next_id,
        union_value
      )
      builder.emit(mir_type_id)
    end

    private def lower_union_is(is : HIR::UnionIs) : ValueId
      builder = @builder.not_nil!
      union_value = get_value(is.union_value)

      # Create MIR UnionIs instruction (type is hardcoded to BOOL)
      mir_is = MIR::UnionIs.new(
        builder.next_id,
        union_value,
        is.variant_type_id
      )
      builder.emit(mir_is)
    end

    private def lower_array_literal(arr : HIR::ArrayLiteral) : ValueId
      builder = @builder.not_nil!

      # Convert element values
      elements = arr.elements.map { |e| get_value(e) }
      element_type = convert_type(arr.element_type)

      # Create MIR ArrayLiteral instruction
      mir_arr = MIR::ArrayLiteral.new(
        builder.next_id,
        element_type,
        elements
      )
      builder.emit(mir_arr)
    end

    private def lower_array_size(arr_size : HIR::ArraySize) : ValueId
      builder = @builder.not_nil!
      array_val = get_value(arr_size.array_value)

      # Create MIR ArraySize instruction
      mir_size = MIR::ArraySize.new(
        builder.next_id,
        array_val
      )
      builder.emit(mir_size)
    end

    private def lower_string_interpolation(interp : HIR::StringInterpolation) : ValueId
      builder = @builder.not_nil!

      # Convert part values
      parts = interp.parts.map { |p| get_value(p) }

      # Create MIR StringInterpolation instruction
      mir_interp = MIR::StringInterpolation.new(
        builder.next_id,
        parts
      )
      builder.emit(mir_interp)
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Exception Handling
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_raise(raise_inst : HIR::Raise) : ValueId
      builder = @builder.not_nil!

      # Lower raise as a call to runtime raise function
      if exc = raise_inst.exception
        exc_val = get_value(exc)
        builder.extern_call("__crystal_v2_raise", [exc_val], TypeRef::VOID)
      elsif msg = raise_inst.message
        # Raise with message string
        msg_val = builder.const_string(msg)
        builder.extern_call("__crystal_v2_raise_msg", [msg_val], TypeRef::VOID)
      else
        # Re-raise current exception
        empty_args = Array(ValueId).new
        builder.extern_call("__crystal_v2_reraise", empty_args, TypeRef::VOID)
      end
    end

    private def lower_get_exception(get_exc : HIR::GetException) : ValueId
      builder = @builder.not_nil!

      # Get current exception from runtime
      empty_args = Array(ValueId).new
      builder.extern_call("__crystal_v2_get_exception", empty_args, TypeRef::POINTER)
    end

    private def lower_try_begin(try_begin : HIR::TryBegin) : ValueId
      builder = @builder.not_nil!

      # Emit MIR TryBegin which will emit inline setjmp in LLVM IR
      mir_try = TryBegin.new(builder.function.next_value_id)
      builder.emit(mir_try)
      mir_try.id
    end

    private def lower_try_end(try_end : HIR::TryEnd) : ValueId
      builder = @builder.not_nil!

      # Emit MIR TryEnd which will clear exception handler
      mir_try_end = TryEnd.new(builder.function.next_value_id)
      builder.emit(mir_try_end)
      mir_try_end.id
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Pointer Operations Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_pointer_malloc(malloc : HIR::PointerMalloc) : ValueId
      builder = @builder.not_nil!

      count = get_value(malloc.count)
      elem_size = type_size(malloc.element_type)

      # Compute total size = count * element_size
      size_const = builder.const_int(elem_size.to_i64, TypeRef::INT64)
      # Cast count to i64 if needed
      count_i64 = builder.cast(CastKind::SExt, count, TypeRef::INT64)
      total_size = builder.mul(count_i64, size_const, TypeRef::INT64)

      # Call malloc
      args = [total_size]
      builder.extern_call("__crystal_v2_malloc64", args, TypeRef::POINTER)
    end

    private def lower_pointer_load(load : HIR::PointerLoad) : ValueId
      builder = @builder.not_nil!

      ptr = get_value(load.pointer)
      result_type = convert_type(load.type)

      if idx = load.index
        # ptr[idx] - need GEP then load
        index = get_value(idx)
        elem_type = convert_type(load.type)
        gep = builder.gep_dynamic(ptr, index, elem_type)
        builder.load(gep, result_type)
      else
        # ptr.value - direct load
        builder.load(ptr, result_type)
      end
    end

    private def lower_pointer_store(store : HIR::PointerStore) : ValueId
      builder = @builder.not_nil!

      ptr = get_value(store.pointer)
      val = get_value(store.value)

      # Get the element type from the value being stored
      elem_type = get_arg_type(store.value)

      if idx = store.index
        # ptr[idx] = val - need GEP then store
        index = get_value(idx)
        gep = builder.gep_dynamic(ptr, index, elem_type)
        builder.store(gep, val)
      else
        # ptr.value = val - direct store
        builder.store(ptr, val)
      end

      val  # Return stored value
    end

    private def lower_pointer_add(add : HIR::PointerAdd) : ValueId
      builder = @builder.not_nil!

      ptr = get_value(add.pointer)
      offset = get_value(add.offset)
      elem_type = convert_type(add.element_type)

      # GEP with dynamic offset computes ptr + offset * sizeof(elem)
      builder.gep_dynamic(ptr, offset, elem_type)
    end

    private def lower_pointer_realloc(realloc : HIR::PointerRealloc) : ValueId
      builder = @builder.not_nil!

      ptr = get_value(realloc.pointer)
      new_size = get_value(realloc.new_size)

      args = [ptr, new_size]
      builder.extern_call("__crystal_v2_realloc64", args, TypeRef::POINTER)
    end

    private def lower_address_of(addr_of : HIR::AddressOf) : ValueId
      builder = @builder.not_nil!

      # Get the operand value
      operand = get_value(addr_of.operand)

      # For address-of, we need to return the address of the operand
      # In MIR, this is a pointer to the value's storage location
      # For now, emit an alloca and return its address
      builder.addressof(operand, TypeRef::POINTER)
    end

    # Get size of a type in bytes
    private def type_size(type : HIR::TypeRef) : Int32
      case type
      when HIR::TypeRef::VOID    then 0
      when HIR::TypeRef::BOOL    then 1
      when HIR::TypeRef::INT8    then 1
      when HIR::TypeRef::INT16   then 2
      when HIR::TypeRef::INT32   then 4
      when HIR::TypeRef::INT64   then 8
      when HIR::TypeRef::INT128  then 16
      when HIR::TypeRef::UINT8   then 1
      when HIR::TypeRef::UINT16  then 2
      when HIR::TypeRef::UINT32  then 4
      when HIR::TypeRef::UINT64  then 8
      when HIR::TypeRef::UINT128 then 16
      when HIR::TypeRef::FLOAT32 then 4
      when HIR::TypeRef::FLOAT64 then 8
      when HIR::TypeRef::CHAR    then 4
      when HIR::TypeRef::POINTER then 8
      else                            8  # Default pointer size for user types
      end
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Terminator Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_terminator(term : HIR::Terminator)
      builder = @builder.not_nil!

      case term
      when HIR::Return
        if v = term.value
          builder.ret(get_value(v))
        else
          builder.ret
        end
      when HIR::Branch
        cond = get_value(term.condition)
        then_block = @block_map[term.then_block]
        else_block = @block_map[term.else_block]
        builder.branch(cond, then_block, else_block)
      when HIR::Jump
        target = @block_map[term.target]
        builder.jump(target)
      when HIR::Switch
        value = get_value(term.value)
        cases = term.cases.map do |(val_id, block_id)|
          val = get_value(val_id)
          mir_block = @block_map[block_id]
          {0_i64, mir_block}  # Would need to extract actual constant value
        end
        default_block = @block_map[term.default]
        builder.switch(value, cases, default_block)
      when HIR::Unreachable
        builder.unreachable
      end
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Helpers
    # ─────────────────────────────────────────────────────────────────────────

    private def get_value(hir_id : HIR::ValueId) : ValueId
      @value_map[hir_id]? || 0_u32
    end

    private def convert_type(hir_type : HIR::TypeRef) : TypeRef
      # Map HIR type IDs to MIR type IDs
      # Primitives have same IDs in both
      case hir_type
      when HIR::TypeRef::VOID    then TypeRef::VOID
      when HIR::TypeRef::BOOL    then TypeRef::BOOL
      when HIR::TypeRef::INT8    then TypeRef::INT8
      when HIR::TypeRef::INT16   then TypeRef::INT16
      when HIR::TypeRef::INT32   then TypeRef::INT32
      when HIR::TypeRef::INT64   then TypeRef::INT64
      when HIR::TypeRef::INT128  then TypeRef::INT128
      when HIR::TypeRef::UINT8   then TypeRef::UINT8
      when HIR::TypeRef::UINT16  then TypeRef::UINT16
      when HIR::TypeRef::UINT32  then TypeRef::UINT32
      when HIR::TypeRef::UINT64  then TypeRef::UINT64
      when HIR::TypeRef::UINT128 then TypeRef::UINT128
      when HIR::TypeRef::FLOAT32 then TypeRef::FLOAT32
      when HIR::TypeRef::FLOAT64 then TypeRef::FLOAT64
      when HIR::TypeRef::CHAR    then TypeRef::CHAR
      when HIR::TypeRef::STRING  then TypeRef::STRING
      when HIR::TypeRef::NIL     then TypeRef::NIL
      when HIR::TypeRef::SYMBOL  then TypeRef::SYMBOL
      else
        # User-defined types: offset by primitive count
        TypeRef.new(hir_type.id + 20_u32)
      end
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # LOWERING STATISTICS
  # ═══════════════════════════════════════════════════════════════════════════

  class LoweringStats
    property functions_lowered : Int32 = 0
    property blocks_lowered : Int32 = 0
    property values_lowered : Int32 = 0
    property closures_lowered : Int32 = 0

    # Memory strategy counts
    property stack_allocations : Int32 = 0
    property slab_allocations : Int32 = 0
    property arc_allocations : Int32 = 0
    property gc_allocations : Int32 = 0

    def total_allocations : Int32
      stack_allocations + slab_allocations + arc_allocations + gc_allocations
    end

    def to_s(io : IO) : Nil
      io << "Lowering Statistics:\n"
      io << "  Functions: " << functions_lowered << "\n"
      io << "  Blocks: " << blocks_lowered << "\n"
      io << "  Values: " << values_lowered << "\n"
      io << "  Closures: " << closures_lowered << "\n"
      io << "  Memory allocations:\n"
      io << "    Stack: " << stack_allocations << "\n"
      io << "    Slab: " << slab_allocations << "\n"
      io << "    ARC: " << arc_allocations << "\n"
      io << "    GC: " << gc_allocations << "\n"
      io << "    Total: " << total_allocations << "\n"
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # CONVENIENCE METHOD ON HIR MODULE
  # ═══════════════════════════════════════════════════════════════════════════

  end  # module MIR

  class HIR::Module
    def lower_to_mir : MIR::Module
      lowering = MIR::HIRToMIRLowering.new(self)
      lowering.lower
    end
  end
end  # module Crystal

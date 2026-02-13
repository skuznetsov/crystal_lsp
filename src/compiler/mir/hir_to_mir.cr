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

    # Mapping from HIR ValueId to HIR TypeRef per function
    # (needed to lower HIR::Cast into correct MIR::CastKind)
    @hir_value_types : Hash(HIR::ValueId, HIR::TypeRef)

    # Mapping from HIR BlockId to MIR BlockId
    @block_map : Hash(HIR::BlockId, BlockId)

    # Pending phi nodes that need incoming resolution after all blocks are lowered
    @pending_phis : Array(Tuple(Phi, HIR::Phi))

    # Stack slots (mutable locals / block params) that require loads on reads
    @stack_slot_values : Set(ValueId)
    @stack_slot_types : Hash(ValueId, TypeRef)

    # Current function being lowered
    @current_hir_func : HIR::Function?
    @current_mir_func : Function?
    @builder : Builder?
    @current_lowering_func_name : String = ""
    @slab_frame_enabled : Bool
    @current_slab_frame : Bool = false
    @current_block_param_id : HIR::ValueId?
    @class_children : Hash(String, Array(String))

    # Index: base_name (before "$") → first matching MIR function.
    # Eliminates O(N) linear scans during fuzzy call resolution.
    @function_by_base_name : Hash(String, Function) = {} of String => Function

    # Index: class_name → Array of functions belonging to that class.
    # Eliminates O(N) full-scan of all functions during virtual dispatch.
    @functions_by_class : Hash(String, Array(Function)) = {} of String => Array(Function)

    # Caches for virtual dispatch (avoid repeated hierarchy traversals)
    @subclass_cache : Hash(String, Array(String)) = {} of String => Array(String)
    @module_includers_cache : Hash(String, Array(String)) = {} of String => Array(String)
    @resolve_virtual_cache : Hash({String, String, Int32?, Bool}, Function?) = {} of {String, String, Int32?, Bool} => Function?

    # Memory strategy (note: we use inline selection, not global assigner)

    # Track HIR values that point to inline struct data (from Pointer(Struct).value).
    # These need special FieldGet handling: GEP without load for struct-typed fields.
    @inline_struct_ptrs : Set(HIR::ValueId) = Set(HIR::ValueId).new

    # Statistics
    getter stats : LoweringStats = LoweringStats.new

    def initialize(@hir_module : HIR::Module, *, slab_frame : Bool = false)
      @mir_module = Module.new(@hir_module.name)
      @value_map = {} of HIR::ValueId => ValueId
      @hir_value_types = {} of HIR::ValueId => HIR::TypeRef
      @block_map = {} of HIR::BlockId => BlockId
      @pending_phis = [] of Tuple(Phi, HIR::Phi)
      @stack_slot_values = Set(ValueId).new
      @stack_slot_types = {} of ValueId => TypeRef
      @slab_frame_enabled = slab_frame
      @current_block_param_id = nil
      @class_children = {} of String => Array(String)
      @hir_module.class_parents.each do |name, parent|
        next unless parent
        (@class_children[parent] ||= [] of String) << name
      end
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Main Entry Point
    # ─────────────────────────────────────────────────────────────────────────

    def lower(progress : Bool = false) : Module
      # Two-pass approach for forward references:
      # Pass 1: Create all function stubs (for call resolution)
      # Track which functions we've created stubs for (avoid duplicates from methods with/without blocks)
      total = @hir_module.functions.size
      STDERR.puts "    Pass 1: Creating #{total} function stubs..." if progress
      seen_names = Set(String).new
      @hir_module.functions.each_with_index do |hir_func, idx|
        if progress && (idx % 5000 == 0 || idx == total - 1)
          STDERR.puts "      Stub #{idx + 1}/#{total}..."
        end
        # Skip duplicates (methods with block have same mangled name as non-block version)
        next if seen_names.includes?(hir_func.name)
        seen_names.add(hir_func.name)
        create_function_stub(hir_func)
      end

      if (dump_path = ENV["DEBUG_MIR_FUNC_NAMES"]?)
        path = dump_path.empty? || dump_path == "1" ? "/tmp/mir_function_names.txt" : dump_path
        match_sub = ENV["DEBUG_MIR_FUNC_MATCH"]?
        File.open(path, "w") do |io|
          io.puts "[HIR]"
          @hir_module.functions.each do |hir_func|
            name = hir_func.name
            next if match_sub ? !name.includes?(match_sub) : !name.includes?('(')
            io.puts name
          end
          io.puts "[MIR]"
          @mir_module.functions.each do |mir_func|
            name = mir_func.name
            next if match_sub ? !name.includes?(match_sub) : !name.includes?('(')
            io.puts name
          end
        end
      end

      # Build base-name index for fuzzy call resolution (avoids O(N) scans with split)
      # Also build class-name index for virtual dispatch (avoids O(N) full scans)
      @mir_module.functions.each do |func|
        name = func.name
        dollar_idx = name.index('$')
        base = dollar_idx ? name[0, dollar_idx] : name
        @function_by_base_name[base] = func unless @function_by_base_name.has_key?(base)

        # Index by class name (part before '#' or '.')
        hash_idx = name.index('#')
        dot_idx = name.index('.')
        sep_idx = if hash_idx && dot_idx
                    Math.min(hash_idx, dot_idx)
                  else
                    hash_idx || dot_idx
                  end
        if sep_idx
          class_name = name[0, sep_idx]
          (@functions_by_class[class_name] ||= [] of Function) << func
        end
      end

      # Pass 2: Lower function bodies
      # Track which functions we've processed to avoid duplicate lowering
      STDERR.puts "    Pass 2: Lowering #{total} function bodies..." if progress
      processed = Set(String).new
      @hir_module.functions.each_with_index do |hir_func, idx|
        if progress && (idx % 5000 == 0 || idx == total - 1)
          STDERR.puts "      Body #{idx + 1}/#{total}..."
        end
        # Skip if already processed
        next if processed.includes?(hir_func.name)
        processed.add(hir_func.name)
        begin
          @current_lowering_func_name = hir_func.name
          lower_function_body(hir_func)
        rescue ex : IndexError
          raise "Index out of bounds in function #{idx + 1}/#{total}: #{hir_func.name}\n#{ex.message}\n#{ex.backtrace.first(10).join("\n")}"
        end
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

    def register_extern_globals(globals : Array(HIR::ExternGlobal))
      globals.each do |glob|
        mir_type = convert_type(glob.type)
        @mir_module.add_extern_global(glob.real_name, mir_type)
      end
    end

    def self.class_var_global_name(class_name : String, var_name : String) : String
      "#{class_name}__classvar__#{var_name}"
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
        # Skip primitive types - they should not be registered as struct types
        # (their LLVM types are handled by the TypeRef case statement)
        case class_name
        when "Int8", "Int16", "Int32", "Int64", "Int128",
             "UInt8", "UInt16", "UInt32", "UInt64", "UInt128",
             "Float32", "Float64", "Bool", "Char", "Nil", "Void"
          next
        end

        # Convert HIR TypeRef to MIR TypeRef
        mir_type_ref = convert_type(info.type_ref)

        # Determine TypeKind (class = reference type, struct = value type)
        type_kind = info.is_struct ? TypeKind::Struct : TypeKind::Reference

        # Calculate total size (struct: just ivars, class: 4-byte i32 type_id header + ivars)
        total_size = info.size.to_u64

        # Create type in registry, or get existing type by name (for built-in types like String)
        # First check if a type with this name already exists (e.g., String as primitive)
        mir_type = @mir_module.type_registry.get_by_name(class_name)
        if mir_type
          # Update size from class_info if it has ivars (e.g., String primitive registered with
          # size=8 but actual class_info has size=12 after ivar discovery)
          if total_size > mir_type.size && !info.ivars.empty?
            mir_type.size = total_size
          end
        else
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

    # Register enum types so the LLVM backend maps them to i32 instead of ptr.
    # Enums in Crystal are integer types (i32 by default).
    def register_enum_types(enum_names : Set(String), type_descriptors : Array(Crystal::HIR::TypeDescriptor))
      type_descriptors.each_with_index do |desc, idx|
        next unless enum_names.includes?(desc.name)

        hir_ref = Crystal::HIR::TypeRef.new(Crystal::HIR::TypeRef::FIRST_USER_TYPE + idx.to_u32)
        mir_ref = convert_type(hir_ref)
        next if @mir_module.type_registry.get(mir_ref)

        @mir_module.type_registry.create_type_with_id(
          mir_ref.id,
          TypeKind::Enum,
          desc.name,
          4_u64,   # i32 size
          4_u32    # i32 alignment
        )
      end
    end

    # Register tuple/named tuple types from HIR descriptors.
    # This enables tuple element access to lower as struct GEPs instead of array layout.
    def register_tuple_types(type_descriptors : Array(Crystal::HIR::TypeDescriptor))
      type_descriptors.each_with_index do |desc, idx|
        next unless desc.kind == Crystal::HIR::TypeKind::Tuple || desc.kind == Crystal::HIR::TypeKind::NamedTuple

        hir_ref = Crystal::HIR::TypeRef.new(Crystal::HIR::TypeRef::FIRST_USER_TYPE + idx.to_u32)
        mir_ref = convert_type(hir_ref)
        next if @mir_module.type_registry.get(mir_ref)

        element_refs = desc.type_params.map { |t| convert_type(t) }

        size = 0_u64
        align = 1_u32
        element_refs.each do |elem_ref|
          elem_type = @mir_module.type_registry.get(elem_ref)
          # For reference types (classes, structs) our compiler heap-allocates them,
          # so in a Tuple they occupy pointer size (8 bytes), not their full struct size.
          # Only primitives and enums are stored inline with their actual size.
          elem_kind = elem_type.try(&.kind)
          is_inline = elem_kind && (elem_kind.primitive? || elem_kind.enum?)
          elem_size = if is_inline && elem_type && elem_type.size > 0
                        elem_type.size
                      else
                        8_u64  # Pointer/reference size
                      end
          elem_align = if is_inline && elem_type && elem_type.alignment > 0
                         elem_type.alignment
                       else
                         8_u32  # Pointer alignment
                       end
          size = align_u64(size, elem_align)
          size += elem_size
          align = elem_align if elem_align > align
        end
        size = align_u64(size, align)

        mir_type = @mir_module.type_registry.create_type_with_id(
          mir_ref.id,
          TypeKind::Tuple,
          desc.name,
          size,
          align
        )
        element_refs.each do |elem_ref|
          elem_type = @mir_module.type_registry.get(elem_ref) || @mir_module.type_registry.get(TypeRef::POINTER)
          mir_type.add_element_type(elem_type) if elem_type
        end
      end
    end

    private def align_u64(value : UInt64, align : UInt32) : UInt64
      a = align.to_u64
      return value if a <= 1
      ((value + a - 1) // a) * a
    end

    # Create function stub with params and return type (no body)
    private def create_function_stub(hir_func : HIR::Function)
      mir_return_type = convert_type(hir_func.return_type)
      mir_func = @mir_module.create_function(
        hir_func.name,
        mir_return_type
      )

      # Add parameter types (needed for call site type checking)
      hir_func.params.each do |param|
        mir_func.add_param(param.name, convert_type(param.type), param.default_literal)
      end
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Function Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_function_body(hir_func : HIR::Function)
      # Get the pre-created function stub
      mir_func = @mir_module.get_function(hir_func.name)
      if mir_func.nil?
        # List available functions for debugging
        available = @mir_module.functions.map(&.name).sort.join(", ")
        raise "MIR function stub not found for: #{hir_func.name}\nAvailable functions containing 'step': #{@mir_module.functions.select { |f| f.name.includes?("step") }.map(&.name).join(", ")}"
      end

      @current_hir_func = hir_func
      @current_mir_func = mir_func
      @current_block_param_id = function_contains_yield?(hir_func) ? infer_block_param_id(hir_func) : nil
      @current_slab_frame = should_use_slab_frame?(hir_func)
      mir_func.slab_frame = @current_slab_frame
      @value_map.clear
      @hir_value_types.clear
      @block_map.clear
      @pending_phis.clear
      @stack_slot_values.clear
      @stack_slot_types.clear
      @inline_struct_ptrs.clear
      @builder = Builder.new(mir_func)

      # Map HIR params to MIR params (already added in stub)
      hir_func.params.each_with_index do |param, idx|
        # MIR params are value IDs starting from 0
        @value_map[param.id] = idx.to_u32
      end

      # Record HIR value types for cast lowering
      hir_func.params.each do |param|
        @hir_value_types[param.id] = param.type
      end
      hir_func.blocks.each do |hir_block|
        hir_block.instructions.each do |inst|
          @hir_value_types[inst.id] = inst.type
        end
      end

      # Create all blocks first (for forward references)
      hir_func.blocks.each do |hir_block|
        mir_block_id = mir_func.create_block
        @block_map[hir_block.id] = mir_block_id
      end

      # Fix entry block mapping
      @block_map[hir_func.entry_block] = mir_func.entry_block

      # Lower each block (phi incoming resolution is deferred)
      ordered_blocks = order_blocks_for(hir_func)
      ordered_blocks.each do |hir_block|
        lower_block(hir_block)
      end

      # Now resolve all phi incoming values (after all blocks are lowered)
      resolve_pending_phis

      # Compute predecessors for phi resolution
      mir_func.compute_predecessors

      @stats.functions_lowered += 1
      @current_block_param_id = nil
    end

    # Resolve phi incoming values after all blocks are lowered
    private def resolve_pending_phis
      builder = @builder.not_nil!
      original_block = builder.current_block

      @pending_phis.each do |(mir_phi, hir_phi)|
        mir_phi_type = mir_phi.type
        is_phi_union = is_union_type?(mir_phi_type)
        union_descriptor = is_phi_union ? @mir_module.get_union_descriptor(mir_phi_type) : nil

        hir_phi.incoming.each do |(hir_block, hir_value)|
          mir_block = @block_map[hir_block]
          mir_value = get_value(hir_value)

          if is_phi_union && union_descriptor
            if hir_type = @hir_value_types[hir_value]?
              incoming_mir_type = convert_type(hir_type)
              if incoming_mir_type == TypeRef::VOID || incoming_mir_type == TypeRef::NIL
                if nil_variant = union_descriptor.variants.find { |v| v.type_ref == TypeRef::NIL }
                  builder.current_block = mir_block
                  nil_val = builder.const_nil
                  mir_value = builder.union_wrap(nil_val, nil_variant.type_id, mir_phi_type)
                end
              elsif !is_union_type?(incoming_mir_type)
                variant = union_descriptor.variants.find { |v| v.type_ref == incoming_mir_type }
                if variant
                  builder.current_block = mir_block
                  mir_value = builder.union_wrap(mir_value, variant.type_id, mir_phi_type)
                end
              end
            end
          end

          mir_phi.add_incoming(from: mir_block, value: mir_value)
        end
      end

      builder.current_block = original_block
    end

    private def order_blocks_for(hir_func : HIR::Function) : Array(HIR::Block)
      visited = Set(HIR::BlockId).new
      ordered = [] of HIR::Block
      stack = [] of HIR::BlockId
      stack << hir_func.entry_block

      while block_id = stack.pop?
        next if visited.includes?(block_id)
        visited.add(block_id)
        block = hir_func.get_block(block_id)
        ordered << block

        successors = block_successors(block)
        # Preserve a stable order by pushing in reverse.
        successors.reverse_each { |succ| stack << succ }
      end

      # Append unreachable blocks to keep lowering deterministic.
      hir_func.blocks.each do |block|
        next if visited.includes?(block.id)
        ordered << block
      end

      ordered
    end

    private def block_successors(block : HIR::Block) : Array(HIR::BlockId)
      case term = block.terminator
      when HIR::Branch
        [term.then_block, term.else_block]
      when HIR::Jump
        [term.target]
      when HIR::Switch
        succs = term.cases.map { |(_, bid)| bid }
        succs << term.default
        succs
      else
        [] of HIR::BlockId
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
      mir_id = begin
        case hir_value
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
                   slot = builder.alloc(MemoryStrategy::Stack, param_type)
                   record_stack_slot(slot, param_type)
                   if (default_id = default_value_for_type(builder, param_type))
                     builder.store(slot, default_id)
                   end
                   @value_map[hir_value.id] = slot
                   slot
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
               when HIR::FuncPointer
                 lower_func_pointer(hir_value)
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
               when HIR::ArraySetSize
                 lower_array_set_size(hir_value)
               when HIR::ArrayNew
                 lower_array_new(hir_value)
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
      rescue ex : IndexError
        raise "Index error in #{@current_lowering_func_name} lowering #{hir_value.class} (id=#{hir_value.id}): #{ex.message}\n#{ex.backtrace.first(10).join("\n")}"
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
        record_stack_slot(ptr, convert_type(local.type))
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
      elsif !alloc.constructor_args.empty?
        # Fallback for tuples not in registry: estimate from constructor arg count
        alloc_size = (alloc.constructor_args.size * 8).to_u64
      end

      # Fix StaticArray size: if type is StaticArray but size is 0, compute from name
      if alloc_size == 0
        type_name = @mir_module.type_registry.get(mir_type_ref).try(&.name) || ""
        if m = type_name.match(/StaticArray\((.+),\s*(\d+)\)/)
          elem_name = m[1].strip
          count = m[2].to_u64
          elem_type = @mir_module.type_registry.get_by_name(elem_name)
          elem_size = elem_type ? elem_type.size : 1_u64
          elem_size = 1_u64 if elem_size == 0
          alloc_size = elem_size * count
        end
        alloc_size = 8_u64 if alloc_size == 0  # safety fallback
      end

      # Create allocation with proper size
      ptr = builder.alloc(strategy, mir_type_ref, alloc_size)

      # Stamp class allocations with their type_id in the header (vtable slot).
      if !alloc.is_value_type
        if mir_type = @mir_module.type_registry.get(mir_type_ref)
          if mir_type.kind.reference?
            type_id_value = builder.const_int(mir_type_ref.id.to_i64, TypeRef::INT32)
            header_ptr = builder.gep(ptr, [0_u32], TypeRef::POINTER)
            builder.store(header_ptr, type_id_value)
          end
        end
      end

      # Store constructor_args into tuple/struct fields
      unless alloc.constructor_args.empty?
        if ENV["DEBUG_ALLOC_ARGS"]?
          func_name = builder.@function.name
          STDERR.puts "[ALLOC_ARGS] func=#{func_name} type=#{mir_type_ref.id} alloc_ptr=#{ptr} args=#{alloc.constructor_args.size}"
        end

        # Pre-compute byte offsets for tuple element types (instead of using field index)
        tuple_byte_offsets : Array(UInt32)? = nil
        if mir_type = @mir_module.type_registry.get(mir_type_ref)
          if mir_type.kind.tuple? && (elements = mir_type.element_types)
            offsets = [] of UInt32
            current_offset = 0_u64
            elements.each do |elem|
              # For reference types (classes, structs) our compiler heap-allocates them,
              # so in a Tuple they occupy pointer size (8), not their full struct size.
              is_inline = elem.kind.primitive? || elem.kind.enum?
              elem_size = if is_inline && elem.size > 0
                            elem.size
                          else
                            8_u64  # Pointer/reference size
                          end
              elem_align = if is_inline && elem.alignment > 0
                             elem.alignment
                           else
                             8_u32
                           end
              current_offset = align_u64(current_offset, elem_align)
              offsets << current_offset.to_u32
              current_offset += elem_size
            end
            tuple_byte_offsets = offsets
          elsif fields = mir_type.fields
            # For struct types with fields, use field byte offsets
            offsets = fields.map(&.offset)
            tuple_byte_offsets = offsets unless offsets.empty?
          end
        end

        alloc.constructor_args.each_with_index do |arg_hir_id, idx|
          arg_val = get_value(arg_hir_id)
          byte_offset = if (offsets = tuple_byte_offsets) && idx < offsets.size
                          offsets[idx]
                        else
                          # Fallback: estimate byte offset assuming 8-byte elements
                          (idx * 8).to_u32
                        end
          field_ptr = builder.gep(ptr, [byte_offset], TypeRef::POINTER)
          store_id = builder.store(field_ptr, arg_val)
          if ENV["DEBUG_ALLOC_ARGS"]?
            STDERR.puts "[ALLOC_ARGS]   [#{idx}] hir=#{arg_hir_id} mir_val=#{arg_val} byte_offset=#{byte_offset} gep=#{field_ptr} store=#{store_id}"
          end
        end
      end

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
        mapped = map_hir_strategy(strat)
        if @current_slab_frame && alloc.lifetime == HIR::LifetimeTag::StackLocal && mapped == MemoryStrategy::ARC
          return MemoryStrategy::Slab
        end
        return mapped
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

    private def should_use_slab_frame?(hir_func : HIR::Function) : Bool
      return false unless @slab_frame_enabled

      saw_alloc = false
      hir_func.blocks.each do |block|
        block.instructions.each do |inst|
          next unless inst.is_a?(HIR::Allocate)
          saw_alloc = true
          return false unless inst.lifetime == HIR::LifetimeTag::StackLocal

          taints = inst.taints
          return false if taints.thread_shared? || taints.ffi_exposed? || taints.cyclic?
        end
      end

      saw_alloc
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Field Access Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_field_get(field : HIR::FieldGet) : ValueId
      builder = @builder.not_nil!
      obj_ptr = get_value(field.object)

      # GEP to field address
      # field_offset is byte offset from object start
      field_ptr = builder.gep(obj_ptr, [field.field_offset.to_u32], TypeRef::POINTER)

      # When the object was obtained from Pointer(Struct).value (inline struct data),
      # struct-typed fields are also stored inline — return the GEP pointer without loading.
      # For non-inline objects (normal heap-allocated), always load as before.
      if @inline_struct_ptrs.includes?(field.object) && hir_type_is_struct?(field.type)
        # Tag the result as also pointing to inline struct data
        @inline_struct_ptrs << field.id
        field_ptr
      else
        builder.load(field_ptr, convert_type(field.type))
      end
    end

    private def lower_field_set(field : HIR::FieldSet) : ValueId
      builder = @builder.not_nil!
      obj_ptr = get_value(field.object)
      value = get_value(field.value)

      # GEP to field address + store
      field_ptr = builder.gep(obj_ptr, [field.field_offset.to_u32], TypeRef::POINTER)
      builder.store(field_ptr, value)
      value
    end

    # Check if a HIR TypeRef refers to a struct (value type).
    private def hir_type_is_struct?(type : HIR::TypeRef) : Bool
      return false if type.id < HIR::TypeRef::FIRST_USER_TYPE
      desc = @hir_module.get_type_descriptor(type)
      return false unless desc
      desc.kind == HIR::TypeKind::Struct
    end

    # Get the inline size of a struct type (from class_info via MIR type registry).
    private def hir_type_inline_size(type : HIR::TypeRef) : Int32
      mir_ref = convert_type(type)
      if mir_type = @mir_module.type_registry.get(mir_ref)
        return mir_type.size.to_i32 if mir_type.size > 0
      end
      0
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
      if element_type == MIR::TypeRef::NIL
        return builder.const_nil_typed(element_type)
      end
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
      if element_type == MIR::TypeRef::NIL
        return builder.const_nil_typed(element_type)
      end
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
      value
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
      debug_virtual = ENV.has_key?("DEBUG_VIRTUAL_CALLS")

      # Get arguments
      begin
        args = call.args.map { |arg| get_value(arg) }
      rescue ex : IndexError
        raise "Index error getting args for call to #{call.method_name}: #{ex.message}"
      end

      # Add receiver as first arg if present
      if recv = call.receiver
        args.unshift(get_value(recv))
      end
      if debug_virtual && call.virtual
        recv_type = call.receiver ? @hir_value_types[call.receiver.not_nil!]? : nil
        recv_type_name = hir_type_name(recv_type)
        STDERR.puts "[VIRTUAL_CALL] method=#{call.method_name} receiver=#{recv_type_name} args=#{call.args.size} func=#{@current_lowering_func_name}"
      end

      recv_type = call.receiver ? @hir_value_types[call.receiver.not_nil!]? : nil
      recv_desc = recv_type ? @hir_module.get_type_descriptor(recv_type) : nil

      # Union numeric conversions (to_i*, to_u*): inline by extracting payload bytes.
      if recv_desc && recv_desc.kind == HIR::TypeKind::Union && call.args.empty?
        if method_suffix = extract_method_suffix_loose(call.method_name)
          if target_type = union_conversion_target_type(method_suffix)
            if converted = lower_union_numeric_conversion(args[0], recv_type.not_nil!, target_type)
              return converted
            end
          end
        end
      end

      # Check if this is an external/runtime call
      if call.method_name.starts_with?("__crystal_v2_")
        return builder.extern_call(call.method_name, args, convert_type(call.type))
      end

      # Atomic intrinsics: Atomic(T)#get → load from self+0, Atomic(T)#set → store to self+0
      # Original Crystal uses @[Primitive(:load_atomic)] / @[Primitive(:store_atomic)]
      # but our compiler can't lower those primitives, so we inline them here.
      if call.method_name.includes?("Atomic") && call.receiver
        method_suffix = call.method_name.rpartition("#").last.split("$").first
        if method_suffix == "get" || method_suffix == "Hget"
          # Atomic#get: load value from self + 0
          self_ptr = args[0]
          val_ptr = builder.gep(self_ptr, [0_u32], TypeRef::POINTER)
          return builder.load(val_ptr, convert_type(call.type))
        elsif method_suffix == "set" || method_suffix == "Hset"
          # Atomic#set: store value to self + 0
          self_ptr = args[0]
          new_val = args.size > 2 ? args[2] : (args.size > 1 ? args[1] : builder.const_int(0, TypeRef::INT32))
          val_ptr = builder.gep(self_ptr, [0_u32], TypeRef::POINTER)
          builder.store(val_ptr, new_val)
          return new_val
        elsif method_suffix == "swap" || method_suffix == "Hswap"
          # Atomic#swap: load old, store new, return old
          self_ptr = args[0]
          new_val = args.size > 2 ? args[2] : (args.size > 1 ? args[1] : builder.const_int(0, TypeRef::INT32))
          val_ptr = builder.gep(self_ptr, [0_u32], TypeRef::POINTER)
          old_val = builder.load(val_ptr, convert_type(call.type))
          builder.store(val_ptr, new_val)
          return old_val
        end
      end

      # Special handling for Proc#call - emit indirect call through function pointer
      # Proc calls have format "call$Type" or just "call" and receiver is a Proc type
      # Also match "call(...)" patterns from typed proc calls
      # Also handle Proc-shorthand types like "(A, B -> C)#call" from monomorphized generics
      is_proc_call = call.method_name == "call" ||
                     call.method_name.starts_with?("call$") ||
                     call.method_name.starts_with?("call(") ||
                     call.method_name == "Proc#call" ||
                     call.method_name.starts_with?("Proc#call$") ||
                     call.method_name.starts_with?("Proc#call(") ||
                     call.method_name.includes?("#call") ||  # e.g., "(A, B -> C)#call"
                     call.method_name.includes?("->") && call.method_name.ends_with?("#call")
      if ENV.has_key?("DEBUG_PROC_CALL") && call.method_name.includes?("call")
        recv_type = call.receiver ? @hir_value_types[call.receiver.not_nil!]? : nil
        recv_desc = recv_type ? @hir_module.get_type_descriptor(recv_type) : nil
        STDERR.puts "[PROC_CALL] func=#{@current_lowering_func_name} method=#{call.method_name} receiver=#{call.receiver} recv_type=#{recv_type.try(&.id)} recv_desc=#{recv_desc.try(&.name)} kind=#{recv_desc.try(&.kind)}"
      end
      if call.receiver && is_proc_call
        recv_type = @hir_value_types[call.receiver.not_nil!]?
        recv_desc = recv_type ? @hir_module.get_type_descriptor(recv_type) : nil
        if recv_type
          if recv_desc && recv_desc.kind == HIR::TypeKind::Proc
            # Proc is a function pointer - emit indirect call
            # args[0] = receiver (func ptr), args[1..] = actual arguments
            filtered_args = [] of ValueId
            filtered_args << args[0]
            call.args.each_with_index do |arg_id, idx|
              arg_type = @hir_value_types[arg_id]?
              in_map = @value_map.has_key?(arg_id)
              next if arg_type == HIR::TypeRef::VOID
              next unless in_map
              filtered_args << args[idx + 1]
            end
            return builder.call_indirect(filtered_args[0], filtered_args[1..].to_a, convert_type(call.type))
          end
        end
      end

      # Intercept IO#puts/print for Float64/Float32 BEFORE virtual dispatch.
      # Float::Printer.shortest is broken (yield/block issues), so redirect to printf-based helpers.
      if args.size == 2 # self (IO) + float value
        _mn = call.method_name
        float_extern = if _mn.includes?("puts$Float64")
                          "__crystal_v2_print_float64_ln"
                        elsif _mn.includes?("puts$Float32")
                          "__crystal_v2_print_float32_ln"
                        elsif _mn.includes?("print$Float64")
                          "__crystal_v2_print_float64"
                        elsif _mn.includes?("print$Float32")
                          "__crystal_v2_print_float32"
                        else
                          nil
                        end
        if float_extern
          # Pass only the float argument (args[1]), not the IO self
          return builder.extern_call(float_extern, [args[1]], TypeRef::VOID)
        end
      end

      if call.virtual
        if dispatched = lower_virtual_dispatch(call, args)
          return dispatched
        end
      end

      # Method name may be mangled as "puts$Int32" or "puts:Int32" etc, so extract base name
      # We use $ as separator (: is not valid in LLVM identifiers)
      # Avoid regex by extracting up to first : or $
      method_name_str = call.method_name
      colon_pos = method_name_str.index(':')
      dollar_pos = method_name_str.index('$')
      base_method_name = if colon_pos || dollar_pos
                           split_pos = [colon_pos || method_name_str.size, dollar_pos || method_name_str.size].min
                           method_name_str[0, split_pos]
                         else
                           method_name_str
                         end

      # Look up function by name - try exact match first, then fuzzy match
      func = @mir_module.get_function(method_name_str)

      # If not found, try fuzzy matching to handle type variations (e.g., String vs String | Nil)
      unless func
        if debug_virtual && call.virtual
          STDERR.puts "[VIRTUAL_CALL] unresolved method=#{method_name_str} base=#{base_method_name} func=#{@current_lowering_func_name}"
        end
        # Only apply fuzzy matching for qualified method names (containing . or #)
        if method_name_str.includes?('.') || method_name_str.includes?('#')
          # Extract base name (before $ type suffix) without allocating
          dollar_idx = method_name_str.index('$')
          base_name = dollar_idx ? method_name_str[0, dollar_idx] : method_name_str

          # O(1) lookup via pre-computed index
          func = @function_by_base_name[base_name]?
        else
          # For unqualified method names with a receiver, try to qualify based on receiver type
          if call.receiver
            recv_type = @hir_value_types[call.receiver.not_nil!]?
            if recv_type
              recv_desc = @hir_module.get_type_descriptor(recv_type)
              type_name = recv_desc.try(&.name) || hir_type_name(recv_type)
              if type_name && !type_name.empty?
                # Try qualified name with type prefix
                qualified_name = "#{type_name}##{method_name_str}"
                func = @mir_module.get_function(qualified_name)

                # If not found, try fuzzy matching via index
                unless func
                  q_dollar = qualified_name.index('$')
                  q_base = q_dollar ? qualified_name[0, q_dollar] : qualified_name
                  func = @function_by_base_name[q_base]?
                end
              end
            end
          end
        end
      end

      if func
        callee_id = func.id
        # Build hir_args that matches mir_args ordering (receiver first, then explicit args)
        hir_args_for_coerce = if recv = call.receiver
                                 [recv] + call.args
                               else
                                 call.args
                               end
        coerced_args = coerce_call_args(builder, args, hir_args_for_coerce, func)
        return builder.call(callee_id, coerced_args, convert_type(call.type))
      end

      # Built-in print functions (fallback only when no user-defined function exists).
      if base_method_name == "puts"
        # Determine the actual extern based on argument type
        if args.size == 1
          # Get arg type - could be from call.args[0] or from receiver
          arg_type = if call.args.size > 0
                       get_arg_type(call.args[0])
                     elsif recv_id = call.receiver
                       get_arg_type(recv_id)
                     else
                       TypeRef::STRING
                     end
          extern_name = case arg_type
                        when TypeRef::INT32, TypeRef::UINT32, TypeRef::CHAR
                          "__crystal_v2_print_int32_ln"
                        when TypeRef::INT64, TypeRef::UINT64
                          "__crystal_v2_print_int64_ln"
                        when TypeRef::FLOAT32
                          "__crystal_v2_print_float32_ln"
                        when TypeRef::FLOAT64
                          "__crystal_v2_print_float64_ln"
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
          # Get arg type - could be from call.args[0] or from receiver
          arg_type = if call.args.size > 0
                       get_arg_type(call.args[0])
                     elsif recv_id = call.receiver
                       get_arg_type(recv_id)
                     else
                       TypeRef::INT32
                     end
          extern_name = case arg_type
                        when TypeRef::INT32, TypeRef::UINT32, TypeRef::CHAR
                          "__crystal_v2_print_int32"
                        when TypeRef::INT64, TypeRef::UINT64
                          "__crystal_v2_print_int64"
                        when TypeRef::FLOAT32
                          "__crystal_v2_print_float32"
                        when TypeRef::FLOAT64
                          "__crystal_v2_print_float64"
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

      # Unknown function - emit as extern call
      if ENV.has_key?("CRYSTAL_V2_UNRESOLVED_CALL_TRACE")
        recv_type = call.receiver ? @hir_value_types[call.receiver.not_nil!]? : nil
        recv_name = hir_type_name(recv_type)
        STDERR.puts "[UNRESOLVED_CALL] func=#{@current_lowering_func_name} method=#{call.method_name} base=#{base_method_name} recv=#{recv_name} virtual=#{call.virtual} args=#{call.args.size}"
      elsif ENV.has_key?("DEBUG_CALLS")
        STDERR.puts "[UNRESOLVED_CALL] #{call.method_name} in #{@current_lowering_func_name}"
      end
      builder.extern_call(call.method_name, args, convert_type(call.type))
    end

    private def extract_method_suffix_loose(full_name : String) : String?
      if suffix = extract_method_suffix(full_name)
        return suffix
      end
      conversions = ["to_i", "to_i8", "to_i16", "to_i32", "to_i64", "to_i128",
                     "to_u", "to_u8", "to_u16", "to_u32", "to_u64", "to_u128"]
      conversions.each do |suffix|
        return suffix if full_name.ends_with?("_#{suffix}") || full_name == suffix
      end
      nil
    end

    private def union_conversion_target_type(method_suffix : String) : TypeRef?
      # Strip trailing ! (unsafe conversion variant) before matching
      suffix = method_suffix.rstrip('!')
      case suffix
      when "to_i", "to_i32"
        TypeRef::INT32
      when "to_i8"
        TypeRef::INT8
      when "to_i16"
        TypeRef::INT16
      when "to_i64"
        TypeRef::INT64
      when "to_i128"
        TypeRef::INT128
      when "to_u", "to_u32"
        TypeRef::UINT32
      when "to_u8"
        TypeRef::UINT8
      when "to_u16"
        TypeRef::UINT16
      when "to_u64"
        TypeRef::UINT64
      when "to_u128"
        TypeRef::UINT128
      else
        nil
      end
    end

    private def union_numeric_bit_width(type_ref : TypeRef) : Int32?
      case type_ref
      when TypeRef::INT8, TypeRef::UINT8
        8
      when TypeRef::INT16, TypeRef::UINT16
        16
      when TypeRef::INT32, TypeRef::UINT32, TypeRef::CHAR
        32
      when TypeRef::INT64, TypeRef::UINT64
        64
      when TypeRef::INT128, TypeRef::UINT128
        128
      else
        nil
      end
    end

    private def union_unsigned_type_for_bits(bits : Int32) : TypeRef
      case bits
      when 8
        TypeRef::UINT8
      when 16
        TypeRef::UINT16
      when 32
        TypeRef::UINT32
      when 64
        TypeRef::UINT64
      when 128
        TypeRef::UINT128
      else
        TypeRef::UINT64
      end
    end

    private def union_signed_type_for_bits(bits : Int32) : TypeRef
      case bits
      when 8
        TypeRef::INT8
      when 16
        TypeRef::INT16
      when 32
        TypeRef::INT32
      when 64
        TypeRef::INT64
      when 128
        TypeRef::INT128
      else
        TypeRef::INT64
      end
    end

    private def union_numeric_conversion_cast_kind(src_bits : Int32, dst_bits : Int32, dst_signed : Bool) : CastKind
      if dst_bits < src_bits
        CastKind::Trunc
      elsif dst_bits > src_bits
        dst_signed ? CastKind::SExt : CastKind::ZExt
      else
        CastKind::Bitcast
      end
    end

    private def union_numeric_conversion_signed?(type_ref : TypeRef) : Bool
      case type_ref
      when TypeRef::INT8, TypeRef::INT16, TypeRef::INT32, TypeRef::INT64, TypeRef::INT128
        true
      else
        false
      end
    end

    private def lower_union_numeric_conversion(
      union_value : ValueId,
      recv_type : HIR::TypeRef,
      target_type : TypeRef
    ) : ValueId?
      mir_union_ref = convert_type(recv_type)
      union_desc = @mir_module.get_union_descriptor(mir_union_ref)
      return nil unless union_desc

      # Reject unions with Nil (unsafe to ignore tag).
      union_desc.variants.each do |variant|
        return nil if variant.type_ref == TypeRef::NIL
      end

      # Collect numeric variants and determine payload width.
      max_bits = nil.as(Int32?)
      union_desc.variants.each do |variant|
        bits = union_numeric_bit_width(variant.type_ref)
        return nil unless bits
        max_bits = max_bits ? (bits > max_bits ? bits : max_bits) : bits
      end
      return nil unless max_bits

      payload_type = union_unsigned_type_for_bits(max_bits)
      payload = @builder.not_nil!.cast(CastKind::Bitcast, union_value, payload_type)

      # If target is same width unsigned payload, reuse directly.
      return payload if payload_type == target_type

      dst_bits = union_numeric_bit_width(target_type)
      return nil unless dst_bits
      kind = union_numeric_conversion_cast_kind(max_bits, dst_bits, union_numeric_conversion_signed?(target_type))
      @builder.not_nil!.cast(kind, payload, target_type)
    end

    # Dispatch kind for unified vdispatch generator
    private enum VDispatchKind
      Union  # receiver is a union type - use UnionTypeIdGet + UnionUnwrap
      Class  # receiver is a class pointer - use gep header + load type_id
    end

    # Unified candidate structure for vdispatch
    private alias VDispatchCandidate = NamedTuple(
      type_id: Int32,
      func: Function?,
      type_ref: TypeRef?,       # for Union unwrap
      variant_id: Int32?,       # for Union unwrap
      dispatch_class: String?   # for nested class dispatch
    )

    # Unified vdispatch body generator - handles both Union and Class dispatch
    # Returns the phi node if return type is non-void, nil otherwise
    private def generate_vdispatch_body(
      dispatch_func : Function,
      dispatch_builder : Builder,
      param_values : Array(ValueId),
      candidates : Array(VDispatchCandidate),
      kind : VDispatchKind,
      method_suffix : String?,
      hir_call : HIR::Call?
    ) : Phi?
      # 1. Get type ID based on dispatch kind
      type_id_val = case kind
        in .union?
          dispatch_builder.emit(MIR::UnionTypeIdGet.new(dispatch_builder.next_id, param_values[0]))
        in .class?
          header_ptr = dispatch_builder.gep(param_values[0], [0_u32], TypeRef::POINTER)
          dispatch_builder.load(header_ptr, TypeRef::INT32)
      end

      # 2. Create end block and phi (if non-void return)
      end_block = dispatch_func.create_block
      phi : Phi? = nil
      if dispatch_func.return_type != TypeRef::VOID
        dispatch_builder.current_block = end_block
        phi = dispatch_builder.phi(dispatch_func.return_type)
      end

      # 3. Create default block and case blocks
      default_block = dispatch_func.create_block
      cases = [] of Tuple(Int64, BlockId)
      candidates.each do |candidate|
        case_block = dispatch_func.create_block
        cases << {candidate[:type_id].to_i64, case_block}
      end

      # 4. Set up switch in entry block
      dispatch_builder.current_block = dispatch_func.entry_block
      dispatch_func.get_block(dispatch_func.entry_block).terminator = Switch.new(type_id_val, cases, default_block)

      # 5. Generate each case
      candidates.each_with_index do |candidate, idx|
        case_block = cases[idx][1]
        dispatch_builder.current_block = case_block

        cand_args = param_values.dup

        # Union: unwrap receiver to concrete type
        if kind.union? && candidate[:type_ref] && candidate[:variant_id]
          unwrap = MIR::UnionUnwrap.new(
            dispatch_builder.next_id,
            candidate[:type_ref].not_nil!,
            param_values[0],
            candidate[:variant_id].not_nil!,
            false
          )
          dispatch_builder.emit(unwrap)
          cand_args[0] = unwrap.id
        end

        # Get the function to call (may need nested dispatch for union containing class hierarchy)
        call_func = candidate[:func]
        if call_func.nil? && candidate[:dispatch_class] && method_suffix && hir_call
          call_func = ensure_class_dispatch_for_union(
            candidate[:dispatch_class].not_nil!,
            method_suffix,
            candidate[:type_ref] || TypeRef::POINTER,
            hir_call
          )
        end

        if call_func
          callee_param_count = call_func.params.size
          coerced_args = cand_args
          if hir_call
            recv_id = hir_call.receiver
            if callee_param_count == cand_args.size
              # Same param count — pass all args including receiver
              hir_args_with_receiver = recv_id ? [recv_id] + hir_call.args : hir_call.args
              coerced_args = coerce_call_args(dispatch_builder, cand_args, hir_args_with_receiver, call_func)
            elsif callee_param_count < cand_args.size
              # Callee has fewer params (e.g. subclass doesn't take optional arg).
              # Truncate dispatch args to match callee's param count, keeping receiver.
              truncated_args = cand_args[0, callee_param_count]
              hir_args_with_receiver = recv_id ? [recv_id] + hir_call.args : hir_call.args
              truncated_hir = hir_args_with_receiver[0, callee_param_count]? || hir_args_with_receiver
              coerced_args = coerce_call_args(dispatch_builder, truncated_args, truncated_hir, call_func)
            else
              # Callee expects more args than dispatch provides — pass all, let coercion handle it
              hir_args_with_receiver = recv_id ? [recv_id] + hir_call.args : hir_call.args
              coerced_args = coerce_call_args(dispatch_builder, cand_args, hir_args_with_receiver, call_func)
            end
          elsif callee_param_count < cand_args.size
            coerced_args = cand_args[0, callee_param_count]
          end

          call_val = dispatch_builder.call(call_func.id, coerced_args, dispatch_func.return_type)
          if phi && call_val != 0_u32
            phi.add_incoming(from: case_block, value: call_val)
          end
          dispatch_func.get_block(case_block).terminator = Jump.new(end_block)
        else
          # No implementation found - jump to default (unreachable)
          dispatch_func.get_block(case_block).terminator = Jump.new(default_block)
        end
      end

      # 6. Set up end block return
      if phi
        dispatch_builder.current_block = end_block
        dispatch_func.get_block(end_block).terminator = Return.new(phi.id)
      else
        dispatch_func.get_block(end_block).terminator = Return.new(nil)
      end

      # 7. Default block is unreachable
      dispatch_func.get_block(default_block).terminator = Unreachable.new

      phi
    end

    private def lower_virtual_dispatch(call : HIR::Call, args : Array(ValueId)) : ValueId?
      recv_id = call.receiver
      return nil unless recv_id

      recv_type = @hir_value_types[recv_id]? || return nil
      recv_desc = @hir_module.get_type_descriptor(recv_type)
      return nil unless recv_desc

      method_suffix = extract_method_suffix(call.method_name)
      return nil unless method_suffix

      old_candidates = virtual_dispatch_candidates(recv_desc, recv_type, method_suffix, call.args.size)

      # When receiver is a Generic-kind type wrapping a union (e.g., Union(*Nil | Int32)),
      # the union descriptor might be at a different ref. Try to find it by name matching.
      generic_union_ref = nil.as(TypeRef?)
      if old_candidates.empty? && recv_desc.kind == HIR::TypeKind::Generic && recv_desc.name.starts_with?("Union(")
        inner_name = recv_desc.name[6..-2] # Strip "Union(" and ")"
        @mir_module.union_descriptors.each do |ref, desc|
          if desc.name == inner_name
            # Found matching union descriptor — build candidates from it
            desc.variants.each do |variant|
              next if variant.full_name == "Nil" || variant.full_name.starts_with?('*')
              if func = resolve_virtual_method_for_class(variant.full_name, method_suffix, call.args.size)
                old_candidates << {
                  type_id: variant.type_id,
                  type_ref: variant.type_ref,
                  variant_id: variant.type_id,
                  func: func,
                  dispatch_class: nil.as(String?)
                }
              end
            end
            generic_union_ref = ref
            break
          end
        end
      end
      return nil if old_candidates.empty?

      dispatch_name = "__vdispatch__#{call.method_name}"
      if existing = @mir_module.get_function(dispatch_name)
        return @builder.not_nil!.call(existing.id, args, existing.return_type)
      end

      # Determine return type: prefer candidate function return type over call.type
      # because the call site may discard the result (VOID) while the function actually
      # returns a value (e.g. property getters returning union types).
      ret_type = convert_type(call.type)
      if ret_type == TypeRef::VOID
        old_candidates.each do |c|
          if f = c[:func]
            if f.return_type != TypeRef::VOID
              ret_type = f.return_type
              break
            end
          end
        end
      end

      # Create dispatch function
      dispatch_func = @mir_module.create_function(dispatch_name, ret_type)
      param_values = [] of ValueId

      # For Generic-wrapped unions, use the union descriptor's ref as the receiver param type.
      # This ensures the LLVM type mapper produces a union struct type, making
      # UnionTypeIdGet and UnionUnwrap work correctly.
      recv_param_type = generic_union_ref || convert_type(recv_type)
      dispatch_func.add_param("recv", recv_param_type)
      param_values << 0_u32

      # Other params
      call.args.each_with_index do |arg_id, idx|
        arg_type = @hir_value_types[arg_id]? || HIR::TypeRef::POINTER
        dispatch_func.add_param("arg#{idx}", convert_type(arg_type))
        param_values << (idx + 1).to_u32
      end

      dispatch_builder = Builder.new(dispatch_func)

      # Convert to unified candidate format
      candidates = old_candidates.map do |c|
        VDispatchCandidate.new(
          type_id: c[:type_id],
          func: c[:func],
          type_ref: c[:type_ref],
          variant_id: c[:variant_id],
          dispatch_class: c[:dispatch_class]
        )
      end

      # Determine dispatch kind — Generic-wrapped unions get Union dispatch
      kind = (recv_desc.kind == HIR::TypeKind::Union || generic_union_ref) ? VDispatchKind::Union : VDispatchKind::Class

      # Use unified generator
      generate_vdispatch_body(
        dispatch_func,
        dispatch_builder,
        param_values,
        candidates,
        kind,
        method_suffix,
        call
      )

      # For Generic-wrapped unions, the caller passes a ptr but the dispatch function
      # expects a union struct value. Load the union value from the pointer.
      call_args = args
      if generic_union_ref
        builder = @builder.not_nil!
        union_val = builder.load(args[0], generic_union_ref)
        call_args = args.dup
        call_args[0] = union_val
      end

      @builder.not_nil!.call(dispatch_func.id, call_args, convert_type(call.type))
    end

    private def extract_method_suffix(full_name : String) : String?
      if idx = full_name.index('#')
        return full_name[(idx + 1)..-1]
      end
      nil
    end

    private def subclasses_for(base : String) : Array(String)
      if cached = @subclass_cache[base]?
        return cached
      end
      result = [] of String
      seen = Set(String).new
      queue = @class_children[base]?.dup || [] of String
      until queue.empty?
        name = queue.shift
        next if seen.includes?(name)
        seen.add(name)
        result << name
        if children = @class_children[name]?
          children.each { |child| queue << child }
        end
      end
      @subclass_cache[base] = result
      result
    end

    private def module_includers_for(module_name : String) : Array(String)
      if cached = @module_includers_cache[module_name]?
        return cached
      end
      base_module = strip_generic_args(module_name)
      includers = @hir_module.module_includers[module_name]? || @hir_module.module_includers[base_module]?
      if includers.nil? || includers.empty?
        matches = @hir_module.module_includers.keys.select do |key|
          key.ends_with?("::#{module_name}") || key.ends_with?("::#{base_module}")
        end
        includers = @hir_module.module_includers[matches.first]? if matches.size == 1
      end

      if includers.nil? || includers.empty?
        generic_matches = @hir_module.module_includers.keys.select do |key|
          strip_generic_args(key) == base_module
        end
        if generic_matches.size == 1
          includers = @hir_module.module_includers[generic_matches.first]?
        elsif generic_matches.size > 1
          merged = [] of String
          generic_matches.each do |key|
            if list = @hir_module.module_includers[key]?
              list.each { |entry| merged << entry }
            end
          end
          includers = merged.uniq! if merged.any?
        end
      end

      if (includers.nil? || includers.empty?) && module_name.includes?("::")
        short_name = short_module_name(module_name)
        base_short_name = short_module_name(base_module)
        includers = @hir_module.module_includers[short_name]? || @hir_module.module_includers[base_short_name]?
        if includers.nil? || includers.empty?
          matches = @hir_module.module_includers.keys.select do |key|
            key.ends_with?("::#{short_name}") || key.ends_with?("::#{base_short_name}")
          end
          includers = @hir_module.module_includers[matches.first]? if matches.size == 1
        end
      end

      if (includers.nil? || includers.empty?) && module_name.includes?("::")
        generic_matches = @hir_module.module_includers.keys.select do |key|
          strip_generic_args(key) == base_short_name
        end
        if generic_matches.size == 1
          includers = @hir_module.module_includers[generic_matches.first]?
        elsif generic_matches.size > 1
          merged = [] of String
          generic_matches.each do |key|
            if list = @hir_module.module_includers[key]?
              list.each { |entry| merged << entry }
            end
          end
          includers = merged.uniq! if merged.any?
        end
      end

      result = includers ? includers.dup : [] of String
      @module_includers_cache[module_name] = result
      result
    end

    @[AlwaysInline]
    private def strip_generic_args(name : String) : String
      if idx = name.index('(')
        return name.byte_slice(0, idx)
      end
      name
    end

    @[AlwaysInline]
    private def short_module_name(name : String) : String
      if idx = name.rindex("::")
        return name.byte_slice(idx + 2)
      end
      name
    end

    private def ensure_reference_type_for_name(name : String) : Type?
      if mir_type = @mir_module.type_registry.get_by_name(name)
        return mir_type
      end

      hir_index = @hir_module.types.index { |desc| desc.name == name }
      return nil unless hir_index
      hir_desc = @hir_module.types[hir_index]
      return nil unless hir_desc.kind == HIR::TypeKind::Module

      hir_ref = HIR::TypeRef.new(HIR::TypeRef::FIRST_USER_TYPE + hir_index.to_u32)
      mir_ref = convert_type(hir_ref)
      @mir_module.type_registry.create_type_with_id(
        mir_ref.id,
        TypeKind::Reference,
        name,
        8_u64,
        8_u32
      )
    end

    private def enclosing_class_for_module(module_name : String) : String?
      parts = module_name.split("::")
      while parts.size > 1
        parts.pop
        candidate = parts.join("::")
        return candidate if @hir_module.class_parents.has_key?(candidate)
      end
      nil
    end

    private def virtual_dispatch_candidates(
      recv_desc : HIR::TypeDescriptor,
      recv_type : HIR::TypeRef,
      method_suffix : String,
      arg_count : Int32
    ) : Array(NamedTuple(type_id: Int32, type_ref: TypeRef, variant_id: Int32, func: Function?, dispatch_class: String?))
      candidates = [] of NamedTuple(type_id: Int32, type_ref: TypeRef, variant_id: Int32, func: Function?, dispatch_class: String?)

      if recv_desc.kind == HIR::TypeKind::Union
        mir_union_ref = convert_type(recv_type)
        if union_desc = @mir_module.get_union_descriptor(mir_union_ref)
          if ENV["DEBUG_VDISPATCH_UNION"]? && method_suffix == "next_power_of_two"
            variants = union_desc.variants.map(&.full_name).join(",")
            STDERR.puts "[VDISPATCH_UNION] union=#{union_desc.name} variants=#{variants}"
          end
          union_desc.variants.each do |variant|
            if variant.full_name == "Nil"
              next
            end
            if func = resolve_virtual_method_for_class(variant.full_name, method_suffix, arg_count)
              if ENV["DEBUG_VDISPATCH_UNION"]? && method_suffix == "next_power_of_two"
                STDERR.puts "[VDISPATCH_UNION] candidate=#{variant.full_name} func=#{func.name}"
              end
              candidates << {
                type_id: variant.type_id,
                type_ref: variant.type_ref,
                variant_id: variant.type_id,
                func: func,
                dispatch_class: nil
              }
            elsif (mir_type = @mir_module.type_registry.get_by_name(variant.full_name)) &&
                  !mir_type.is_value_type? &&
                  !subclasses_for(variant.full_name).empty?
              candidates << {
                type_id: variant.type_id,
                type_ref: variant.type_ref,
                variant_id: variant.type_id,
                func: nil,
                dispatch_class: variant.full_name
              }
            end
          end
        end
      elsif recv_desc.kind == HIR::TypeKind::Class
        base = recv_desc.name
        ([base] + subclasses_for(base)).each do |class_name|
          func_name = "#{class_name}##{method_suffix}"
          func = @mir_module.get_function(func_name) ||
                 resolve_virtual_method_for_class(class_name, method_suffix, arg_count)
          next unless func
          next unless mir_type = @mir_module.type_registry.get_by_name(class_name)
          candidates << {
            type_id: mir_type.id.to_i32,
            type_ref: TypeRef.new(mir_type.id),
            variant_id: mir_type.id.to_i32,
            func: func,
            dispatch_class: nil
          }
        end
      elsif recv_desc.kind == HIR::TypeKind::Module || recv_desc.kind == HIR::TypeKind::Generic
        seen = Set(String).new
        module_name = recv_desc.name
        if recv_desc.kind == HIR::TypeKind::Generic
          # Keep the full generic name (e.g., "Enumerable(Fiber)") so that
          # module_includers_for can narrow to type-parameter-specific includers.
          # This matches the original compiler's per-type-parameter including_types.
          # module_includers_for has fallback logic to the base name if needed.
        end
        includers = module_includers_for(module_name)
        if includers.empty?
          if outer = enclosing_class_for_module(module_name)
            includers = [outer]
          end
        end
        includers.each do |includer|
          ([includer] + subclasses_for(includer)).each do |class_name|
            next if seen.includes?(class_name)
            seen.add(class_name)
            func_name = "#{class_name}##{method_suffix}"
            func = @mir_module.get_function(func_name) ||
                   resolve_virtual_method_for_class(class_name, method_suffix, arg_count, allow_module_method: true)
            next unless func
            mir_type = ensure_reference_type_for_name(class_name) ||
              @mir_module.type_registry.get_by_name(class_name)
            next unless mir_type
            next if mir_type.is_value_type?
            candidates << {
              type_id: mir_type.id.to_i32,
              type_ref: TypeRef.new(mir_type.id),
              variant_id: mir_type.id.to_i32,
              func: func,
              dispatch_class: nil
            }
          end
        end
      end

      candidates
    end

    private def ensure_class_dispatch_for_union(
      class_name : String,
      method_suffix : String,
      receiver_type : TypeRef,
      call : HIR::Call
    ) : Function?
      dispatch_name = "__vdispatch__#{class_name}##{method_suffix}"
      if existing = @mir_module.get_function(dispatch_name)
        return existing
      end

      # Gather candidates from class hierarchy
      old_candidates = [] of NamedTuple(type_id: Int32, func: Function)
      ([class_name] + subclasses_for(class_name)).each do |name|
        if func = resolve_virtual_method_for_class(name, method_suffix, call.args.size)
          next unless mir_type = @mir_module.type_registry.get_by_name(name)
          next if mir_type.is_value_type?
          old_candidates << {type_id: mir_type.id.to_i32, func: func}
        end
      end
      return nil if old_candidates.empty?

      # Determine return type from candidates (not call.type which may be VOID)
      ret_type = convert_type(call.type)
      if ret_type == TypeRef::VOID
        old_candidates.each do |c|
          if c[:func].return_type != TypeRef::VOID
            ret_type = c[:func].return_type
            break
          end
        end
      end

      # Create dispatch function
      dispatch_func = @mir_module.create_function(dispatch_name, ret_type)
      param_values = [] of ValueId
      dispatch_func.add_param("recv", receiver_type)
      param_values << 0_u32
      call.args.each_with_index do |arg_id, idx|
        arg_type = @hir_value_types[arg_id]? || HIR::TypeRef::POINTER
        dispatch_func.add_param("arg#{idx}", convert_type(arg_type))
        param_values << (idx + 1).to_u32
      end

      dispatch_builder = Builder.new(dispatch_func)

      # Convert to unified candidate format (class dispatch = no unwrap needed)
      candidates = old_candidates.map do |c|
        VDispatchCandidate.new(
          type_id: c[:type_id],
          func: c[:func],
          type_ref: nil,        # No unwrap for class dispatch
          variant_id: nil,      # No unwrap for class dispatch
          dispatch_class: nil   # No nested dispatch
        )
      end

      # Use unified generator with Class kind
      generate_vdispatch_body(
        dispatch_func,
        dispatch_builder,
        param_values,
        candidates,
        VDispatchKind::Class,
        nil,   # No method_suffix needed (direct func calls)
        nil    # No HIR call needed (simple args)
      )

      dispatch_func
    end

    private def resolve_virtual_method_for_class(
      class_name : String,
      method_suffix : String,
      arg_count : Int32? = nil,
      allow_module_method : Bool = false
    ) : Function?
      cache_key = {class_name, method_suffix, arg_count, allow_module_method}
      if @resolve_virtual_cache.has_key?(cache_key)
        return @resolve_virtual_cache[cache_key]
      end
      result = _resolve_virtual_walk(class_name, method_suffix, arg_count, allow_module_method)
      @resolve_virtual_cache[cache_key] = result
      result
    end

    private def _resolve_virtual_walk(
      class_name : String,
      method_suffix : String,
      arg_count : Int32?,
      allow_module_method : Bool
    ) : Function?
      # Pre-compute the base method name (before '$') once
      base_method = if dollar = method_suffix.index('$')
                      method_suffix[0, dollar]
                    else
                      method_suffix
                    end

      current = class_name
      seen = Set(String).new
      while !current.empty? && !seen.includes?(current)
        seen.add(current)

        # Pre-compute exact name once per iteration (avoid repeated interpolation)
        exact_name = String.build(current.bytesize + 1 + method_suffix.bytesize) do |io|
          io << current; io << '#'; io << method_suffix
        end

        if func = @mir_module.get_function(exact_name)
          # Check for naming collision using class index instead of full scan
          prefix_with_underscore = String.build(exact_name.bytesize + 1) do |io|
            io << exact_name; io << '_'
          end
          longer_match = nil.as(Function?)
          if class_funcs = @functions_by_class[current]?
            class_funcs.each do |candidate|
              if candidate.name.starts_with?(prefix_with_underscore)
                if lm_prev = longer_match
                  longer_match = candidate if candidate.params.size > lm_prev.params.size
                else
                  longer_match = candidate
                end
              end
            end
          end
          if lm = longer_match
            func.params.each_with_index do |short_param, pi|
              if (dv = short_param.default_value) && pi < lm.params.size && lm.params[pi].default_value.nil?
                old_p = lm.params[pi]
                lm.params[pi] = Parameter.new(old_p.index, old_p.name, old_p.type, dv)
              end
            end
            return lm
          end
          return func
        end

        if allow_module_method
          module_name = String.build(current.bytesize + 1 + method_suffix.bytesize) do |io|
            io << current; io << '.'; io << method_suffix
          end
          if func = @mir_module.get_function(module_name)
            return func
          end
        end

        if arg_count
          # Use class index + pre-computed prefix (avoids O(N) full scan + GC from interpolation)
          instance_prefix = String.build(current.bytesize + 1 + base_method.bytesize) do |io|
            io << current; io << '#'; io << base_method
          end
          if class_funcs = @functions_by_class[current]?
            candidates = [] of Function
            class_funcs.each do |candidate|
              next unless candidate.name.starts_with?(instance_prefix)
              next unless candidate.params.size == arg_count + 1
              candidates << candidate
            end
            return candidates.first if candidates.size == 1
          end
          if allow_module_method
            module_prefix = String.build(current.bytesize + 1 + base_method.bytesize) do |io|
              io << current; io << '.'; io << base_method
            end
            if class_funcs2 = @functions_by_class[current]?
              candidates = [] of Function
              class_funcs2.each do |candidate|
                next unless candidate.name.starts_with?(module_prefix)
                next unless candidate.params.size == arg_count
                candidates << candidate
              end
              return candidates.first if candidates.size == 1
            end
          end
        end
        parent = @hir_module.class_parents[current]?
        current = parent || ""
      end
      nil
    end

    private def hir_type_name(type_ref : HIR::TypeRef?) : String
      return "unknown" unless type_ref
      if desc = @hir_module.get_type_descriptor(type_ref)
        return desc.name
      end
      # Map primitive type IDs to their names
      case type_ref
      when HIR::TypeRef::VOID    then "Void"
      when HIR::TypeRef::BOOL    then "Bool"
      when HIR::TypeRef::INT8    then "Int8"
      when HIR::TypeRef::INT16   then "Int16"
      when HIR::TypeRef::INT32   then "Int32"
      when HIR::TypeRef::INT64   then "Int64"
      when HIR::TypeRef::INT128  then "Int128"
      when HIR::TypeRef::UINT8   then "UInt8"
      when HIR::TypeRef::UINT16  then "UInt16"
      when HIR::TypeRef::UINT32  then "UInt32"
      when HIR::TypeRef::UINT64  then "UInt64"
      when HIR::TypeRef::UINT128 then "UInt128"
      when HIR::TypeRef::FLOAT32 then "Float32"
      when HIR::TypeRef::FLOAT64 then "Float64"
      when HIR::TypeRef::CHAR    then "Char"
      when HIR::TypeRef::STRING  then "String"
      when HIR::TypeRef::NIL     then "Nil"
      when HIR::TypeRef::SYMBOL  then "Symbol"
      when HIR::TypeRef::POINTER then "Pointer"
      else                            type_ref.id.to_s
      end
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
        begin
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
          is_arg_union = is_union_type?(arg_type)
          if arg_type != param_type && is_param_union && !is_arg_union
            # Wrap concrete value in union type
            variant_id = get_union_variant_id(arg_type, param_type)
            # Debug disabled for performance:
            # STDERR.puts "[MIR-COERCE] Wrapping arg #{mir_arg} type #{arg_type.id} into union type #{param_type.id} (variant #{variant_id})"
            wrapped = builder.union_wrap(mir_arg, variant_id, param_type)
            result << wrapped
          else
            # Debug disabled for performance:
            # if func.name.includes?("format_gutter")
            #   STDERR.puts "[MIR-COERCE] No wrap: arg_type=#{arg_type.id} param_type=#{param_type.id} is_param_union=#{is_param_union}"
            # end
            result << mir_arg
          end
        rescue ex : IndexError
          raise "Index error in coerce_call_args for #{func.name} at arg #{idx}: mir_args.size=#{mir_args.size} params.size=#{params.size} hir_args.size=#{hir_args.size}\n#{ex.message}"
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
            return desc.kind == HIR::TypeKind::Union
          end
        end
      end
      false
    end

    # Get the variant ID for a concrete type when wrapping into a union
    private def get_union_variant_id(concrete_type : TypeRef, union_type : TypeRef? = nil) : Int32
      # Look up from union descriptor if available (authoritative source)
      if union_type
        if descriptor = @mir_module.union_descriptors[union_type]?
          descriptor.variants.each do |variant|
            if variant.type_ref == concrete_type
              return variant.type_id
            end
          end
          # For pointer-like types (classes), try matching against POINTER variant
          if concrete_type != TypeRef::NIL
            descriptor.variants.each do |variant|
              if variant.type_ref == TypeRef::POINTER
                return variant.type_id
              end
            end
          end
        end
      end
      # Fallback: Nil is variant 1, other concrete types are variant 0
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
      if slot_type = @stack_slot_types[value]?
        value = builder.load(value, slot_type)
      end
      src_hir_type = @hir_value_types[cast.value]? || HIR::TypeRef::POINTER
      dst_hir_type = cast.target_type

      src_type = convert_type(src_hir_type)
      dst_type = convert_type(dst_hir_type)

      # Union unwrap: cast union to concrete variant
      if is_union_type?(src_type) && !is_union_type?(dst_type)
        if descriptor = @mir_module.get_union_descriptor(src_type)
          if variant = descriptor.variants.find { |v| v.type_ref == dst_type }
            unwrap = UnionUnwrap.new(builder.next_id, dst_type, value, variant.type_id, cast.safe)
            return builder.emit(unwrap)
          end
        end
      end

      # Union wrap: cast concrete value into union variant
      if !is_union_type?(src_type) && is_union_type?(dst_type)
        if descriptor = @mir_module.get_union_descriptor(dst_type)
          if variant = descriptor.variants.find { |v| v.type_ref == src_type }
            return builder.union_wrap(value, variant.type_id, dst_type)
          end
        end
      end

      # No-op cast
      if src_type == dst_type
        return value
      end

      # Helpers (HIR types carry signedness via Int*/UInt*)
      int_like = ->(t : HIR::TypeRef) do
        case t
        when HIR::TypeRef::BOOL,
             HIR::TypeRef::INT8, HIR::TypeRef::INT16, HIR::TypeRef::INT32, HIR::TypeRef::INT64, HIR::TypeRef::INT128,
             HIR::TypeRef::UINT8, HIR::TypeRef::UINT16, HIR::TypeRef::UINT32, HIR::TypeRef::UINT64, HIR::TypeRef::UINT128,
             HIR::TypeRef::CHAR
          true
        else
          false
        end
      end

      float_like = ->(t : HIR::TypeRef) do
        t == HIR::TypeRef::FLOAT32 || t == HIR::TypeRef::FLOAT64
      end

      signed_int = ->(t : HIR::TypeRef) do
        case t
        when HIR::TypeRef::INT8, HIR::TypeRef::INT16, HIR::TypeRef::INT32, HIR::TypeRef::INT64, HIR::TypeRef::INT128
          true
        else
          false
        end
      end

      # Unsafe bitcast for numeric types of the same size (unsafe_as semantics).
      if !cast.safe
        if (int_like.call(src_hir_type) && float_like.call(dst_hir_type)) ||
           (float_like.call(src_hir_type) && int_like.call(dst_hir_type))
          if type_size(src_hir_type) == type_size(dst_hir_type)
            return builder.cast(CastKind::Bitcast, value, dst_type)
          end
        end
      end

      kind = if src_type == TypeRef::POINTER && int_like.call(dst_hir_type)
               CastKind::PtrToInt
             elsif dst_type == TypeRef::POINTER && int_like.call(src_hir_type)
               CastKind::IntToPtr
             elsif int_like.call(src_hir_type) && int_like.call(dst_hir_type)
               src_size = type_size(src_hir_type)
               dst_size = type_size(dst_hir_type)
               if dst_size < src_size
                 CastKind::Trunc
               elsif dst_size > src_size
                 signed_int.call(src_hir_type) ? CastKind::SExt : CastKind::ZExt
               else
                 CastKind::Bitcast
               end
             elsif float_like.call(src_hir_type) && float_like.call(dst_hir_type)
               src_size = type_size(src_hir_type)
               dst_size = type_size(dst_hir_type)
               dst_size < src_size ? CastKind::FPTrunc : CastKind::FPExt
             elsif float_like.call(src_hir_type) && int_like.call(dst_hir_type)
               signed_int.call(dst_hir_type) ? CastKind::FPToSI : CastKind::FPToUI
             elsif int_like.call(src_hir_type) && float_like.call(dst_hir_type)
               signed_int.call(src_hir_type) ? CastKind::SIToFP : CastKind::UIToFP
             else
               CastKind::Bitcast
             end

      result = builder.cast(kind, value, dst_type)
      result
    end

    private def lower_is_a(isa : HIR::IsA) : ValueId
      builder = @builder.not_nil!
      mir_check_type = convert_type(isa.check_type)

      # Check if the value's type is already known (concrete, non-union).
      # If so, resolve the is_a? check statically to avoid loading type_id
      # from a raw value (e.g. Int32 treated as pointer → segfault).
      if hir_value_type = @hir_value_types[isa.value]?
        value_desc = @hir_module.get_type_descriptor(hir_value_type)
        mir_value_type_for_static = convert_type(hir_value_type)
        # Only resolve statically for truly concrete types: primitives, or
        # class types that have NO subclasses (leaf classes). Classes with
        # subclasses can have any runtime type_id → need runtime check.
        has_subclasses = false
        if value_desc && value_desc.kind != HIR::TypeKind::Union && !hir_value_type.primitive?
          val_mir_type = @mir_module.type_registry.get(mir_value_type_for_static)
          if val_mir_type
            has_subclasses = !subclasses_for(val_mir_type.name).empty?
          end
        end
        is_concrete = hir_value_type.primitive? ||
                      (value_desc && value_desc.kind != HIR::TypeKind::Union && !has_subclasses)
        if is_concrete
          # Concrete leaf type — resolve statically
          # Collect matching type_ids (check_type + subclasses)
          matching_type_ids = Set(UInt32).new
          matching_type_ids << mir_check_type.id
          if check_mir_type = @mir_module.type_registry.get(mir_check_type)
            subclasses_for(check_mir_type.name).each do |sub_name|
              if sub_mir_type = @mir_module.type_registry.get_by_name(sub_name)
                matching_type_ids << sub_mir_type.id
              end
            end
          end
          is_match = matching_type_ids.includes?(mir_value_type_for_static.id)
          return builder.const_int(is_match ? 1_i64 : 0_i64, TypeRef::BOOL)
        end
      end

      obj = get_value(isa.value)

      # For ptr-typed values (class instances represented as raw pointers),
      # nilable checks must use null comparison instead of loading type_id
      # from the object header (which would crash on null ptr).
      # is_a?(Nil) → ptr == null, is_a?(SomeClass) → ptr != null
      hir_val_type = @hir_value_types[isa.value]?
      mir_val_type = hir_val_type ? convert_type(hir_val_type) : nil
      val_is_ptr_type = false
      if mir_val_type
        val_desc = @mir_module.type_registry.get(mir_val_type)
        val_is_ptr_type = mir_val_type == TypeRef::POINTER ||
                          mir_val_type == TypeRef::NIL ||
                          (val_desc && (val_desc.kind == MIR::TypeKind::Reference ||
                                        val_desc.kind == MIR::TypeKind::Struct)) ||
                          (val_desc.nil? && !mir_val_type.primitive?)  # Not in registry and not primitive → ptr
      end
      if val_is_ptr_type && mir_check_type == TypeRef::NIL
        # is_a?(Nil) on a ptr value → compare to null
        null_val = builder.const_int(0_i64, TypeRef::POINTER)
        return builder.eq(obj, null_val)
      elsif val_is_ptr_type && mir_check_type != TypeRef::NIL
        # ptr != null is ONLY valid when the static type matches the check type
        # (i.e., simple nilable checks like `x : Foo?` → is_a?(Foo)).
        # For class hierarchies (Base → is_a?(SubClass)), need runtime type_id check.
        if mir_val_type && mir_val_type.id == mir_check_type.id
          null_val = builder.const_int(0_i64, TypeRef::POINTER)
          return builder.ne(obj, null_val)
        end
        # Otherwise fall through to runtime type_id check below
      end

      # Collect all type_ids that should match: the check_type itself
      # plus all its subclasses (for parent type checks like is_a?(Base))
      matching_type_ids = [] of UInt32
      matching_type_ids << mir_check_type.id

      # Find check type name and add subclass type_ids
      if check_mir_type = @mir_module.type_registry.get(mir_check_type)
        check_name = check_mir_type.name
        subclasses_for(check_name).each do |sub_name|
          if sub_mir_type = @mir_module.type_registry.get_by_name(sub_name)
            matching_type_ids << sub_mir_type.id unless matching_type_ids.includes?(sub_mir_type.id)
          end
        end
      end

      # Load type_id from object header (offset 0, i32)
      type_id_ptr = builder.gep(obj, [0_u32], TypeRef::POINTER)
      loaded_type_id = builder.load(type_id_ptr, TypeRef::INT32)

      # Compare against all matching type_ids with OR chain
      if matching_type_ids.size == 1
        expected = builder.const_int(matching_type_ids[0].to_i64, TypeRef::INT32)
        builder.eq(loaded_type_id, expected)
      else
        # Multiple possible type_ids — OR chain
        first_expected = builder.const_int(matching_type_ids[0].to_i64, TypeRef::INT32)
        result = builder.eq(loaded_type_id, first_expected)
        matching_type_ids[1..].each do |tid|
          expected = builder.const_int(tid.to_i64, TypeRef::INT32)
          check = builder.eq(loaded_type_id, expected)
          result = builder.bit_or(result, check, TypeRef::BOOL)
        end
        result
      end
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
      # Load from stack slot when copy reads a mutable local / block param.
      source = get_value(copy.source)
      if slot_type = @stack_slot_types[source]?
        builder = @builder.not_nil!
        return builder.load(source, slot_type)
      end
      source
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

    private def lower_func_pointer(fp : HIR::FuncPointer) : ValueId
      builder = @builder.not_nil!
      mir_fp = MIR::FuncPointer.new(builder.next_id, TypeRef::POINTER, fp.func_name)
      builder.emit(mir_fp)
      mir_fp.id
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Yield Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_yield(yld : HIR::Yield) : ValueId
      builder = @builder.not_nil!
      block_param_id = @current_block_param_id
      unless block_param_id
        return builder.const_nil
      end
      # Yield becomes indirect call through block parameter.
      # We treat the block param as a Proc value and emit an indirect call.
      args = [] of ValueId
      yld.args.each do |arg|
        arg_type = @hir_value_types[arg]?
        next if arg_type == HIR::TypeRef::VOID
        next unless @value_map.has_key?(arg)
        args << get_value(arg)
      end
      block_val = get_value(block_param_id)
      block_type = @hir_value_types[block_param_id]? || HIR::TypeRef::POINTER
      block_desc = @hir_module.get_type_descriptor(block_type)
      is_ptr = block_type == HIR::TypeRef::POINTER || (block_desc && block_desc.kind == HIR::TypeKind::Proc)
      unless is_ptr
        if block_type == HIR::TypeRef::VOID
          return builder.const_nil
        end
        block_val = builder.cast(CastKind::IntToPtr, block_val, TypeRef::POINTER)
      end
      builder.call_indirect(block_val, args, convert_type(yld.type))
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

    private def infer_block_param_id(hir_func : HIR::Function) : HIR::ValueId?
      # Prefer explicit Proc-typed param if present.
      hir_func.params.reverse_each do |param|
        if desc = @hir_module.get_type_descriptor(param.type)
          return param.id if desc.kind == HIR::TypeKind::Proc
        end
      end
      # Fallback: use the last parameter as the block param.
      hir_func.params.last?.try(&.id)
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Class Variable Lowering
    # ─────────────────────────────────────────────────────────────────────────

    private def lower_classvar_get(cv : HIR::ClassVarGet) : ValueId
      builder = @builder.not_nil!
      extern = @hir_module.get_extern_global(cv.class_name, cv.var_name)
      global_name = extern ? extern.real_name : HIRToMIRLowering.class_var_global_name(cv.class_name, cv.var_name)
      hir_type = (extern && cv.type == HIR::TypeRef::VOID) ? extern.type : cv.type
      builder.global_load(global_name, convert_type(hir_type))
    end

    private def lower_classvar_set(cv : HIR::ClassVarSet) : ValueId
      builder = @builder.not_nil!
      value = get_value(cv.value)
      extern = @hir_module.get_extern_global(cv.class_name, cv.var_name)
      global_name = extern ? extern.real_name : HIRToMIRLowering.class_var_global_name(cv.class_name, cv.var_name)
      hir_type = (extern && cv.type == HIR::TypeRef::VOID) ? extern.type : cv.type
      builder.global_store(global_name, value, convert_type(hir_type))
      value
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
      if union_hir_type = @hir_value_types[unwrap.union_value]?
        union_mir_type = convert_type(union_hir_type)
        if descriptor = @mir_module.union_descriptors[union_mir_type]?
          if variant = descriptor.variants.find { |v| v.type_id == unwrap.variant_type_id }
            result_type = variant.type_ref
            if nested = @mir_module.union_descriptors[result_type]?
              if nested_variant = nested.variants.find { |v| v.type_ref != TypeRef::NIL && v.type_ref != TypeRef::VOID }
                result_type = nested_variant.type_ref
              end
            end
            if ENV.has_key?("DEBUG_UNION_UNWRAP")
              STDERR.puts "[UNION_UNWRAP] union_type=#{union_mir_type.id} variant_id=#{unwrap.variant_type_id} variant_type=#{variant.type_ref.id}"
            end
          elsif ENV.has_key?("DEBUG_UNION_UNWRAP")
            STDERR.puts "[UNION_UNWRAP] union_type=#{union_mir_type.id} variant_id=#{unwrap.variant_type_id} variant_type=nil"
          end
        end
      elsif ENV.has_key?("DEBUG_UNION_UNWRAP")
        STDERR.puts "[UNION_UNWRAP] missing hir type for union_value=#{unwrap.union_value} unwrap_type=#{unwrap.type} result_type=#{result_type.id}"
      end
      if ENV.has_key?("DEBUG_UNION_UNWRAP")
        if descriptor = @mir_module.union_descriptors[result_type]?
          variants = descriptor.variants.map { |v| v.type_ref.id }.join(",")
          STDERR.puts "[UNION_UNWRAP] union_value=#{unwrap.union_value} unwrap_type=#{unwrap.type} result_type=#{result_type.id} has_union=true variants=#{variants}"
        else
          STDERR.puts "[UNION_UNWRAP] union_value=#{unwrap.union_value} unwrap_type=#{unwrap.type} result_type=#{result_type.id} has_union=false"
        end
      end

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

      # Determine memory strategy from HIR lifetime tag.
      # Array is a reference type — default to GC (heap) for safety.
      # The memory optimizer can demote to Stack when escape analysis proves it safe.
      strategy = case arr.lifetime
                 when HIR::LifetimeTag::StackLocal
                   # Conservative: Array commonly escapes (stored in ivars, returned, etc.)
                   # Only use Stack if escape analysis explicitly confirms non-escape.
                   # For now, default to GC since HIR lacks array escape analysis.
                   MIR::MemoryStrategy::GC
                 when HIR::LifetimeTag::ArgEscape
                   MIR::MemoryStrategy::Slab
                 when HIR::LifetimeTag::HeapEscape
                   MIR::MemoryStrategy::ARC
                 when HIR::LifetimeTag::GlobalEscape
                   MIR::MemoryStrategy::AtomicARC
                 else
                   MIR::MemoryStrategy::GC
                 end

      # Create MIR ArrayLiteral instruction
      mir_arr = MIR::ArrayLiteral.new(
        builder.next_id,
        element_type,
        elements,
        strategy
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

    private def lower_array_set_size(set_size : HIR::ArraySetSize) : ValueId
      builder = @builder.not_nil!
      array_val = get_value(set_size.array_value)
      size_val = get_value(set_size.size_value)

      mir_set_size = MIR::ArraySetSize.new(
        builder.next_id,
        array_val,
        size_val
      )
      builder.emit(mir_set_size)
    end

    private def lower_array_new(array_new : HIR::ArrayNew) : ValueId
      builder = @builder.not_nil!
      capacity_val = get_value(array_new.capacity_value)
      mir_new = MIR::ArrayNew.new(
        builder.next_id,
        convert_type(array_new.element_type),
        capacity_val
      )
      builder.emit(mir_new)
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
        # For struct element types, the data is inline — GEP result IS the value pointer
        if hir_type_is_struct?(load.type)
          @inline_struct_ptrs << load.id
          gep
        else
          builder.load(gep, result_type)
        end
      else
        # ptr.value - direct access
        # For struct types, the pointer already points to inline struct data.
        # Return the pointer unchanged (no load). Matches Crystal value semantics.
        if hir_type_is_struct?(load.type)
          @inline_struct_ptrs << load.id
          ptr
        else
          builder.load(ptr, result_type)
        end
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
      builder.gep_dynamic(ptr, offset, elem_type, add.element_byte_size)
    end

    private def lower_pointer_realloc(realloc : HIR::PointerRealloc) : ValueId
      builder = @builder.not_nil!

      ptr = get_value(realloc.pointer)
      new_size = get_value(realloc.new_size)

      # Crystal's Pointer#realloc(count) expects element count, but C realloc expects bytes.
      # Multiply count by element size. Determine element size from the pointer's type descriptor.
      elem_size = 8 # default: pointer-sized elements (class instances)
      ptr_type = realloc.type
      if desc = @hir_module.get_type_descriptor(ptr_type)
        if desc.name.starts_with?("Pointer(")
          elem_name = desc.name[8, desc.name.size - 9]
          elem_size = case elem_name
                      when "UInt8", "Int8", "Bool" then 1
                      when "UInt16", "Int16"       then 2
                      when "UInt32", "Int32", "Float32", "Char" then 4
                      when "UInt64", "Int64", "Float64" then 8
                      when "UInt128", "Int128" then 16
                      else
                        # For struct types, look up actual size from type registry
                        if elem_mir_type = @mir_module.type_registry.get_by_name(elem_name)
                          elem_mir_type.size > 0 ? elem_mir_type.size.to_i32 : 8
                        else
                          8 # class/reference instances are pointers (8 bytes)
                        end
                      end
        end
      end

      # Multiply element count by element size to get byte count
      elem_size_val = builder.const_int(elem_size.to_i64, TypeRef::INT64)
      # Extend new_size to i64 for multiplication
      size_i64 = builder.cast(CastKind::SExt, new_size, TypeRef::INT64)
      byte_count = builder.mul(size_i64, elem_size_val, TypeRef::INT64)

      args = [ptr, byte_count]
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
      if mapped = @value_map[hir_id]?
        return mapped
      end
      0_u32
    end

    private def record_stack_slot(slot : ValueId, type : TypeRef)
      @stack_slot_values.add(slot)
      @stack_slot_types[slot] = type
    end

    private def default_value_for_type(builder : Builder, type : TypeRef) : ValueId?
      case type
      when TypeRef::BOOL
        builder.const_bool(false)
      when TypeRef::INT8, TypeRef::INT16, TypeRef::INT32, TypeRef::INT64, TypeRef::INT128,
           TypeRef::UINT8, TypeRef::UINT16, TypeRef::UINT32, TypeRef::UINT64, TypeRef::UINT128,
           TypeRef::CHAR
        builder.const_int(0_i64, type)
      when TypeRef::FLOAT32, TypeRef::FLOAT64
        builder.const_float(0.0, type)
      when TypeRef::POINTER
        builder.const_nil_typed(TypeRef::POINTER)
      else
        nil
      end
    end

    private def convert_type(hir_type : HIR::TypeRef) : TypeRef
      # Map HIR type IDs to MIR type IDs
      # Note: HIR and MIR have DIFFERENT layouts! HIR: BOOL=1, MIR: NIL=1, BOOL=2
      result = case hir_type
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
      when HIR::TypeRef::POINTER then TypeRef::POINTER
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

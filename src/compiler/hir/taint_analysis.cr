# Taint Analysis for HIR
#
# Propagates taint information (ThreadShared, FFIExposed, Cyclic, Mutable)
# through the HIR graph. Can optionally use type information from semantic analysis.
#
# See docs/codegen_architecture.md Section 7 for full specification.

require "./hir"

module Crystal::HIR
  # Abstract interface for type information (to avoid circular deps)
  module TypeInfoProvider
    abstract def class_names : Array(String)
    abstract def instance_var_types(class_name : String) : Hash(String, String)

    def acyclic_type_names : Set(String)
      Set(String).new
    end

    def type_name_for(type_ref : TypeRef) : String?
      nil
    end

    def type_kind_for(type_ref : TypeRef) : TypeKind?
      nil
    end
  end

  # Taint analyzer using worklist algorithm
  class TaintAnalyzer

    # Known FFI methods that expose values to C
    FFI_METHODS = Set{
      "to_unsafe", "pointer", "as_pointer",
    }

    # Methods that indicate thread sharing
    SPAWN_METHODS = Set{
      "spawn",
    }

    # Channel-related type names
    CHANNEL_TYPES = Set{
      "Channel", "Fiber", "Mutex", "Atomic",
    }

    getter function : Function
    getter cyclic_types : Set(String)

    @worklist : Deque(ValueId)
    @definitions : Hash(ValueId, Value)
    @users : Hash(ValueId, Array(ValueId))
    @value_blocks : Hash(ValueId, BlockId)

    # Optional: type info provider for cycle detection
    @type_info : TypeInfoProvider?
    @effect_provider : MethodEffectProvider?

    def initialize(
      @function : Function,
      @type_info : TypeInfoProvider? = nil,
      @effect_provider : MethodEffectProvider? = nil
    )
      @cyclic_types = Set(String).new
      @worklist = Deque(ValueId).new
      @definitions = Hash(ValueId, Value).new
      @users = Hash(ValueId, Array(ValueId)).new { |h, k| h[k] = [] of ValueId }
      @value_blocks = {} of ValueId => BlockId

      # Pre-compute cyclic types if we have type info
      if ti = @type_info
        detect_cyclic_types(ti)
      end
    end

    # Alternative: initialize with explicit cyclic types (for testing)
    def initialize(
      @function : Function,
      @cyclic_types : Set(String),
      @effect_provider : MethodEffectProvider? = nil
    )
      @type_info = nil
      @worklist = Deque(ValueId).new
      @definitions = Hash(ValueId, Value).new
      @users = Hash(ValueId, Array(ValueId)).new { |h, k| h[k] = [] of ValueId }
      @value_blocks = {} of ValueId => BlockId
    end

    # Run taint analysis
    def analyze
      # Phase 1: Build dependency graph
      build_dependency_graph

      # Phase 2: Seed taints from known sources
      seed_taints

      # Phase 3: Propagate taints
      propagate_taints
    end

    # =========================================================================
    # CYCLE DETECTION
    # =========================================================================

    # Detect types with cyclic references using DFS on type graph
    private def detect_cyclic_types(type_info : TypeInfoProvider)
      visited = Set(String).new
      in_stack = Set(String).new

      # Build type graph: class_name → field types
      type_graph = {} of String => Array(String)
      type_info.class_names.each do |class_name|
        field_types = type_info.instance_var_types(class_name).values.compact.flat_map do |type_ann|
          extract_type_references(type_ann)
        end
        type_graph[class_name] = field_types.uniq
      end

      type_graph.each_key do |name|
        unless visited.includes?(name)
          dfs_find_cycles(name, type_graph, visited, in_stack)
        end
      end

      # Allow user override via @[Acyclic]
      type_info.acyclic_type_names.each do |acyclic|
        @cyclic_types.delete(acyclic)
      end
    end

    private def dfs_find_cycles(
      type_name : String,
      type_graph : Hash(String, Array(String)),
      visited : Set(String),
      in_stack : Set(String)
    )
      visited.add(type_name)
      in_stack.add(type_name)

      # Check field types for cycles
      if field_types = type_graph[type_name]?
        field_types.each do |field_type|
          if in_stack.includes?(field_type)
            # Found cycle!
            @cyclic_types.add(type_name)
            @cyclic_types.add(field_type)
          elsif !visited.includes?(field_type) && type_graph.has_key?(field_type)
            dfs_find_cycles(field_type, type_graph, visited, in_stack)
          end
        end
      end

      in_stack.delete(type_name)
    end

    # Extract all referenced type names from an annotation like:
    # "Array(Hash(String, Node?))" → ["Array", "Hash", "String", "Node"]
    private def extract_type_references(type_ann : String) : Array(String)
      refs = [] of String
      current = ""

      type_ann.each_char do |char|
        if char.ascii_alphanumeric? || char == '_' || char == ':'
          current += char
        else
          unless current.empty?
            refs << current
            current = ""
          end
        end
      end

      refs << current unless current.empty?
      refs
    end

    # =========================================================================
    # DEPENDENCY GRAPH
    # =========================================================================

    private def build_dependency_graph
      @function.blocks.each do |block|
        block.instructions.each do |value|
          @definitions[value.id] = value
          @value_blocks[value.id] = block.id
          record_uses(value)
        end
      end
    end

    private def record_uses(value : Value)
      each_operand(value) do |operand|
        @users[operand] << value.id
      end
    end

    private def each_operand(value : Value, &block : ValueId ->)
      case value
      when Copy
        yield value.source
      when BinaryOperation
        yield value.left
        yield value.right
      when UnaryOperation
        yield value.operand
      when Call
        if recv = value.receiver
          yield recv
        end
        value.args.each { |arg| yield arg }
      when FieldGet
        yield value.object
      when FieldSet
        yield value.object
        yield value.value
      when IndexGet
        yield value.object
        yield value.index
      when IndexSet
        yield value.object
        yield value.index
        yield value.value
      when MakeClosure
        value.captures.each { |cap| yield cap.value_id }
      when Yield
        value.args.each { |arg| yield arg }
      when Phi
        value.incoming.each { |(_, val)| yield val }
      when Cast
        yield value.value
      when IsA
        yield value.value
      when Allocate
        value.constructor_args.each { |arg| yield arg }
      when ExternCall
        value.args.each { |arg| yield arg }
      end
    end

    # =========================================================================
    # TAINT SEEDING
    # =========================================================================

    private def seed_taints
      @definitions.each_value do |value|
        case value
        # Allocations of cyclic types get Cyclic taint
        when Allocate
          type_name = get_type_name(value.type)
          if cyclic_type_name?(type_name)
            mark_taint(value.id, Taint::Cyclic)
          end

        # Calls that might expose to FFI or spawn
        when Call
          if effects = @effect_provider.try(&.method_effects_for(value.method_name))
            if effects.thread_shared
              if recv = value.receiver
                mark_taint(recv, Taint::ThreadShared)
              end
              value.args.each { |arg| mark_taint(arg, Taint::ThreadShared) }
            end
            if effects.ffi_exposed
              if recv = value.receiver
                mark_taint(recv, Taint::FFIExposed)
              end
              value.args.each { |arg| mark_taint(arg, Taint::FFIExposed) }
            end
          end

          if is_ffi_method?(value.method_name)
            # to_unsafe-style methods expose the receiver to FFI
            if recv = value.receiver
              mark_taint(recv, Taint::FFIExposed)
            else
              value.args.each { |arg| mark_taint(arg, Taint::FFIExposed) }
            end
          end

          case value.method_name
          when "spawn"
            # spawn shares captured values; only treat closure arguments as shared
            if value.receiver.nil?
              value.args.each do |arg|
                if defn = @definitions[arg]?
                  if defn.is_a?(MakeClosure)
                    mark_taint(arg, Taint::ThreadShared)
                  end
                end
              end
              if blk = value.block
                block_capture_values(blk).each do |captured|
                  mark_taint(captured, Taint::ThreadShared)
                end
              end
            end
          when "send"
            # Channel send shares the argument across fibers
            if recv = value.receiver
              mark_taint(recv, Taint::ThreadShared)
              value.args.each { |arg| mark_taint(arg, Taint::ThreadShared) }
            end
          when "receive"
            # Receiving from a channel yields a shared value
            if recv = value.receiver
              mark_taint(recv, Taint::ThreadShared)
            end
          end

          if is_channel_type?(value.method_name)
            # Value passed through channel is thread-shared
            value.args.each { |arg| mark_taint(arg, Taint::ThreadShared) }
          end
        when ExternCall
          # Arguments passed to C are exposed to FFI
          value.args.each { |arg| mark_taint(arg, Taint::FFIExposed) }

        # Class variables are implicitly thread-shared
        when ClassVarGet, ClassVarSet
          # The class var value is potentially shared
          if value.is_a?(ClassVarSet)
            mark_taint(value.value, Taint::ThreadShared)
          end
          mark_taint(value.id, Taint::ThreadShared)

        # Closures capture mutable state
        when MakeClosure
          value.captures.each do |cap|
            if cap.by_reference
              mark_taint(cap.value_id, Taint::Mutable)
            end
          end
        end
      end

      # All allocations start as potentially mutable
      @definitions.each_value do |value|
        if value.is_a?(Allocate)
          mark_taint(value.id, Taint::Mutable)
        end
      end
    end

    # =========================================================================
    # TAINT PROPAGATION
    # =========================================================================

    private def propagate_taints
      while val_id = @worklist.shift?
        value = @definitions[val_id]?
        next unless value

        current_taints = value.taints

        # Propagate to users
        @users[val_id].each do |user_id|
          user = @definitions[user_id]?
          next unless user

          propagate_to_user(value, user, current_taints)
        end

        propagate_backwards(value, current_taints)
      end
    end

    private def propagate_backwards(value : Value, taints : Taint)
      flow_taints = taints & (Taint::ThreadShared | Taint::FFIExposed)
      return if flow_taints == Taint::None

      case value
      when Copy
        merge_taints(value.source, flow_taints)
      when Phi
        value.incoming.each { |(_, val)| merge_taints(val, flow_taints) }
      when FieldGet
        merge_taints(value.object, flow_taints)
      when IndexGet
        merge_taints(value.object, flow_taints)
      when Cast
        merge_taints(value.value, flow_taints)
      when IsA
        merge_taints(value.value, flow_taints)
      when UnaryOperation
        merge_taints(value.operand, flow_taints)
      when BinaryOperation
        merge_taints(value.left, flow_taints)
        merge_taints(value.right, flow_taints)
      end
    end

    private def block_capture_values(block_id : BlockId) : Array(ValueId)
      captures = Set(ValueId).new
      closure_scope = @function.get_block(block_id).scope

      @function.blocks.each do |block|
        next unless scope_within?(block.scope, closure_scope)
        block.instructions.each do |inst|
          each_operand(inst) do |operand|
            def_block = @value_blocks[operand]?
            if def_block
              def_scope = @function.get_block(def_block).scope
              next if scope_within?(def_scope, closure_scope)
            end
            captures.add(operand)
          end
        end
      end

      captures.to_a
    end

    private def scope_within?(scope_id : ScopeId, root_scope : ScopeId) : Bool
      current = scope_id
      loop do
        return true if current == root_scope
        scope = @function.get_scope(current)
        parent = scope.parent
        return false unless parent
        current = parent
      end
    end

    private def propagate_to_user(source : Value, user : Value, taints : Taint)
      case user
      when Copy
        # Copy inherits all taints
        merge_taints(user.id, taints)

      when Phi
        # Phi merges taints from all incoming
        merge_taints(user.id, taints)

      when FieldSet
        # If value is tainted, the containing object gets tainted
        if source.id == user.value
          merge_taints(user.object, taints)
        end

      when IndexSet
        # Value taints propagate to container
        if source.id == user.value
          merge_taints(user.object, taints)
        end

      when Call
        # If receiver is thread-shared, result might be too
        if recv = user.receiver
          if recv == source.id && taints.thread_shared?
            merge_taints(user.id, Taint::ThreadShared)
          end
        end

      when MakeClosure
        # Captured values taint the closure
        if source.taints != Taint::None
          merge_taints(user.id, source.taints)
        end

        # REVERSE PROPAGATION: If the closure itself is ThreadShared,
        # all captured values become ThreadShared too (they'll be accessed
        # from another fiber/thread via the closure)
        if user.taints.thread_shared?
          user.captures.each do |cap|
            merge_taints(cap.value_id, Taint::ThreadShared)
          end
        end
      end
    end

    private def mark_taint(value_id : ValueId, taint : Taint)
      value = @definitions[value_id]?
      return unless value

      unless value.taints.includes?(taint)
        value.taints |= taint
        @worklist << value_id
      end
    end

    private def merge_taints(value_id : ValueId, taints : Taint)
      value = @definitions[value_id]?
      return unless value

      new_taints = value.taints | taints
      if new_taints != value.taints
        value.taints = new_taints
        @worklist << value_id
      end
    end

    # =========================================================================
    # TYPE HELPERS
    # =========================================================================

    private def get_type_name(type_ref : TypeRef) : String
      if ti = @type_info
        if name = ti.type_name_for(type_ref)
          return name
        end
      end

      # Fallback: use simple mapping from TypeRef ID
      case type_ref
      when TypeRef::STRING then "String"
      when TypeRef::INT32  then "Int32"
      when TypeRef::INT64  then "Int64"
      when TypeRef::BOOL   then "Bool"
      else
        "Type#{type_ref.id}"
      end
    end

    private def cyclic_type_name?(name : String) : Bool
      return true if @cyclic_types.includes?(name)

      # Check if it's a collection type - only cyclic if element type is cyclic
      if name.starts_with?("Array(") || name.starts_with?("Hash(") || name.starts_with?("Tuple(")
        # Extract element type(s) and check if any could form cycles
        element_types = extract_generic_params(name)
        return element_types.any? { |elem| could_be_cyclic?(elem) }
      end

      # Nilable types: T? is cyclic only if T is cyclic
      if name.ends_with?("?")
        base = name.rchop("?")
        return could_be_cyclic?(base)
      end

      false
    end

    # Extract generic type parameters from "Array(T)" → ["T"]
    # or "Hash(K, V)" → ["K", "V"]
    private def extract_generic_params(name : String) : Array(String)
      paren_start = name.index('(')
      return [] of String unless paren_start

      paren_end = name.rindex(')')
      return [] of String unless paren_end

      inner = name[(paren_start + 1)...paren_end]

      # Simple split by comma (doesn't handle nested generics perfectly, but good enough)
      # For nested like Array(Hash(K,V)), we'd need proper parsing
      params = [] of String
      depth = 0
      current = ""

      inner.each_char do |c|
        case c
        when '('
          depth += 1
          current += c
        when ')'
          depth -= 1
          current += c
        when ','
          if depth == 0
            params << current.strip
            current = ""
          else
            current += c
          end
        else
          current += c
        end
      end
      params << current.strip unless current.empty?

      params
    end

    # Check if a type could potentially form reference cycles
    private def could_be_cyclic?(type_name : String) : Bool
      # Already known cyclic
      return true if @cyclic_types.includes?(type_name)

      # Primitive types cannot form cycles
      return false if primitive_type?(type_name)

      # User-defined reference types COULD form cycles (conservative for unknown types)
      # But only if they have fields that reference back
      # For now, check if it's in our cyclic_types set
      @cyclic_types.includes?(type_name)
    end

    # Check if a type name represents a primitive (non-reference) type
    private def primitive_type?(name : String) : Bool
      PRIMITIVE_TYPES.includes?(name)
    end

    # Primitive types that cannot form reference cycles
    PRIMITIVE_TYPES = Set{
      "Int8", "Int16", "Int32", "Int64", "Int128",
      "UInt8", "UInt16", "UInt32", "UInt64", "UInt128",
      "Float32", "Float64",
      "Bool", "Char", "Symbol", "Nil", "Void",
      "Pointer", # Raw pointers don't participate in RC
    }

    private def is_ffi_method?(name : String) : Bool
      FFI_METHODS.includes?(name)
    end

    private def is_spawn_method?(name : String) : Bool
      SPAWN_METHODS.includes?(name)
    end

    private def is_channel_type?(name : String) : Bool
      CHANNEL_TYPES.any? { |t| name.includes?(t) }
    end

    # =========================================================================
    # PUBLIC HELPERS
    # =========================================================================

    # Check if a type is cyclic
    def cyclic?(type_name : String) : Bool
      @cyclic_types.includes?(type_name)
    end

    # Get all values with a specific taint
    def values_with_taint(taint : Taint) : Array(ValueId)
      result = [] of ValueId
      @definitions.each do |id, value|
        if value.taints.includes?(taint)
          result << id
        end
      end
      result
    end

    # Get taint summary for the function
    def taint_summary : Hash(String, Int32)
      {
        "cyclic_types"   => @cyclic_types.size,
        "thread_shared"  => values_with_taint(Taint::ThreadShared).size,
        "ffi_exposed"    => values_with_taint(Taint::FFIExposed).size,
        "mutable"        => values_with_taint(Taint::Mutable).size,
        "cyclic_values"  => values_with_taint(Taint::Cyclic).size,
      }
    end
  end

  # Convenience method on Function
  class Function
    def analyze_taints(type_info : TypeInfoProvider? = nil)
      analyzer = TaintAnalyzer.new(self, type_info)
      analyzer.analyze
      analyzer
    end

    def analyze_taints(cyclic_types : Set(String))
      analyzer = TaintAnalyzer.new(self, cyclic_types)
      analyzer.analyze
      analyzer
    end
  end

end

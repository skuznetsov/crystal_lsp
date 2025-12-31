# Escape Analysis for HIR
#
# Determines whether values escape their defining scope.
# This is the foundation for memory management decisions.
#
# See docs/codegen_architecture.md Section 5 for full specification.

require "./hir"
require "./taint_analysis"

module Crystal::HIR
  # Result of escape analysis for a function
  class EscapeSummary
    # Which parameters escape and how?
    getter param_escapes : Array(LifetimeTag)

    # Does return value alias any parameters?
    getter return_aliases_params : Set(Int32)

    # Does function capture arguments in closures?
    property captures_args : Bool

    def initialize(param_count : Int32)
      @param_escapes = Array.new(param_count, LifetimeTag::StackLocal)
      @return_aliases_params = Set(Int32).new
      @captures_args = false
    end
  end

  # Escape analyzer using worklist algorithm
  class EscapeAnalyzer
    # Container methods that cause ArgEscape
    CONTAINER_ADD_METHODS = Set{
      "<<", "push", "unshift", "add", "insert",
      "[]=", "put", "store",
    }

    getter function : Function
    getter summary : EscapeSummary

    # Track which values have been processed
    @worklist : Deque(ValueId)

    # Track value users (reverse dependency)
    @users : Hash(ValueId, Array(ValueId))

    # Track value definitions
    @definitions : Hash(ValueId, Value)

    @effect_provider : MethodEffectProvider?

    def initialize(
      @function : Function,
      @type_info : TypeInfoProvider? = nil,
      @effect_provider : MethodEffectProvider? = nil
    )
      @summary = EscapeSummary.new(@function.params.size)
      @worklist = Deque(ValueId).new
      @users = Hash(ValueId, Array(ValueId)).new { |h, k| h[k] = [] of ValueId }
      @definitions = Hash(ValueId, Value).new
    end

    # Run escape analysis on the function
    def analyze : EscapeSummary
      # Phase 1: Build dependency graph and initialize lifetimes
      build_dependency_graph
      initialize_lifetimes

      # Phase 2: Seed worklist with known escape points
      seed_escape_points

      # Phase 3: Propagate escapes through data flow
      propagate_escapes

      # Phase 4: Build summary from parameters
      build_summary

      @summary
    end

    private def build_dependency_graph
      @function.blocks.each do |block|
        block.instructions.each do |value|
          @definitions[value.id] = value
          record_uses(value)
        end
      end
    end

    # Record which values are used by this value
    private def record_uses(value : Value)
      case value
      when Copy
        @users[value.source] << value.id
      when BinaryOperation
        @users[value.left] << value.id
        @users[value.right] << value.id
      when UnaryOperation
        @users[value.operand] << value.id
      when Call
        if recv = value.receiver
          @users[recv] << value.id
        end
        value.args.each { |arg| @users[arg] << value.id }
      when FieldGet
        @users[value.object] << value.id
      when FieldSet
        @users[value.object] << value.id
        @users[value.value] << value.id
      when IndexGet
        @users[value.object] << value.id
        @users[value.index] << value.id
      when IndexSet
        @users[value.object] << value.id
        @users[value.index] << value.id
        @users[value.value] << value.id
      when MakeClosure
        value.captures.each { |cap| @users[cap.value_id] << value.id }
      when Yield
        value.args.each { |arg| @users[arg] << value.id }
      when Phi
        value.incoming.each { |(_, val)| @users[val] << value.id }
      when Cast
        @users[value.value] << value.id
      when IsA
        @users[value.value] << value.id
      when Allocate
        value.constructor_args.each { |arg| @users[arg] << value.id }
      end
    end

    private def initialize_lifetimes
      # All values start as StackLocal (optimistic)
      @definitions.each_value do |value|
        value.lifetime = LifetimeTag::StackLocal
      end

      # Parameters start as Unknown (could be anything)
      @function.params.each do |param|
        param.lifetime = LifetimeTag::Unknown
      end
    end

    private def seed_escape_points
      # 1. Return values escape to caller (HeapEscape)
      @function.blocks.each do |block|
        case term = block.terminator
        when Return
          if val = term.value
            mark_escape(val, LifetimeTag::HeapEscape)
          end
        end
      end

      # 2. Closure captures escape (HeapEscape)
      @definitions.each_value do |value|
        case value
        when MakeClosure
          value.captures.each do |cap|
            mark_escape(cap.value_id, LifetimeTag::HeapEscape)
            @summary.captures_args = true if is_parameter?(cap.value_id)
          end
        end
      end

      # 3. Global/class variable assignments (GlobalEscape)
      @definitions.each_value do |value|
        case value
        when ClassVarSet
          mark_escape(value.value, LifetimeTag::GlobalEscape)
        end
      end

      # 4. Container adds and field assignments
      @definitions.each_value do |value|
        case value
        when Call
          if effects = @effect_provider.try(&.method_effects_for(value.method_name))
            if effects.transfer
              value.args.each do |arg|
                mark_escape(arg, LifetimeTag::ArgEscape)
              end
            end
            # NoEscape means "do nothing" here; trust the effect summary.
          else
            if is_container_add?(value.method_name)
              # Arguments escape into the container
              value.args.each do |arg|
                mark_escape(arg, LifetimeTag::ArgEscape)
              end
            elsif is_virtual_call?(value)
              # Conservative: args may escape through virtual call
              value.args.each do |arg|
                mark_escape(arg, LifetimeTag::HeapEscape)
              end
            end
          end

        when FieldSet
          # If object escapes, value escapes too
          obj_lifetime = get_lifetime(value.object)
          if obj_lifetime.escapes_more_than?(LifetimeTag::StackLocal)
            mark_escape(value.value, obj_lifetime)
          end

        when IndexSet
          # Value escapes into container
          mark_escape(value.value, LifetimeTag::ArgEscape)
        end
      end

      # 5. Yield values (conservative - may escape via block)
      @definitions.each_value do |value|
        case value
        when Yield
          value.args.each do |arg|
            mark_escape(arg, LifetimeTag::HeapEscape)
          end
        end
      end
    end

    private def propagate_escapes
      while val_id = @worklist.shift?
        value = @definitions[val_id]?
        next unless value

        current_lifetime = value.lifetime

        # Propagate to users
        @users[val_id].each do |user_id|
          user = @definitions[user_id]?
          next unless user

          propagate_to_user(value, user, current_lifetime)
        end

        # Propagate backwards through data flow
        propagate_backwards(value, current_lifetime)
      end
    end

    private def propagate_to_user(source : Value, user : Value, lifetime : LifetimeTag)
      case user
      when Copy
        # Copy propagates escape
        mark_escape(user.id, lifetime)

      when Phi
        # Phi merges escapes
        mark_escape(user.id, lifetime)

      when FieldSet
        # If we're setting a field on an escaping object, the object escapes more
        if source.id == user.value
          obj_lifetime = get_lifetime(user.object)
          if lifetime.escapes_more_than?(obj_lifetime)
            # Value escapes more than object - object needs to escape too
            mark_escape(user.object, lifetime)
          end
        end

      when Call
        # If this value is returned from a call, it inherits caller's escape
        # (handled in summary-based inter-proc analysis)
        nil

      when MakeClosure
        # Already handled in seeding
        nil
      end
    end

    private def propagate_backwards(value : Value, lifetime : LifetimeTag)
      # Propagate escape information backwards through definitions
      case value
      when Copy
        mark_escape(value.source, lifetime)

      when Phi
        value.incoming.each do |(_, val)|
          mark_escape(val, lifetime)
        end

      when Allocate
        # If allocation escapes, its initializer args should be analyzed
        # (they become part of the escaping object)
        if lifetime.escapes_more_than?(LifetimeTag::StackLocal)
          value.constructor_args.each do |arg|
            mark_escape(arg, LifetimeTag::ArgEscape)
          end
        end
      end
    end

    private def mark_escape(value_id : ValueId, tag : LifetimeTag)
      # Check if it's a parameter
      param = @function.params.find { |p| p.id == value_id }
      if param
        if tag.escapes_more_than?(param.lifetime)
          param.lifetime = tag
          @worklist << value_id
        end
        return
      end

      # Regular value
      value = @definitions[value_id]?
      return unless value

      if tag.escapes_more_than?(value.lifetime)
        value.lifetime = tag
        @worklist << value_id
      end
    end

    private def get_lifetime(value_id : ValueId) : LifetimeTag
      # Check parameters first
      param = @function.params.find { |p| p.id == value_id }
      return param.lifetime if param

      # Then definitions
      @definitions[value_id]?.try(&.lifetime) || LifetimeTag::Unknown
    end

    private def is_parameter?(value_id : ValueId) : Bool
      @function.params.any? { |p| p.id == value_id }
    end

    private def is_container_add?(method_name : String) : Bool
      CONTAINER_ADD_METHODS.includes?(container_method_base(method_name))
    end

    private def container_method_base(method_name : String) : String
      base = method_name
      if idx = base.rindex('#')
        base = base[(idx + 1)..]
      elsif idx = base.rindex('.')
        base = base[(idx + 1)..]
      end
      if idx = base.index('$')
        base[0, idx]
      else
        base
      end
    end

    private def is_virtual_call?(call : Call) : Bool
      # Conservative by default: treat as virtual unless we can prove otherwise.
      return false unless call.receiver
      return true if call.virtual

      if type_info = @type_info
        if recv = @definitions[call.receiver]?
          if kind = type_info.type_kind_for(recv.type)
            return false if kind.in?(TypeKind::Struct, TypeKind::Primitive)
          end
        end
      end

      true
    end

    private def build_summary
      # Record parameter escapes
      @function.params.each_with_index do |param, idx|
        @summary.param_escapes[idx] = param.lifetime
      end

      # Check if return value aliases any parameters
      @function.blocks.each do |block|
        case term = block.terminator
        when Return
          if val = term.value
            check_return_aliases(val)
          end
        end
      end
    end

    private def check_return_aliases(value_id : ValueId, visited : Set(ValueId)? = nil)
      # Track visited nodes to prevent infinite recursion on phi cycles
      visited ||= Set(ValueId).new
      return if visited.includes?(value_id)
      visited.add(value_id)

      # Check if value is directly a parameter
      @function.params.each_with_index do |param, idx|
        if param.id == value_id
          @summary.return_aliases_params << idx
          return
        end
      end

      # Check if value is derived from a parameter
      value = @definitions[value_id]?
      return unless value

      case value
      when Copy
        check_return_aliases(value.source, visited)
      when Phi
        value.incoming.each { |(_, val)| check_return_aliases(val, visited) }
      when FieldGet
        check_return_aliases(value.object, visited)
      when IndexGet
        check_return_aliases(value.object, visited)
      end
    end

    # =========================================================================
    # Public analysis helpers
    # =========================================================================

    # Check if a value escapes
    def escapes?(value_id : ValueId) : Bool
      get_lifetime(value_id).escapes_more_than?(LifetimeTag::StackLocal)
    end

    # Get the escape level of a value
    def escape_level(value_id : ValueId) : LifetimeTag
      get_lifetime(value_id)
    end

    # Get all values that escape
    def escaping_values : Array(ValueId)
      result = [] of ValueId
      @definitions.each do |id, value|
        if value.lifetime.escapes_more_than?(LifetimeTag::StackLocal)
          result << id
        end
      end
      @function.params.each do |param|
        if param.lifetime.escapes_more_than?(LifetimeTag::StackLocal)
          result << param.id
        end
      end
      result
    end
  end

  # Convenience method on Function
  class Function
    def analyze_escapes(
      type_info : TypeInfoProvider? = nil,
      effect_provider : MethodEffectProvider? = nil
    ) : EscapeSummary
      analyzer = EscapeAnalyzer.new(self, type_info, effect_provider)
      analyzer.analyze
    end
  end
end

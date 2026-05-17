# Macro Value System for Crystal v2
#
# This module provides a type-safe value system for macro interpretation.
# Unlike the original Crystal compiler which uses AST nodes directly,
# we use lightweight value objects optimized for macro evaluation.
#
# Design goals:
# - Type-safe: each value type has specific methods
# - Fast: minimal allocations, inline storage where possible
# - Compatible: match original Crystal macro semantics

module CrystalV2
  module Compiler
    module Semantic
      # Base class for all macro values
      abstract class MacroValue
        # Convert to string for output in macro expansion
        abstract def to_macro_output : String

        # Convert to boolean for conditionals (Crystal truthiness)
        def truthy? : Bool
          true # Most values are truthy; NilValue and BoolValue(false) override
        end

        # Get string representation for .stringify
        def stringify : String
          to_macro_output
        end

        # Get identifier representation for .id
        def to_id : String
          to_macro_output
        end

        # Call a method on this value
        def call_method(name : String, args : Array(MacroValue), named_args : Hash(String, MacroValue)?) : MacroValue
          case name
          when "stringify"
            MacroStringValue.new(stringify)
          when "id"
            MacroIdValue.new(to_id)
          when "class_name"
            MacroStringValue.new(class_name)
          when "is_a?"
            if expected = args[0]?
              MacroBoolValue.new(class_name == expected.to_id)
            else
              MacroBoolValue.new(false)
            end
          else
            MacroNilValue.new # Unknown method returns nil
          end
        end

        # Get the macro type name (for .class_name)
        abstract def class_name : String

        # Get the Crystal type name (for typeof())
        # Returns the actual Crystal type of the value
        def type_name : String
          # Default to class_name, subclasses override for specific types
          class_name
        end
      end

      # Nil value
      class MacroNilValue < MacroValue
        def to_macro_output : String
          ""
        end

        def truthy? : Bool
          false
        end

        def class_name : String
          "NilLiteral"
        end

        def type_name : String
          "Nil"
        end
      end

      # Boolean value
      class MacroBoolValue < MacroValue
        getter value : Bool

        def initialize(@value : Bool)
        end

        def to_macro_output : String
          @value ? "true" : "false"
        end

        def truthy? : Bool
          @value
        end

        def class_name : String
          "BoolLiteral"
        end

        def type_name : String
          "Bool"
        end
      end

      # Numeric value (Int or Float)
      class MacroNumberValue < MacroValue
        getter value : Int64 | Float64
        getter source_literal : String?

        def self.from_literal(literal : String) : MacroNumberValue?
          normalized = literal.gsub("_", "")
          suffix = numeric_suffix(normalized)
          body = suffix ? normalized[0, normalized.size - suffix.size] : normalized

          if body.includes?(".") || body.includes?("e") || body.includes?("E")
            value = body.to_f64?
            return value ? MacroNumberValue.new(value, literal) : nil
          end

          if suffix && unsigned_numeric_suffix?(suffix)
            return nil if body.starts_with?('-')

            base = 10
            digits = body
            if digits.starts_with?("0x")
              base = 16
              digits = digits[2..]
            elsif digits.starts_with?("0b")
              base = 2
              digits = digits[2..]
            elsif digits.starts_with?("0o")
              base = 8
              digits = digits[2..]
            end

            return nil if digits.empty?
            uint_value = digits.to_u64?(base)
            return nil unless uint_value
            return MacroNumberValue.new(uint_value.unsafe_as(Int64), literal)
          end

          value = body.to_i64?(prefix: true)
          value ? MacroNumberValue.new(value, literal) : nil
        end

        private def self.numeric_suffix(literal : String) : String?
          return "_i8" if literal.ends_with?("_i8")
          return "_i16" if literal.ends_with?("_i16")
          return "_i32" if literal.ends_with?("_i32")
          return "_i64" if literal.ends_with?("_i64")
          return "_u8" if literal.ends_with?("_u8")
          return "_u16" if literal.ends_with?("_u16")
          return "_u32" if literal.ends_with?("_u32")
          return "_u64" if literal.ends_with?("_u64")
          return "_f32" if literal.ends_with?("_f32")
          return "_f64" if literal.ends_with?("_f64")
          return "i8" if literal.ends_with?("i8")
          return "i16" if literal.ends_with?("i16")
          return "i32" if literal.ends_with?("i32")
          return "i64" if literal.ends_with?("i64")
          return "u8" if literal.ends_with?("u8")
          return "u16" if literal.ends_with?("u16")
          return "u32" if literal.ends_with?("u32")
          return "u64" if literal.ends_with?("u64")
          return "f32" if literal.ends_with?("f32")
          return "f64" if literal.ends_with?("f64")
          nil
        end

        private def self.unsigned_numeric_suffix?(suffix : String) : Bool
          suffix.ends_with?("u8") || suffix.ends_with?("u16") || suffix.ends_with?("u32") || suffix.ends_with?("u64")
        end

        def initialize(value : Int32 | Int64 | Float32 | Float64, @source_literal : String? = nil)
          @value = value.is_a?(Float32 | Float64) ? value.to_f64 : value.to_i64
        end

        def to_macro_output : String
          @source_literal || @value.to_s
        end

        def to_i : Int64
          @value.is_a?(Int64) ? @value.as(Int64) : @value.as(Float64).to_i64
        end

        def to_f : Float64
          @value.is_a?(Float64) ? @value.as(Float64) : @value.as(Int64).to_f64
        end

        def class_name : String
          "NumberLiteral"
        end

        def type_name : String
          if literal = @source_literal
            case
            when literal.ends_with?("_i8") || literal.ends_with?("i8")
              return "Int8"
            when literal.ends_with?("_i16") || literal.ends_with?("i16")
              return "Int16"
            when literal.ends_with?("_i32") || literal.ends_with?("i32")
              return "Int32"
            when literal.ends_with?("_i64") || literal.ends_with?("i64")
              return "Int64"
            when literal.ends_with?("_u8") || literal.ends_with?("u8")
              return "UInt8"
            when literal.ends_with?("_u16") || literal.ends_with?("u16")
              return "UInt16"
            when literal.ends_with?("_u32") || literal.ends_with?("u32")
              return "UInt32"
            when literal.ends_with?("_u64") || literal.ends_with?("u64")
              return "UInt64"
            when literal.ends_with?("_f32") || literal.ends_with?("f32")
              return "Float32"
            when literal.ends_with?("_f64") || literal.ends_with?("f64")
              return "Float64"
            end
          end
          @value.is_a?(Float64) ? "Float64" : "Int64"
        end

        def call_method(name : String, args : Array(MacroValue), named_args : Hash(String, MacroValue)?) : MacroValue
          case name
          when "+"
            if arg = args[0]?
              if arg.is_a?(MacroNumberValue)
                return MacroNumberValue.new(@value.is_a?(Int64) && arg.value.is_a?(Int64) ? to_i + arg.to_i : to_f + arg.to_f)
              end
            end
            MacroNilValue.new
          when "-"
            if arg = args[0]?
              if arg.is_a?(MacroNumberValue)
                return MacroNumberValue.new(@value.is_a?(Int64) && arg.value.is_a?(Int64) ? to_i - arg.to_i : to_f - arg.to_f)
              end
            end
            MacroNilValue.new
          when "*"
            if arg = args[0]?
              if arg.is_a?(MacroNumberValue)
                return MacroNumberValue.new(@value.is_a?(Int64) && arg.value.is_a?(Int64) ? to_i * arg.to_i : to_f * arg.to_f)
              end
            end
            MacroNilValue.new
          when "/"
            if arg = args[0]?
              if arg.is_a?(MacroNumberValue)
                return MacroNumberValue.new(to_f / arg.to_f)
              end
            end
            MacroNilValue.new
          when "//"
            if arg = args[0]?
              if arg.is_a?(MacroNumberValue)
                if @value.is_a?(Int64) && arg.value.is_a?(Int64)
                  return MacroNumberValue.new(to_i // arg.to_i)
                end
                return MacroNumberValue.new((to_f / arg.to_f).floor)
              end
            end
            MacroNilValue.new
          when "**"
            if arg = args[0]?
              if arg.is_a?(MacroNumberValue)
                if @value.is_a?(Int64) && arg.value.is_a?(Int64)
                  return MacroNumberValue.new(to_i ** arg.to_i)
                end
                return MacroNumberValue.new(to_f ** arg.to_f)
              end
            end
            MacroNilValue.new
          when ">", ">=", "<", "<=", "==", "!="
            if arg = args[0]?
              if arg.is_a?(MacroNumberValue)
                result = case name
                         when ">"  then to_f > arg.to_f
                         when ">=" then to_f >= arg.to_f
                         when "<"  then to_f < arg.to_f
                         when "<=" then to_f <= arg.to_f
                         when "==" then to_f == arg.to_f
                         when "!=" then to_f != arg.to_f
                         else           false
                         end
                return MacroBoolValue.new(result)
              end
            end
            MacroNilValue.new
          else
            super
          end
        end
      end

      # String value
      class MacroStringValue < MacroValue
        getter value : String

        def initialize(@value : String)
        end

        def to_macro_output : String
          @value.inspect # Output with quotes
        end

        def stringify : String
          @value
        end

        def to_id : String
          @value # Without quotes
        end

        def class_name : String
          "StringLiteral"
        end

        def type_name : String
          "String"
        end

        def call_method(name : String, args : Array(MacroValue), named_args : Hash(String, MacroValue)?) : MacroValue
          case name
          when "size", "bytesize"
            MacroNumberValue.new(@value.size.to_i64)
          when "empty?"
            MacroBoolValue.new(@value.empty?)
          when "chomp"
            MacroStringValue.new(@value.chomp)
          when "strip"
            MacroStringValue.new(@value.strip)
          when "downcase"
            MacroStringValue.new(@value.downcase)
          when "upcase"
            MacroStringValue.new(@value.upcase)
          when "capitalize"
            MacroStringValue.new(@value.capitalize)
          when "camelcase"
            MacroStringValue.new(@value.camelcase)
          when "underscore"
            MacroStringValue.new(@value.underscore)
          when "starts_with?"
            if arg = args[0]?
              if arg.is_a?(MacroStringValue)
                return MacroBoolValue.new(@value.starts_with?(arg.value))
              end
            end
            MacroNilValue.new
          when "ends_with?"
            if arg = args[0]?
              if arg.is_a?(MacroStringValue)
                return MacroBoolValue.new(@value.ends_with?(arg.value))
              end
            end
            MacroNilValue.new
          when "includes?"
            if arg = args[0]?
              if arg.is_a?(MacroStringValue)
                return MacroBoolValue.new(@value.includes?(arg.value))
              end
            end
            MacroNilValue.new
          when "+"
            if arg = args[0]?
              return MacroStringValue.new(@value + arg.to_macro_output)
            end
            MacroNilValue.new
          when "[]"
            if arg = args[0]?
              if arg.is_a?(MacroNumberValue)
                idx = arg.to_i.to_i32
                if idx >= 0 && idx < @value.size
                  return MacroStringValue.new(@value[idx].to_s)
                end
              end
            end
            MacroNilValue.new
          else
            super
          end
        end
      end

      # Symbol value
      class MacroSymbolValue < MacroValue
        getter value : String

        def initialize(@value : String)
        end

        def to_macro_output : String
          ":#{@value}"
        end

        def to_id : String
          @value
        end

        def class_name : String
          "SymbolLiteral"
        end

        def type_name : String
          "Symbol"
        end
      end

      # Identifier (MacroId) - unquoted string
      class MacroIdValue < MacroValue
        getter value : String

        def initialize(@value : String)
        end

        def to_macro_output : String
          @value
        end

        def stringify : String
          @value
        end

        def to_id : String
          @value
        end

        def class_name : String
          "MacroId"
        end

        def call_method(name : String, args : Array(MacroValue), named_args : Hash(String, MacroValue)?) : MacroValue
          case name
          when "==", "!="
            if arg = args[0]?
              result = @value == arg.to_id
              result = !result if name == "!="
              return MacroBoolValue.new(result)
            end
            MacroBoolValue.new(false)
          when "downcase"
            MacroIdValue.new(@value.downcase)
          when "upcase"
            MacroIdValue.new(@value.upcase)
          when "capitalize"
            MacroIdValue.new(@value.capitalize)
          when "camelcase"
            MacroIdValue.new(@value.camelcase)
          when "underscore"
            MacroIdValue.new(@value.underscore)
          when "strip"
            MacroIdValue.new(@value.strip)
          when "chomp"
            MacroIdValue.new(@value.chomp)
          when "starts_with?"
            arg = args[0]?
            MacroBoolValue.new(arg ? @value.starts_with?(arg.to_id) : false)
          when "ends_with?"
            arg = args[0]?
            MacroBoolValue.new(arg ? @value.ends_with?(arg.to_id) : false)
          when "includes?"
            arg = args[0]?
            MacroBoolValue.new(arg ? @value.includes?(arg.to_id) : false)
          when "split"
            separator = args[0]?.try(&.to_id) || " "
            parts = @value.split(separator).map { |part| MacroIdValue.new(part).as(MacroValue) }
            MacroArrayValue.new(parts)
          when "symbolize"
            MacroSymbolValue.new(@value)
          else
            super
          end
        end
      end

      # AST node value (Assign, TypeDeclaration, Identifier, etc.)
      # Used by macros like `record` and `getter` that inspect AST node kinds.
      class MacroNodeValue < MacroValue
        getter node_id : Frontend::ExprId
        getter arena : Frontend::ArenaLike
        getter string_pool : Frontend::StringPool?

        def initialize(
          @node_id : Frontend::ExprId,
          @arena : Frontend::ArenaLike,
          @string_pool : Frontend::StringPool? = nil
        )
        end

        def to_macro_output : String
          stringify_node(@arena[@node_id])
        end

        def class_name : String
          Frontend.node_kind(@arena[@node_id]).to_s
        end

        def call_method(name : String, args : Array(MacroValue), named_args : Hash(String, MacroValue)?) : MacroValue
          node = @arena[@node_id]
          case name
          when "id"
            if id_name = node_identifier_name(node)
              MacroIdValue.new(id_name)
            else
              MacroIdValue.new(to_macro_output)
            end
          when "target"
            if node.is_a?(Frontend::AssignNode)
              MacroNodeValue.new(node.target, @arena, @string_pool)
            else
              MacroNilValue.new
            end
          when "var"
            if node.is_a?(Frontend::TypeDeclarationNode)
              MacroIdValue.new(intern_name(node.name))
            else
              MacroNilValue.new
            end
          when "type"
            if node.is_a?(Frontend::TypeDeclarationNode)
              MacroIdValue.new(intern_name(node.declared_type))
            else
              MacroNilValue.new
            end
          when "value"
            if node.is_a?(Frontend::TypeDeclarationNode)
              if value_id = node.value
                MacroNodeValue.new(value_id, @arena, @string_pool)
              else
                MacroNilValue.new
              end
            elsif node.is_a?(Frontend::AssignNode)
              MacroNodeValue.new(node.value, @arena, @string_pool)
            else
              MacroNilValue.new
            end
          when "is_a?"
            if expected = args[0]?
              if ENV["DEBUG_MACRO_ISA"]?
                STDERR.puts "[MACRO_ISA] node=#{class_name} expected=#{expected.to_id}"
              end
              MacroBoolValue.new(class_name == expected.to_id)
            else
              MacroBoolValue.new(false)
            end
          else
            super
          end
        end

        private def node_identifier_name(node) : String?
          case node
          when Frontend::IdentifierNode
            intern_name(node.name)
          when Frontend::ConstantNode
            intern_name(node.name)
          when Frontend::InstanceVarNode
            intern_name(node.name)
          when Frontend::TypeDeclarationNode
            # Crystal: TypeDeclaration#id returns the full form "var : Type = default"
            # so that record macro's "@#{field.id}".id produces "@var : Type = default"
            type_n = intern_name(node.declared_type)
            type_n = type_n[1..] if type_n.starts_with?(':')
            id_result = "#{intern_name(node.name)} : #{type_n}"
            if val_id = node.value
              val_str = stringify_node(@arena[val_id])
              id_result = "#{id_result} = #{val_str}" unless val_str.empty?
            end
            id_result
          when Frontend::GenericNode
            base_name = node_identifier_name(@arena[node.base_type]) || stringify_node(@arena[node.base_type])
            args = node.type_args.map do |arg_id|
              node_identifier_name(@arena[arg_id]) || stringify_node(@arena[arg_id])
            end
            if base_name
              "#{base_name}(#{args.join(", ")})"
            else
              nil
            end
          when Frontend::AssignNode
            node_identifier_name(@arena[node.target])
          when Frontend::PathNode
            parts = [] of String
            current = node
            loop do
              right = current.right
              right_name = node_identifier_name(@arena[right])
              parts << right_name if right_name
              left_id = current.left
              break unless left_id
              left_node = @arena[left_id.not_nil!]
              break unless left_node.is_a?(Frontend::PathNode)
              current = left_node
            end
            if left_id = current.left
              left_name = node_identifier_name(@arena[left_id.not_nil!])
              parts << left_name if left_name
            end
            parts.reverse.join("::")
          else
            nil
          end
        end

        private def stringify_node(node) : String
          case node
          when Frontend::NumberNode
            String.new(node.value)
          when Frontend::StringNode
            String.new(node.value).inspect
          when Frontend::CharNode
            literal = Frontend.node_literal_string(node) || ""
            literal.empty? ? "" : "'#{literal}'"
          when Frontend::SymbolNode
            literal = Frontend.node_literal_string(node) || ""
            literal.starts_with?(":") ? literal : ":#{literal}"
          when Frontend::IdentifierNode
            intern_name(node.name)
          when Frontend::ConstantNode
            intern_name(node.name)
          when Frontend::InstanceVarNode
            intern_name(node.name)
        when Frontend::TypeDeclarationNode
          type_name = intern_name(node.declared_type)
          type_name = type_name[1..] if type_name.starts_with?(':')
          sn_result = "#{intern_name(node.name)} : #{type_name}"
          if sn_val_id = node.value
            sn_val_str = stringify_node(@arena[sn_val_id])
            sn_result = "#{sn_result} = #{sn_val_str}" unless sn_val_str.empty?
          end
          sn_result
          when Frontend::PathNode
            node_identifier_name(node) || ""
          when Frontend::MemberAccessNode
            object = stringify_node(@arena[node.object])
            member = intern_name(node.member)
            object.empty? ? member : "#{object}.#{member}"
          when Frontend::SafeNavigationNode
            object = stringify_node(@arena[node.object])
            member = intern_name(node.member)
            object.empty? ? member : "#{object}&.#{member}"
          when Frontend::CallNode
            callee = stringify_node(@arena[node.callee])
            args = node.args.map { |arg_id| stringify_node(@arena[arg_id]) }
            if named_args = node.named_args
              named_args.each do |entry|
                args << "#{intern_name(entry.name)}: #{stringify_node(@arena[entry.value])}"
              end
            end
            if args.empty?
              callee
            else
              "#{callee}(#{args.join(", ")})"
            end
          when Frontend::AsNode
            "#{stringify_node(@arena[node.expression])}.as(#{intern_name(node.target_type)})"
          when Frontend::AsQuestionNode
            "#{stringify_node(@arena[node.expression])}.as?(#{intern_name(node.target_type)})"
          when Frontend::IsANode
            "#{stringify_node(@arena[node.expression])}.is_a?(#{intern_name(node.target_type)})"
          when Frontend::AssignNode
            target = stringify_node(@arena[node.target])
            value = stringify_node(@arena[node.value])
            "#{target} = #{value}"
          when Frontend::GenericNode
            base_name = stringify_node(@arena[node.base_type])
            args = node.type_args.map { |arg_id| stringify_node(@arena[arg_id]) }
            "#{base_name}(#{args.join(", ")})"
          when Frontend::PointerofNode
            inner = node.args.first?
            inner_text = inner ? stringify_node(@arena[inner]) : ""
            "pointerof(#{inner_text})"
          when Frontend::UnaryNode
            op = String.new(node.operator)
            operand = stringify_node(@arena[node.operand])
            "#{op}#{operand}"
          when Frontend::GroupingNode
            "(#{stringify_node(@arena[node.expression])})"
          when Frontend::TupleLiteralNode
            parts = node.elements.map { |elem_id| stringify_node(@arena[elem_id]) }
            "{#{parts.join(", ")}}"
          when Frontend::ArrayLiteralNode
            parts = node.elements.map { |elem_id| stringify_node(@arena[elem_id]) }
            "[#{parts.join(", ")}]"
          else
            Frontend.node_literal_string(node) || ""
          end
        end

        private def intern_name(slice : Slice(UInt8)) : String
          if pool = @string_pool
            pool.intern_string(slice)
          else
            String.new(slice)
          end
        end
      end

      # Array value
      class MacroArrayValue < MacroValue
        getter elements : Array(MacroValue)

        def initialize(@elements : Array(MacroValue) = [] of MacroValue)
        end

        def to_macro_output : String
          "[#{@elements.map(&.to_macro_output).join(", ")}]"
        end

        def class_name : String
          "ArrayLiteral"
        end

        def call_method(name : String, args : Array(MacroValue), named_args : Hash(String, MacroValue)?) : MacroValue
          case name
          when "size"
            MacroNumberValue.new(@elements.size.to_i64)
          when "empty?"
            MacroBoolValue.new(@elements.empty?)
          when "splat"
            # Expand array elements as comma-separated identifiers.
            MacroIdValue.new(@elements.map(&.to_id).join(", "))
          when "first"
            @elements.first? || MacroNilValue.new
          when "last"
            @elements.last? || MacroNilValue.new
          when "[]"
            if arg = args[0]?
              if arg.is_a?(MacroNumberValue)
                idx = arg.to_i.to_i32
                if idx >= 0 && idx < @elements.size
                  return @elements[idx]
                elsif idx < 0 && idx >= -@elements.size
                  return @elements[idx]
                end
              end
            end
            MacroNilValue.new
          when "map"
            # Basic map - returns same array for now
            # Full implementation would need block support
            self
          when "select", "reject"
            # Would need block support
            self
          when "includes?"
            if arg = args[0]?
              target = arg.to_macro_output
              found = @elements.any? { |e| e.to_macro_output == target }
              return MacroBoolValue.new(found)
            end
            MacroBoolValue.new(false)
          when "+"
            if arg = args[0]?
              if arg.is_a?(MacroArrayValue)
                return MacroArrayValue.new(@elements + arg.elements)
              end
            end
            self
          when "join"
            sep = args[0]?.try { |a| a.is_a?(MacroStringValue) ? a.value : ", " } || ""
            MacroStringValue.new(@elements.map(&.to_id).join(sep))
          when "sort"
            MacroArrayValue.new(@elements.sort_by(&.to_id))
          else
            super
          end
        end

        # Iteration support
        def each(&block : MacroValue ->)
          @elements.each { |e| block.call(e) }
        end

        def each_with_index(&block : MacroValue, Int32 ->)
          @elements.each_with_index { |e, i| block.call(e, i) }
        end
      end

      # Tuple value (similar to array but immutable semantics)
      class MacroTupleValue < MacroValue
        getter elements : Array(MacroValue)

        def initialize(@elements : Array(MacroValue) = [] of MacroValue)
        end

        def to_macro_output : String
          "{#{@elements.map(&.to_macro_output).join(", ")}}"
        end

        def class_name : String
          "TupleLiteral"
        end

        def call_method(name : String, args : Array(MacroValue), named_args : Hash(String, MacroValue)?) : MacroValue
          case name
          when "size"
            MacroNumberValue.new(@elements.size.to_i64)
          when "empty?"
            MacroBoolValue.new(@elements.empty?)
          when "[]"
            if arg = args[0]?
              if arg.is_a?(MacroNumberValue)
                idx = arg.to_i.to_i32
                if idx >= 0 && idx < @elements.size
                  return @elements[idx]
                end
              end
            end
            MacroNilValue.new
          else
            super
          end
        end
      end

      # Named tuple value
      class MacroNamedTupleValue < MacroValue
        getter entries : Hash(String, MacroValue)

        def initialize(@entries : Hash(String, MacroValue) = {} of String => MacroValue)
        end

        def to_macro_output : String
          pairs = @entries.map { |k, v| "#{k}: #{v.to_macro_output}" }
          "{#{pairs.join(", ")}}"
        end

        def class_name : String
          "NamedTupleLiteral"
        end

        def call_method(name : String, args : Array(MacroValue), named_args : Hash(String, MacroValue)?) : MacroValue
          case name
          when "size"
            MacroNumberValue.new(@entries.size.to_i64)
          when "empty?"
            MacroBoolValue.new(@entries.empty?)
          when "keys"
            MacroArrayValue.new(@entries.keys.map { |k| MacroIdValue.new(k).as(MacroValue) })
          when "values"
            MacroArrayValue.new(@entries.values)
          when "[]"
            if arg = args[0]?
              key = case arg
                    when MacroStringValue then arg.value
                    when MacroSymbolValue then arg.value
                    when MacroIdValue     then arg.value
                    else                       nil
                    end
              if key && @entries.has_key?(key)
                return @entries[key]
              end
            end
            MacroNilValue.new
          else
            super
          end
        end
      end

      # Hash value used by macro-local assignments such as:
      #   {% values = {} of Nil => Nil %}
      #   {% values[key] = expr %}
      class MacroHashValue < MacroValue
        getter entries : Array({MacroValue, MacroValue})

        def initialize(@entries : Array({MacroValue, MacroValue}) = [] of {MacroValue, MacroValue})
        end

        def to_macro_output : String
          pairs = @entries.map { |key, value| "#{key.to_macro_output} => #{value.to_macro_output}" }
          "{#{pairs.join(", ")}}"
        end

        def class_name : String
          "HashLiteral"
        end

        def assign(key : MacroValue, value : MacroValue) : MacroValue
          signature = key_signature(key)
          @entries.each_with_index do |(existing_key, _), idx|
            if key_signature(existing_key) == signature
              @entries[idx] = {key, value}
              return value
            end
          end

          @entries << {key, value}
          value
        end

        def call_method(name : String, args : Array(MacroValue), named_args : Hash(String, MacroValue)?) : MacroValue
          case name
          when "size"
            MacroNumberValue.new(@entries.size.to_i64)
          when "empty?"
            MacroBoolValue.new(@entries.empty?)
          when "keys"
            MacroArrayValue.new(@entries.map { |key, _| key })
          when "values"
            MacroArrayValue.new(@entries.map { |_, value| value })
          when "[]"
            if key = args[0]?
              signature = key_signature(key)
              @entries.each do |entry_key, entry_value|
                return entry_value if key_signature(entry_key) == signature
              end
            end
            MacroNilValue.new
          when "[]="
            if key = args[0]?
              if value = args[1]?
                return assign(key, value)
              end
            end
            MacroNilValue.new
          else
            super
          end
        end

        private def key_signature(key : MacroValue) : String
          key.to_id
        end
      end

      # Type node - represents a Crystal type in macros
      class MacroTypeValue < MacroValue
        getter name : String
        getter type_symbol : ClassSymbol?
        getter? is_struct : Bool
        getter? is_module : Bool
        getter? is_abstract : Bool
        getter superclass_name : String?
        getter type_vars : Array(String)?

        def initialize(
          @name : String,
          @type_symbol : ClassSymbol? = nil,
          @is_struct : Bool = false,
          @is_module : Bool = false,
          @is_abstract : Bool = false,
          @superclass_name : String? = nil,
          @type_vars : Array(String)? = nil
        )
        end

        def to_macro_output : String
          @name
        end

        def class_name : String
          "TypeNode"
        end

        def call_method(name : String, args : Array(MacroValue), named_args : Hash(String, MacroValue)?) : MacroValue
          case name
          when "name"
            # Check for generic_args named argument
            generic_args = true
            if na = named_args
              if ga = na["generic_args"]?
                generic_args = ga.truthy?
              end
            end
            result_name = if generic_args
                            @name
                          else
                            # Strip generic args: Foo(T, U) → Foo
                            if idx = @name.index('(')
                              @name[0, idx]
                            else
                              @name
                            end
                          end
            MacroIdValue.new(result_name)
          when "class?"
            MacroBoolValue.new(!@is_struct && !@is_module)
          when "struct?"
            MacroBoolValue.new(@is_struct)
          when "module?"
            MacroBoolValue.new(@is_module)
          when "abstract?"
            MacroBoolValue.new(@is_abstract)
          when "nilable?"
            MacroBoolValue.new(@name.ends_with?("?") || @name == "Nil" || @name.includes?("| Nil") || @name.includes?("Nil |"))
          when "superclass"
            if sc = @superclass_name
              MacroTypeValue.new(sc)
            else
              MacroNilValue.new
            end
          when "type_vars"
            if tv = @type_vars
              MacroArrayValue.new(tv.map { |v| MacroIdValue.new(v).as(MacroValue) })
            else
              MacroArrayValue.new
            end
          when "size"
            # For tuples/named tuples this would return element count
            # For types, return type_vars count
            MacroNumberValue.new((@type_vars.try(&.size) || 0).to_i64)
          when "instance_vars"
            if sym = @type_symbol
              vars = sym.instance_var_infos.map do |ivar_name, info|
                MacroMetaVarValue.new(ivar_name, info, sym).as(MacroValue)
              end
              MacroArrayValue.new(vars)
            else
              MacroArrayValue.new
            end
          when "methods"
            if sym = @type_symbol
              methods = sym.methods.map { |m| MacroIdValue.new(m).as(MacroValue) }
              MacroArrayValue.new(methods)
            else
              MacroArrayValue.new
            end
          when "has_method?"
            if arg = args[0]?
              method_name = arg.to_id
              if sym = @type_symbol
                return MacroBoolValue.new(sym.has_method?(method_name))
              end
            end
            MacroBoolValue.new(false)
          when "annotation", "annotations"
            # Return annotation(s) for the type
            if sym = @type_symbol
              filter_type = args[0]?.try(&.to_id)
              matching = if filter_type
                           sym.annotations.select { |a| a.full_name == filter_type }
                         else
                           sym.annotations
                         end
              if name == "annotation"
                matching.first?.try { |a| MacroAnnotationValue.new(a) } || MacroNilValue.new
              else
                MacroArrayValue.new(matching.map { |a| MacroAnnotationValue.new(a).as(MacroValue) })
              end
            else
              name == "annotation" ? MacroNilValue.new : MacroArrayValue.new
            end
          else
            super
          end
        end
      end

      # MetaVar - represents an instance variable in macros
      class MacroMetaVarValue < MacroValue
        getter var_name : String
        getter info : InstanceVarInfo
        getter owner : ClassSymbol

        def initialize(@var_name : String, @info : InstanceVarInfo, @owner : ClassSymbol)
        end

        def to_macro_output : String
          @var_name
        end

        def to_id : String
          @var_name
        end

        def class_name : String
          "MetaVar"
        end

        def call_method(name : String, args : Array(MacroValue), named_args : Hash(String, MacroValue)?) : MacroValue
          case name
          when "name"
            MacroIdValue.new(@var_name)
          when "type"
            if type_str = @info.type_annotation
              MacroTypeValue.new(type_str)
            else
              MacroNilValue.new
            end
          when "has_default_value?"
            MacroBoolValue.new(@info.has_default?)
          when "default_value"
            # Would need arena access to stringify
            # For now return nil
            MacroNilValue.new
          when "annotation", "annotations"
            filter_type = args[0]?.try(&.to_id)
            matching = @owner.ivar_annotations[@var_name]? || [] of AnnotationInfo
            if filter_type
              matching = matching.select { |a| a.full_name == filter_type }
            end
            if name == "annotation"
              matching.first?.try { |a| MacroAnnotationValue.new(a) } || MacroNilValue.new
            else
              MacroArrayValue.new(matching.map { |a| MacroAnnotationValue.new(a).as(MacroValue) })
            end
          else
            super
          end
        end
      end

      # Annotation value - represents an annotation with args access
      class MacroAnnotationValue < MacroValue
        getter info : AnnotationInfo
        # Arena reference for evaluating arg expressions
        property arena : Frontend::ArenaLike?

        def initialize(@info : AnnotationInfo, @arena : Frontend::ArenaLike? = nil)
        end

        def to_macro_output : String
          "@[#{@info.full_name}]"
        end

        def class_name : String
          "Annotation"
        end

        def call_method(name : String, args : Array(MacroValue), named_args : Hash(String, MacroValue)?) : MacroValue
          case name
          when "name"
            MacroIdValue.new(@info.full_name)
          when "[]"
            if arg = args[0]?
              case arg
              when MacroNumberValue
                # Positional arg access: ann[0]
                idx = arg.to_i.to_i32
                if idx >= 0 && idx < @info.args.size
                  return expr_to_macro_value(@info.args[idx])
                end
              when MacroSymbolValue, MacroStringValue, MacroIdValue
                # Named arg access: ann[:key] or ann["key"]
                key = arg.to_id
                if expr_id = @info.named_args[key]?
                  return expr_to_macro_value(expr_id)
                end
              end
            end
            MacroNilValue.new
          when "args"
            values = @info.args.map { |expr_id| expr_to_macro_value(expr_id) }
            MacroTupleValue.new(values)
          when "named_args"
            entries = {} of String => MacroValue
            @info.named_args.each do |key, expr_id|
              entries[key] = expr_to_macro_value(expr_id)
            end
            MacroNamedTupleValue.new(entries)
          else
            super
          end
        end

        private def expr_to_macro_value(expr_id : Frontend::ExprId) : MacroValue
          return MacroNilValue.new unless @arena
          arena = @arena.not_nil!
          node = arena[expr_id]

          case Frontend.node_kind(node)
          when .number?
            literal = Frontend.node_literal_string(node)
            if literal
              MacroNumberValue.from_literal(literal) || MacroNilValue.new
            else
              MacroNilValue.new
            end
          when .string?
            MacroStringValue.new(Frontend.node_literal_string(node) || "")
          when .symbol?
            MacroSymbolValue.new(Frontend.node_literal_string(node) || "")
          when .bool?
            MacroBoolValue.new(Frontend.node_literal_string(node) == "true")
          when Frontend::NodeKind::Nil
            MacroNilValue.new
          when .identifier?
            MacroIdValue.new(Frontend.node_literal_string(node) || "")
          else
            # For complex expressions, return as MacroId
            MacroIdValue.new(Frontend.node_literal_string(node) || "")
          end
        end
      end
    end
  end
end

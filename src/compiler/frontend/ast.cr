require "./span"

module CrystalV2
  module Compiler
    module Frontend

      # Abstract base class for all AST nodes
      # Uses vtable dispatch instead of 94-type union
      abstract class Node
        abstract def span : Span
        # Each subclass defines node_kind method returning its kind
      end

      struct ExprId
        getter index : Int32

        def initialize(@index : Int32)
        end

        def invalid?
          @index < 0
        end
      end

      # Numeric literal kind for precise type inference
      #
      # Simplified version covering main use cases:
      # Phase 103J: Extended number kinds to match Crystal language spec
      # Signed: i8, i16, i32, i64, i128
      # Unsigned: u8, u16, u32, u64, u128
      # Float: f32, f64
      enum NumberKind
        # Signed integers
        I8
        I16
        I32
        I64
        I128
        # Unsigned integers
        U8
        U16
        U32
        U64
        U128
        # Floats
        F32
        F64

        def to_s : String
          case self
          when I8   then "Int8"
          when I16  then "Int16"
          when I32  then "Int32"
          when I64  then "Int64"
          when I128 then "Int128"
          when U8   then "UInt8"
          when U16  then "UInt16"
          when U32  then "UInt32"
          when U64  then "UInt64"
          when U128 then "UInt128"
          when F32  then "Float32"
          when F64  then "Float64"
          else      "Int32"  # Default fallback
          end
        end
      end

      # Phase 37: Visibility modifier for methods
      enum Visibility
        Public
        Private
        Protected
      end

      # Phase 103J: Visibility modifier wrapper
      # Wraps an expression with visibility (private/protected)
      # Example: private CONST = 42 → VisibilityModifierNode(Private, Assign)
      class VisibilityModifierNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::VisibilityModifier
        end

        getter visibility : Visibility
        getter expression : ExprId

        def initialize(@span : Span, @visibility : Visibility, @expression : ExprId)
        end
      end

      # Represents an elsif branch in an if expression
      #
      # For production compiler with IDE support, we track:
      # - condition: The condition expression
      # - body: The body expressions
      # - span: Exact source location for diagnostics and IDE tools
      struct ElsifBranch
        getter condition : ExprId
        getter body : Array(ExprId)
        getter span : Span

        def initialize(@condition : ExprId, @body : Array(ExprId), @span : Span)
        end
      end

      # Represents a when branch in a case expression
      #
      # Phase 11: Case/when pattern matching
      # - conditions: Multiple values to match (e.g., when 1, 2, 3)
      # - body: The body expressions for this branch
      # - span: Exact source location
      struct WhenBranch
        getter conditions : Array(ExprId)
        getter body : Array(ExprId)
        getter span : Span

        def initialize(@conditions : Array(ExprId), @body : Array(ExprId), @span : Span)
        end
      end

      # Represents a when branch in a select expression
      #
      # Phase 90A: Select/when for concurrent channel operations
      # - condition: Single condition (channel operation: receive/send/timeout)
      # - body: The body expressions for this branch
      # - span: Exact source location
      struct SelectBranch
        getter condition : ExprId
        getter body : Array(ExprId)
        getter span : Span

        def initialize(@condition : ExprId, @body : Array(ExprId), @span : Span)
        end
      end

      # Represents a key-value pair in a hash literal
      #
      # Phase 14: Hash literals
      # For production compiler with IDE support, we track:
      # - key: The key expression
      # - value: The value expression
      # - span: Exact source location for entire entry (key => value)
      # - arrow_span: Location of => operator for precise diagnostics/hover
      struct HashEntry
        getter key : ExprId
        getter value : ExprId
        getter span : Span
        getter arrow_span : Span

        def initialize(@key : ExprId, @value : ExprId, @span : Span, @arrow_span : Span)
        end
      end

      # Represents a method parameter with optional type annotation
      #
      # Phase 4A: Parameter types for method resolution (PRODUCTION-READY)
      # For production compiler with IDE support, we track:
      # - name: Parameter name (e.g., "x", "y")
      # - type_annotation: Optional type (e.g., "Int32", "String", nil)
      # - span: Full parameter span ("x : Int32")
      # - name_span: Just name ("x") for rename refactoring
      # - type_span: Just type ("Int32") for hover, optional like type_annotation
      #
      # Examples:
      #   def foo(x)           → Parameter("x", nil, span, name_span, nil)
      #   def foo(x : Int32)   → Parameter("x", "Int32", span, name_span, type_span)
      struct Parameter
        getter name : Slice(UInt8)?          # Phase BLOCK_CAPTURE: nil for anonymous block (&)
        getter external_name : Slice(UInt8)?  # Phase 103K: External parameter name (e.g., "to" in "def foo(to limit)")
        getter type_annotation : Slice(UInt8)?
        getter default_value : ExprId?  # Phase 71: default parameter value
        getter span : Span              # Full "x : Int32 = 5" span
        getter name_span : Span?        # Phase BLOCK_CAPTURE: nil for anonymous block
        getter external_name_span : Span?  # Phase 103K: External name span for navigation
        getter type_span : Span?        # Just "Int32" for hover (optional)
        getter default_span : Span?     # Phase 71: Just default value span
        getter is_splat : Bool          # Phase 68: *args (single splat)
        getter is_double_splat : Bool   # Phase 68: **kwargs (double splat)
        getter is_block : Bool          # Phase 103: &block (block parameter)
        getter is_instance_var : Bool   # Instance variable parameter shorthand: @value : T

        def initialize(
          @name : Slice(UInt8)?,       # Phase BLOCK_CAPTURE: optional for anonymous block
          @external_name : Slice(UInt8)? = nil,  # Phase 103K: External parameter name
          @type_annotation : Slice(UInt8)? = nil,
          @default_value : ExprId? = nil,
          @span : Span = Span.zero,
          @name_span : Span? = nil,    # Phase BLOCK_CAPTURE: optional
          @external_name_span : Span? = nil,  # Phase 103K: External name span
          @type_span : Span? = nil,
          @default_span : Span? = nil,
          @is_splat : Bool = false,
          @is_double_splat : Bool = false,
          @is_block : Bool = false,
          @is_instance_var : Bool = false
        )
        end
      end

      # Represents a named tuple entry (key: value pair)
      #
      # Phase 70: Named tuple literals
      # For {name: "Alice", age: 30}:
      # - key: Entry key as identifier ("name", "age")
      # - value: Expression for the value
      # - key_span: Just key for navigation
      # - value_span: Just value for hover
      # - span: Full "key: value" span
      struct NamedTupleEntry
        getter key : Slice(UInt8)       # TIER 2.3: Zero-copy slice
        getter value : ExprId
        getter key_span : Span          # Just "key"
        getter value_span : Span        # Just value expression

        def initialize(
          @key : Slice(UInt8),
          @value : ExprId,
          @key_span : Span,
          @value_span : Span
        )
        end

        # Compute full span on demand to avoid storing redundant span
        def span : Span
          key_span.cover(value_span)
        end
      end

      # Represents a named argument in method call (name: value)
      #
      # Phase 72: Named arguments at call site
      # For foo(x: 10, y: 20):
      # - name: Argument name as identifier ("x", "y")
      # - value: Expression for the argument value
      # - name_span: Just name for navigation
      # - value_span: Just value for hover
      # - span: Full "name: value" span
      struct NamedArgument
        getter name : Slice(UInt8)      # Zero-copy slice from source
        getter value : ExprId
        getter name_span : Span         # Just "name"
        getter value_span : Span        # Just value expression

        def initialize(
          @name : Slice(UInt8),
          @value : ExprId,
          @name_span : Span,
          @value_span : Span
        )
        end

        # Compute full span on demand
        def span : Span
          name_span.cover(value_span)
        end
      end

      # Represents an accessor specification (getter/setter/property)
      #
      # Phase 30: Accessor macros (PRODUCTION-READY)
      # For production compiler with IDE support, we track:
      # - name: Accessor name (e.g., "name", "age")
      # - type_annotation: Optional type (e.g., "String", "Int32", nil)
      # - default_value: Optional default value expression
      # - span: Full accessor span ("name : String = value")
      # - name_span: Just name ("name") for rename refactoring
      # - type_span: Just type ("String") for hover, optional like type_annotation
      #
      # Examples:
      #   getter name              → AccessorSpec("name", nil, nil, ...)
      #   getter name : String     → AccessorSpec("name", "String", nil, ...)
      #   getter name = "default"  → AccessorSpec("name", nil, default_expr_id, ...)
      #   getter name : String = "default" → AccessorSpec("name", "String", default_expr_id, ...)
      struct AccessorSpec
        getter name : Slice(UInt8)
        getter type_annotation : Slice(UInt8)?
        getter default_value : ExprId?
        getter name_span : Span         # Just "name" for rename
        getter type_span : Span?        # Just "String" for hover (optional)
        getter default_span : Span?     # Span of default value expression (optional)
        getter predicate : Bool         # true for getter?/property? (adds ? to getter name)

        def initialize(
          @name : Slice(UInt8),
          @type_annotation : Slice(UInt8)? = nil,
          @default_value : ExprId? = nil,
          @name_span : Span = Span.zero,
          @type_span : Span? = nil,
          @default_span : Span? = nil,
          @predicate : Bool = false
        )
        end

        # Compute full span on demand from available pieces
        def span : Span
          s = name_span
          if ts = type_span
            s = s.cover(ts)
          end
          if ds = default_span
            s = s.cover(ds)
          end
          s
        end
      end

      # Represents a rescue clause in exception handling
      #
      # Phase 29: Exception handling
      # - exception_type: Optional exception type to catch (e.g., "RuntimeError")
      # - variable_name: Optional variable to bind exception (e.g., "e")
      # - body: The rescue handler body expressions
      # - span: Exact source location
      struct RescueClause
        getter exception_type : Slice(UInt8)?
        getter variable_name : Slice(UInt8)?
        getter body : Array(ExprId)
        getter span : Span

        def initialize(@exception_type : Slice(UInt8)?, @variable_name : Slice(UInt8)?, @body : Array(ExprId), @span : Span)
        end
      end

      # Represents an enum member in enum definition
      #
      # Phase 33: Enum definition
      # - name: Member name (e.g., "Red")
      # - value: Optional explicit value (e.g., Red = 1)
      # - name_span: Exact location of member name
      # - value_span: Exact location of value expression (optional)
      struct EnumMember
        getter name : Slice(UInt8)      # TIER 2.3: Zero-copy slice
        getter value : ExprId?
        getter name_span : Span
        getter value_span : Span?

        def initialize(
          @name : Slice(UInt8),
          @value : ExprId? = nil,
          @name_span : Span = Span.zero,
          @value_span : Span? = nil
        )
        end
      end

      enum NodeKind
        Identifier
        MacroVar
        InstanceVar  # @var
        InstanceVarDecl  # @var : Type (Phase 5C)
        ClassVar  # Phase 76: @@var (class variable)
        ClassVarDecl  # Phase 77: @@var : Type (class variable declaration)
        Global  # Phase 75: $var (global variable)
        GlobalVarDecl  # Phase 77: $var : Type (global variable declaration)
        Number
        String
        Char  # Phase 56: character literal 'a'
        Regex  # Phase 57: regex literal /pattern/flags
        Bool
        Nil
        Unary
        Out  # Phase 98: out keyword (C bindings output parameter)
        Binary
        Call
        Index
        MemberAccess
        SafeNavigation  # Phase 47: safe navigation (&.)
        Grouping
        If
        Unless  # Phase 24: unless condition
        While
        Until   # Phase 25: until condition
        For     # Phase 99: for loop (iteration)
        Loop    # Phase 83: infinite loop
        Spawn   # Phase 84: spawn fiber (concurrency)
        Assign
        MultipleAssign  # Phase 73: multiple assignment (a, b = 1, 2)
        MacroExpression
        MacroLiteral
        MacroDef
        MacroIf   # Phase 103B: {% if %}...{% end %} in expression context
        MacroFor  # Phase 103B: {% for %}...{% end %} in expression context
        Def
        Class
        Return  # Phase 6: return statements
        Self    # Phase 7: self keyword
        ImplicitObj  # Phase IMPLICIT_RECEIVER: implicit receiver for .method calls
        Super   # Phase 39: super keyword (call parent method)
        PreviousDef  # Phase 96: previous_def keyword (call previous definition before reopening/redefining)
        Typeof  # Phase 40: typeof (type introspection)
        Sizeof  # Phase 41: sizeof (size in bytes)
        Pointerof  # Phase 42: pointerof (pointer to variable/expression)
        Uninitialized  # Phase 85: uninitialized variable
        Offsetof  # Phase 86: offset of field in type
        Alignof  # Phase 88: ABI alignment in bytes
        InstanceAlignof  # Phase 88: instance alignment
        Asm  # Phase 95: inline assembly
        StringInterpolation  # Phase 8: string interpolation
        ArrayLiteral  # Phase 9: array literals [1, 2, 3]
        Block  # Phase 10: block {|x| ... } or do |x| ... end
        ProcLiteral  # Phase 74: proc literal ->(x) { ... }
        Yield  # Phase 10: yield keyword
        Case  # Phase 11: case/when pattern matching
        Select  # Phase 90A: select/when concurrent channel operations
        Break  # Phase 12: break [value]
        Next   # Phase 12: next
        Range  # Phase 13: range literals (1..10, 1...10)
        HashLiteral  # Phase 14: hash literals {"k"=>v}
        TupleLiteral  # Phase 15: tuple literals {1, 2, 3}
        NamedTupleLiteral  # Phase 70: named tuple literals {name: "value"}
        Symbol  # Phase 16: symbol literals :hello
        Ternary  # Phase 23: ternary operator (cond ? true : false)
        Begin  # Phase 28: begin/end blocks
        Raise  # Phase 29: raise exception
        Require  # Phase 65: require (import file/library)
        TypeDeclaration  # Phase 66: type declaration (x : Type)
        With  # Phase 67: with (context block)
        Getter  # Phase 30: getter macro
        Setter  # Phase 30: setter macro
        Property  # Phase 30: property macro (getter + setter)
        Module  # Phase 31: module definition
        Include  # Phase 31: include module into class/module
        Extend  # Phase 31: extend module into class/module
        Struct  # Phase 32: struct definition (value type)
        Union  # Phase 97: union definition (C bindings)
        Enum  # Phase 33: enum definition (enumerated type)
        Alias  # Phase 34: type alias
        AnnotationDef  # Phase 92: annotation definition (annotation MyAnnotation ... end)
        Annotation  # Phase 92: annotation use (@[Link], @[JSON::Field(...)])
        Constant  # Phase 35: constant declaration
        Lib  # Phase 38: lib (C bindings)
        Fun  # Phase 64: fun (C function declaration)
        As  # Phase 44: type cast (value.as(Type))
        AsQuestion  # Phase 45: safe cast (value.as?(Type))
        IsA  # Phase 93: type check (value.is_a?(Type))
        RespondsTo  # Phase 94: method check (value.responds_to?(:method))
        Generic  # Phase 60: generic type instantiation (Box(Int32))
        Path  # Phase 63: path expression (Foo::Bar)
        VisibilityModifier  # Phase 103J: private/protected wrapper
      end


      struct MacroPiece
        enum Kind
          Text
          Expression
          ControlStart
          ControlElseIf
          ControlElse
          ControlEnd
          MacroVar
        end

        getter kind : Kind
        getter text : String?
        getter expr : ExprId?
        getter control_keyword : String?
        getter trim_left : Bool
        getter trim_right : Bool
        getter iter_vars : Array(String)?
        getter iterable : ExprId?
        getter span : Span?
        getter macro_var_name : String?

        def self.text(value : String, span : Span? = nil)
          new(Kind::Text, value, nil, nil, false, false, nil, nil, span, nil)
        end

        def self.expression(expr : ExprId, trim_left : Bool = false, trim_right : Bool = false, span : Span? = nil)
          new(Kind::Expression, nil, expr, nil, trim_left, trim_right, nil, nil, span, nil)
        end

        def self.control(kind : Kind, keyword : String, expr : ExprId? = nil, trim_left : Bool = false, trim_right : Bool = false, iter_vars : Array(String)? = nil, iterable : ExprId? = nil, span : Span? = nil)
          new(kind, nil, expr, keyword, trim_left, trim_right, iter_vars, iterable, span, nil)
        end

        def self.macro_var(name : String, span : Span? = nil)
          new(Kind::MacroVar, nil, nil, nil, false, false, nil, nil, span, name)
        end

        def initialize(
          @kind : Kind,
          @text : String?,
          @expr : ExprId?,
          @control_keyword : String?,
          @trim_left : Bool = false,
          @trim_right : Bool = false,
          @iter_vars : Array(String)? = nil,
          @iterable : ExprId? = nil,
          @span : Span? = nil,
          @macro_var_name : String? = nil
        )
        end
      end

      # Represents a piece of an interpolated string
      #
      # Phase 8: String interpolation support
      # For "Hello, #{name}!" we have:
      # - Text("Hello, ")
      # - Expression(name_expr_id)
      # - Text("!")
      struct StringPiece
        enum Kind
          Text
          Expression
        end

        getter kind : Kind
        getter text : String?
        getter expr : ExprId?

        def self.text(value : String)
          new(Kind::Text, value, nil)
        end

        def self.expression(expr : ExprId)
          new(Kind::Expression, nil, expr)
        end

        def initialize(@kind : Kind, @text : String?, @expr : ExprId?)
        end
      end

      # ============================================================================
      # Phase B: Typed Arena Prototype - Memory-efficient node structures
      # ============================================================================
      #
      # Specialized struct types for 5 representative AST nodes as a prototype
      # for measuring memory improvements vs the former monolithic ExpressionNode
      # (1024 bytes with 100+ fields where 96% were nil).
      #
      # Design Principles:
      # 1. Minimal fields: Only what each node type actually needs
      # 2. Shared span: All nodes have Span for diagnostics
      # 3. ExprId references: Maintain Arena architecture (indices not pointers)
      # 4. Immutable structs: Same thread-safety benefits as the legacy
      #    ExpressionNode while drastically reducing size
      #
      # Memory Comparison:
      # | Node       | Legacy  | Typed | Savings |
      # |------------|---------|-------|---------|
      # | Number     | 1024 B  | ~48 B | 21x     |
      # | Identifier | 1024 B  | ~40 B | 25x     |
      # | Binary     | 1024 B  | ~56 B | 18x     |
      # | Call       | 1024 B  | ~64 B | 16x     |
      # | If         | 1024 B  | ~88 B | 11x     |
      #
      # Average: ~60 B vs 1024 B = 17x memory improvement

      # NumberNode: Integer and floating-point literals
      # Examples: 42, 3.14, 0x2A
      # Size: ~48 bytes
      class NumberNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Number
        end

        getter value : Slice(UInt8)
        getter kind : NumberKind

        def initialize(@span : Span, @value : Slice(UInt8), @kind : NumberKind)
        end
      end

      # IdentifierNode: Variable and method names
      # Examples: foo, self, initialize
      # Size: ~40 bytes
      class IdentifierNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Identifier
        end

        getter name : Slice(UInt8)

        def initialize(@span : Span, @name : Slice(UInt8))
        end
      end

      class MacroVarNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::MacroVar
        end

        getter name : Slice(UInt8)

        def initialize(@span : Span, @name : Slice(UInt8))
        end
      end

      # BinaryNode: Binary operations
      # Examples: a + b, x * y, foo == bar
      # Size: ~56 bytes
      class BinaryNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Binary
        end

        getter operator : Slice(UInt8)
        getter left : ExprId
        getter right : ExprId

        def initialize(@span : Span, @operator : Slice(UInt8), @left : ExprId, @right : ExprId)
        end

        def operator_string
          String.new(operator)
        end
      end

      # CallNode: Method and function calls
      # Examples: foo(a, b), obj.method, bar { |x| x + 1 }
      # Size: ~64 bytes
      class CallNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Call
        end

        getter callee : ExprId
        getter args : Array(ExprId)
        getter block : ExprId?
        getter named_args : Array(NamedArgument)?

        def initialize(@span : Span, @callee : ExprId, @args : Array(ExprId), @block : ExprId? = nil, @named_args : Array(NamedArgument)? = nil)
        end
      end

      # SplatNode: Splat argument in calls/yield
      # Examples: foo(*args), yield *tuple
      # Size: ~40 bytes
      class SplatNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Splat
        end

        getter expr : ExprId

        def initialize(@span : Span, @expr : ExprId)
        end
      end

      # IfNode: Conditional expressions
      # Examples: if condition then body end
      # Size: ~88 bytes
      class IfNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::If
        end

        getter condition : ExprId
        getter then_body : Array(ExprId)
        getter elsifs : Array(ElsifBranch)?
        getter else_body : Array(ExprId)?

        def initialize(
          @span : Span,
          @condition : ExprId,
          @then_body : Array(ExprId),
          @elsifs : Array(ElsifBranch)? = nil,
          @else_body : Array(ExprId)? = nil
        )
        end
      end

      # ============================================================================
      # Week 1 Batch 1: Literals Group (completing with NumberNode)
      # ============================================================================

      # StringNode: String literals
      # Examples: "hello", "world"
      # Size: ~40 bytes
      class StringNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::String
        end

        getter value : Slice(UInt8)

        def initialize(@span : Span, @value : Slice(UInt8))
        end
      end

      # CharNode: Character literals
      # Examples: 'a', 'z', '\n'
      # Size: ~40 bytes
      class CharNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Char
        end

        getter value : Slice(UInt8)

        def initialize(@span : Span, @value : Slice(UInt8))
        end
      end

      # RegexNode: Regular expression literals
      # Examples: /pattern/, /test/i
      # Size: ~40 bytes
      class RegexNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Regex
        end

        getter pattern : Slice(UInt8)

        def initialize(@span : Span, @pattern : Slice(UInt8))
        end
      end

      # BoolNode: Boolean literals
      # Examples: true, false
      # Size: ~32 bytes
      class BoolNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Bool
        end

        getter value : Bool

        def initialize(@span : Span, @value : Bool)
        end
      end

      # NilNode: Nil literal
      # Example: nil
      # Size: ~24 bytes (just span)
      class NilNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Nil
        end


        def initialize(@span : Span)
        end
      end

      # SymbolNode: Symbol literals
      # Examples: :foo, :bar, :hello
      # Size: ~40 bytes
      class SymbolNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Symbol
        end

        getter name : Slice(UInt8)

        def initialize(@span : Span, @name : Slice(UInt8))
        end
      end

      # ArrayLiteralNode: Array literals
      # Examples: [1, 2, 3], [] of Int32
      # Size: ~56 bytes
      class ArrayLiteralNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::ArrayLiteral
        end

        getter elements : Array(ExprId)
        getter of_type : ExprId?  # Phase 91: explicit type

        def initialize(@span : Span, @elements : Array(ExprId), @of_type : ExprId? = nil)
        end
      end

      # HashLiteralNode: Hash literals
      # Examples: {"key" => value}, {} of String => Int32
      # Size: ~56 bytes
      class HashLiteralNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::HashLiteral
        end

        getter entries : Array(HashEntry)
        getter of_key_type : Slice(UInt8)?
        getter of_value_type : Slice(UInt8)?

        def initialize(
          @span : Span,
          @entries : Array(HashEntry),
          @of_key_type : Slice(UInt8)? = nil,
          @of_value_type : Slice(UInt8)? = nil
        )
        end
      end

      # TupleLiteralNode: Tuple literals
      # Examples: {1, 2, 3}, {1, "hello", true}
      # Size: ~48 bytes
      class TupleLiteralNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::TupleLiteral
        end

        getter elements : Array(ExprId)

        def initialize(@span : Span, @elements : Array(ExprId))
        end
      end

      # NamedTupleLiteralNode: Named tuple literals
      # Examples: {name: "Alice", age: 30}
      # Size: ~48 bytes
      class NamedTupleLiteralNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::NamedTupleLiteral
        end

        getter entries : Array(NamedTupleEntry)

        def initialize(@span : Span, @entries : Array(NamedTupleEntry))
        end
      end

      # RangeNode: Range literals
      # Examples: 1..10, 1...10
      # Size: ~48 bytes
      class RangeNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Range
        end

        getter begin_expr : ExprId
        getter end_expr : ExprId
        getter exclusive : Bool

        def initialize(@span : Span, @begin_expr : ExprId, @end_expr : ExprId, @exclusive : Bool)
        end
      end

      # Week 1 Batch 2: Operators Group (completing with BinaryNode from Phase B)

      class UnaryNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Unary
        end

        getter operator : Slice(UInt8)  # -, !, ~, +, etc.
        getter operand : ExprId

        def initialize(@span : Span, @operator : Slice(UInt8), @operand : ExprId)
        end
      end

      class TernaryNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Ternary
        end

        getter condition : ExprId
        getter true_branch : ExprId
        getter false_branch : ExprId

        def initialize(@span : Span, @condition : ExprId, @true_branch : ExprId, @false_branch : ExprId)
        end
      end

      # Week 1 Batch 3: Variables Group (completing with IdentifierNode from Phase B)

      class InstanceVarNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::InstanceVar
        end

        getter name : Slice(UInt8)  # @var

        def initialize(@span : Span, @name : Slice(UInt8))
        end
      end

      class ClassVarNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::ClassVar
        end

        getter name : Slice(UInt8)  # @@var

        def initialize(@span : Span, @name : Slice(UInt8))
        end
      end

      class GlobalNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Global
        end

        getter name : Slice(UInt8)  # $var

        def initialize(@span : Span, @name : Slice(UInt8))
        end
      end

      class SelfNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Self
        end


        def initialize(@span : Span)
        end
      end

      # Phase IMPLICIT_RECEIVER: Implicit object for method calls without explicit receiver
      # Example: in .i8? (equivalent to self.i8?)
      class ImplicitObjNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::ImplicitObj
        end


        def initialize(@span : Span)
        end
      end

      # Week 1 Batch 4: Control Flow Group (completing with IfNode from Phase B)

      class UnlessNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Unless
        end

        getter condition : ExprId
        getter then_branch : Array(ExprId)
        getter else_branch : Array(ExprId)?

        def initialize(@span : Span, @condition : ExprId, @then_branch : Array(ExprId), @else_branch : Array(ExprId)? = nil)
        end
      end

      class WhileNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::While
        end

        getter condition : ExprId
        getter body : Array(ExprId)

        def initialize(@span : Span, @condition : ExprId, @body : Array(ExprId))
        end
      end

      class UntilNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Until
        end

        getter condition : ExprId
        getter body : Array(ExprId)

        def initialize(@span : Span, @condition : ExprId, @body : Array(ExprId))
        end
      end

      class ForNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::For
        end

        getter variable : Slice(UInt8)
        getter collection : ExprId
        getter body : Array(ExprId)

        def initialize(@span : Span, @variable : Slice(UInt8), @collection : ExprId, @body : Array(ExprId))
        end
      end

      class LoopNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Loop
        end

        getter body : Array(ExprId)

        def initialize(@span : Span, @body : Array(ExprId))
        end
      end

      class CaseNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Case
        end

        getter value : ExprId?
        getter when_branches : Array(WhenBranch)
        getter in_branches : Array(WhenBranch)?  # Phase PERCENT_LITERALS: pattern matching (case...in)
        getter else_branch : Array(ExprId)?

        def initialize(@span : Span, @value : ExprId?, @when_branches : Array(WhenBranch),
                       @else_branch : Array(ExprId)? = nil, @in_branches : Array(WhenBranch)? = nil)
        end
      end

      class BreakNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Break
        end

        getter value : ExprId?

        def initialize(@span : Span, @value : ExprId? = nil)
        end
      end

      class NextNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Next
        end


        def initialize(@span : Span)
        end
      end

      class ReturnNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Return
        end

        getter value : ExprId?

        def initialize(@span : Span, @value : ExprId? = nil)
        end
      end

      class YieldNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Yield
        end

        getter args : Array(ExprId)?

        def initialize(@span : Span, @args : Array(ExprId)? = nil)
        end
      end

      class SpawnNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Spawn
        end

        getter expression : ExprId?
        getter body : Array(ExprId)?

        def initialize(@span : Span, @expression : ExprId? = nil, @body : Array(ExprId)? = nil)
        end
      end

      # Week 1 Batch 5: Calls Group (completing with CallNode from Phase B)

      class IndexNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Index
        end

        getter object : ExprId
        getter indexes : Array(ExprId)

        def initialize(@span : Span, @object : ExprId, @indexes : Array(ExprId))
        end
      end

      class MemberAccessNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::MemberAccess
        end

        getter object : ExprId
        getter member : Slice(UInt8)

        def initialize(@span : Span, @object : ExprId, @member : Slice(UInt8))
        end
      end

      class SafeNavigationNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::SafeNavigation
        end

        getter object : ExprId
        getter member : Slice(UInt8)

        def initialize(@span : Span, @object : ExprId, @member : Slice(UInt8))
        end
      end

      # Week 1 Batch 6: Assignment Group

      class AssignNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Assign
        end

        getter target : ExprId
        getter value : ExprId

        def initialize(@span : Span, @target : ExprId, @value : ExprId)
        end
      end

      class MultipleAssignNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::MultipleAssign
        end

        getter targets : Array(ExprId)
        getter value : ExprId

        def initialize(@span : Span, @targets : Array(ExprId), @value : ExprId)
        end
      end

      # Week 1 Batch 7: Block/Proc and String Interpolation

      class BlockNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Block
        end

        getter params : Array(Parameter)?
        getter body : Array(ExprId)

        def initialize(@span : Span, @params : Array(Parameter)?, @body : Array(ExprId))
        end
      end

      class ProcLiteralNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::ProcLiteral
        end

        getter params : Array(Parameter)?
        getter return_type : Slice(UInt8)?
        getter body : Array(ExprId)

        def initialize(@span : Span, @params : Array(Parameter)?, @return_type : Slice(UInt8)?, @body : Array(ExprId))
        end
      end

      class StringInterpolationNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::StringInterpolation
        end

        getter pieces : Array(StringPiece)

        def initialize(@span : Span, @pieces : Array(StringPiece))
        end
      end

      class GroupingNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Grouping
        end

        getter expression : ExprId

        def initialize(@span : Span, @expression : ExprId)
        end
      end

      # Week 1 Batch 8: Definition Nodes

      class DefNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Def
        end

        getter name : Slice(UInt8)
        getter params : Array(Parameter)?
        getter return_type : Slice(UInt8)?
        getter body : Array(ExprId)?
        getter is_abstract : Bool?
        getter visibility : Visibility?
        getter receiver : Slice(UInt8)?  # Phase PERCENT_LITERALS: self or object name for class/singleton methods

        def initialize(@span : Span, @name : Slice(UInt8), @params : Array(Parameter)?,
                       @return_type : Slice(UInt8)?, @body : Array(ExprId)?,
                       @is_abstract : Bool? = nil, @visibility : Visibility? = nil,
                       @receiver : Slice(UInt8)? = nil)
        end
      end

      class ClassNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Class
        end

        getter name : Slice(UInt8)
        getter super_name : Slice(UInt8)?
        getter body : Array(ExprId)?
        getter is_abstract : Bool?
        getter is_struct : Bool?
        getter is_union : Bool?
        getter type_params : Array(Slice(UInt8))?

        def initialize(@span : Span, @name : Slice(UInt8), @super_name : Slice(UInt8)?,
                       @body : Array(ExprId)?, @is_abstract : Bool? = nil, @is_struct : Bool? = nil,
                       @is_union : Bool? = nil, @type_params : Array(Slice(UInt8))? = nil)
        end

        # Compatibility accessors (legacy ExpressionNode API)
        def class_body
          @body
        end

        def class_is_struct
          @is_struct
        end

        def class_is_union
          @is_union
        end

        def class_is_abstract
          @is_abstract
        end
      end

      class ModuleNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Module
        end

        getter name : Slice(UInt8)
        getter body : Array(ExprId)?
        getter type_params : Array(Slice(UInt8))?

        def initialize(@span : Span, @name : Slice(UInt8), @body : Array(ExprId)?, @type_params : Array(Slice(UInt8))?)
        end
      end

      class StructNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Struct
        end

        getter name : Slice(UInt8)
        getter body : Array(ExprId)?

        def initialize(@span : Span, @name : Slice(UInt8), @body : Array(ExprId)?)
        end
      end

      class UnionNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Union
        end

        getter name : Slice(UInt8)
        getter body : Array(ExprId)?

        def initialize(@span : Span, @name : Slice(UInt8), @body : Array(ExprId)?)
        end
      end

      class EnumNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Enum
        end

        getter name : Slice(UInt8)
        getter base_type : Slice(UInt8)?
        getter members : Array(EnumMember)
        getter body : Array(ExprId)?

        def initialize(@span : Span, @name : Slice(UInt8), @base_type : Slice(UInt8)?, @members : Array(EnumMember), @body : Array(ExprId)? = nil)
        end
      end

      class AliasNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Alias
        end

        getter name : Slice(UInt8)
        getter value : Slice(UInt8)

        def initialize(@span : Span, @name : Slice(UInt8), @value : Slice(UInt8))
        end
      end

      class ConstantNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Constant
        end

        getter name : Slice(UInt8)
        getter value : ExprId

        def initialize(@span : Span, @name : Slice(UInt8), @value : ExprId)
        end
      end

      class IncludeNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Include
        end

        getter name : Slice(UInt8)
        getter target : ExprId

        def initialize(@span : Span, @name : Slice(UInt8), @target : ExprId)
        end
      end

      class ExtendNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Extend
        end

        getter name : Slice(UInt8)
        getter target : ExprId

        def initialize(@span : Span, @name : Slice(UInt8), @target : ExprId)
        end
      end

      class GetterNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Getter
        end

        getter specs : Array(AccessorSpec)
        getter? is_class : Bool  # true for class_getter, false for getter

        def initialize(@span : Span, @specs : Array(AccessorSpec), @is_class : Bool = false)
        end
      end

      class SetterNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Setter
        end

        getter specs : Array(AccessorSpec)
        getter? is_class : Bool  # true for class_setter, false for setter

        def initialize(@span : Span, @specs : Array(AccessorSpec), @is_class : Bool = false)
        end
      end

      class PropertyNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Property
        end

        getter specs : Array(AccessorSpec)
        getter? is_class : Bool  # true for class_property, false for property

        def initialize(@span : Span, @specs : Array(AccessorSpec), @is_class : Bool = false)
        end
      end

      # AnnotationDefNode: Annotation definitions
      # Example: annotation MyAnnotation ... end
      # This is like a class definition for annotations
      class AnnotationDefNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::AnnotationDef
        end

        getter name : Slice(UInt8)    # Simple identifier (like ClassNode, StructNode)

        def initialize(@span : Span, @name : Slice(UInt8))
        end
      end

      # AnnotationNode: Annotation expressions (uses)
      # Examples: @[Link], @[JSON::Field(key: "test")]
      # Annotations are standalone expressions that are linked to following declarations
      # during semantic analysis
      class AnnotationNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Annotation
        end

        getter name : ExprId          # Path or identifier (e.g., Link, JSON::Field)
        getter args : Array(ExprId)   # Positional arguments
        getter named_args : Array(NamedArgument)?  # Named arguments (e.g., key: "test")

        def initialize(
          @span : Span,
          @name : ExprId,
          @args : Array(ExprId) = [] of ExprId,
          @named_args : Array(NamedArgument)? = nil
        )
        end
      end

      # Week 1 Batch 9: Special Operators

      class AsNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::As
        end

        getter expression : ExprId
        getter target_type : Slice(UInt8)

        def initialize(@span : Span, @expression : ExprId, @target_type : Slice(UInt8))
        end
      end

      class AsQuestionNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::AsQuestion
        end

        getter expression : ExprId
        getter target_type : Slice(UInt8)

        def initialize(@span : Span, @expression : ExprId, @target_type : Slice(UInt8))
        end
      end

      class IsANode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::IsA
        end

        getter expression : ExprId
        getter target_type : Slice(UInt8)

        def initialize(@span : Span, @expression : ExprId, @target_type : Slice(UInt8))
        end
      end

      class RespondsToNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::RespondsTo
        end

        getter expression : ExprId
        getter method_name : ExprId

        def initialize(@span : Span, @expression : ExprId, @method_name : ExprId)
        end
      end

      class TypeofNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Typeof
        end

        getter args : Array(ExprId)

        def initialize(@span : Span, @args : Array(ExprId))
        end
      end

      class SizeofNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Sizeof
        end

        getter args : Array(ExprId)

        def initialize(@span : Span, @args : Array(ExprId))
        end
      end

      class PointerofNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Pointerof
        end

        getter args : Array(ExprId)

        def initialize(@span : Span, @args : Array(ExprId))
        end
      end

      class UninitializedNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Uninitialized
        end

        getter type : ExprId

        def initialize(@span : Span, @type : ExprId)
        end
      end

      class OffsetofNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Offsetof
        end

        getter args : Array(ExprId)

        def initialize(@span : Span, @args : Array(ExprId))
        end
      end

      class AlignofNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Alignof
        end

        getter args : Array(ExprId)

        def initialize(@span : Span, @args : Array(ExprId))
        end
      end

      class InstanceAlignofNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::InstanceAlignof
        end

        getter args : Array(ExprId)

        def initialize(@span : Span, @args : Array(ExprId))
        end
      end

      class SuperNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Super
        end

        getter args : Array(ExprId)?

        def initialize(@span : Span, @args : Array(ExprId)? = nil)
        end
      end

      class PreviousDefNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::PreviousDef
        end

        getter args : Array(ExprId)?

        def initialize(@span : Span, @args : Array(ExprId)? = nil)
        end
      end

      class OutNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Out
        end

        getter identifier : Slice(UInt8)

        def initialize(@span : Span, @identifier : Slice(UInt8))
        end
      end

      # Week 1 Batch 10: Advanced Nodes

      class BeginNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Begin
        end

        getter body : Array(ExprId)
        getter rescue_clauses : Array(RescueClause)?
        getter else_body : Array(ExprId)?
        getter ensure_body : Array(ExprId)?

        def initialize(@span : Span, @body : Array(ExprId), @rescue_clauses : Array(RescueClause)? = nil, @else_body : Array(ExprId)? = nil, @ensure_body : Array(ExprId)? = nil)
        end
      end

      class RaiseNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Raise
        end

        getter value : ExprId?

        def initialize(@span : Span, @value : ExprId? = nil)
        end
      end

      class RequireNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Require
        end

        getter path : ExprId

        def initialize(@span : Span, @path : ExprId)
        end
      end

      class TypeDeclarationNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::TypeDeclaration
        end

        getter name : Slice(UInt8)  # Variable name (simple identifier)
        getter declared_type : Slice(UInt8)  # Type name (simple identifier like Int32, String)
        getter value : ExprId?  # Phase 103: Optional initial value for x : Type = value

        def initialize(@span : Span, @name : Slice(UInt8), @declared_type : Slice(UInt8), @value : ExprId? = nil)
        end
      end

      class InstanceVarDeclNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::InstanceVarDecl
        end

        getter name : Slice(UInt8)
        getter type : Slice(UInt8)
        getter value : ExprId?

        def initialize(@span : Span, @name : Slice(UInt8), @type : Slice(UInt8), @value : ExprId? = nil)
        end
      end

      class ClassVarDeclNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::ClassVarDecl
        end

        getter name : Slice(UInt8)
        getter type : Slice(UInt8)
        getter value : ExprId?

        def initialize(@span : Span, @name : Slice(UInt8), @type : Slice(UInt8), @value : ExprId? = nil)
        end
      end

      class GlobalVarDeclNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::GlobalVarDecl
        end

        getter name : Slice(UInt8)
        getter type : Slice(UInt8)

        def initialize(@span : Span, @name : Slice(UInt8), @type : Slice(UInt8))
        end

        # Compatibility alias
        def ivar_decl_type
          @type
        end
      end

      class WithNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::With
        end

        getter receiver : ExprId
        getter body : Array(ExprId)

        def initialize(@span : Span, @receiver : ExprId, @body : Array(ExprId))
        end
      end

      class LibNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Lib
        end

        getter name : Slice(UInt8)
        getter body : Array(ExprId)?

        def initialize(@span : Span, @name : Slice(UInt8), @body : Array(ExprId)?)
        end
      end

      class FunNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Fun
        end

        getter name : Slice(UInt8)
        getter real_name : Slice(UInt8)?  # Actual C function name (if different from name)
        getter params : Array(Parameter)?
        getter return_type : Slice(UInt8)?
        getter varargs : Bool  # Whether function has ... for variadic args

        def initialize(@span : Span, @name : Slice(UInt8), @real_name : Slice(UInt8)?, @params : Array(Parameter)?, @return_type : Slice(UInt8)?, @varargs : Bool)
        end
      end

      class GenericNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Generic
        end

        getter base_type : ExprId
        getter type_args : Array(ExprId)

        def initialize(@span : Span, @base_type : ExprId, @type_args : Array(ExprId))
        end
      end

      class PathNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Path
        end

        getter left : ExprId?
        getter right : ExprId

        def initialize(@span : Span, @left : ExprId?, @right : ExprId)
        end
      end

      class MacroExpressionNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::MacroExpression
        end

        getter expression : ExprId

        def initialize(@span : Span, @expression : ExprId)
        end
      end

      class MacroLiteralNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::MacroLiteral
        end

        getter pieces : Array(MacroPiece)
        getter trim_left : Bool
        getter trim_right : Bool

        def initialize(@span : Span, @pieces : Array(MacroPiece), @trim_left : Bool, @trim_right : Bool)
        end

        # Legacy API compatibility
        def macro_pieces
          @pieces
        end

        def left
          raise NoMethodError.new("MacroLiteralNode does not have left")
        end
      end

      class MacroDefNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::MacroDef
        end

        getter name : Slice(UInt8)
        getter body : ExprId

        def initialize(@span : Span, @name : Slice(UInt8), @body : ExprId)
        end

        # Legacy alias (ExpressionNode#left)
        def left
          @body
        end
      end

      # Phase 103B: Macro control flow in expression context
      class MacroIfNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::MacroIf
        end

        getter condition : ExprId
        getter then_body : ExprId  # MacroLiteralNode
        getter else_body : ExprId? # MacroIfNode (elsif) or MacroLiteralNode (else) or nil

        def initialize(@span : Span, @condition : ExprId, @then_body : ExprId, @else_body : ExprId? = nil)
        end
      end

      class MacroForNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::MacroFor
        end

        getter iter_vars : Array(Slice(UInt8))  # Variable names
        getter iterable : ExprId   # Expression to iterate over
        getter body : ExprId       # MacroLiteralNode

        def initialize(@span : Span, @iter_vars : Array(Slice(UInt8)), @iterable : ExprId, @body : ExprId)
        end
      end

      class SelectNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Select
        end

        getter branches : Array(SelectBranch)
        getter else_branch : Array(ExprId)?

        def initialize(@span : Span, @branches : Array(SelectBranch), @else_branch : Array(ExprId)? = nil)
        end
      end

      class AsmNode < Node
        getter span : Span
        def node_kind : NodeKind
          NodeKind::Asm
        end

        getter args : Array(ExprId)

        def initialize(@span : Span, @args : Array(ExprId))
        end
      end

      # ============================================================================

      # TypedNode: Discriminated union of typed node types
      # Crystal adds a tag (4-8 bytes) to identify which type is stored.
      # Total size = tag + max(all structs) = 8 + 88 = 96 bytes worst case
      # Still 10x better than 1024-byte ExpressionNode!
      alias TypedNode = Node

      # ============================================================================
      # Helper: Get Kind for any node type (ExpressionNode or TypedNode)
      # This allows gradual migration from .kind checks to pattern matching
      # ============================================================================


      def self.node_kind(node : NumberNode) : NodeKind
        NodeKind::Number
      end

      def self.node_kind(node : IdentifierNode) : NodeKind
        NodeKind::Identifier
      end

      def self.node_kind(node : MacroVarNode) : NodeKind
        NodeKind::MacroVar
      end

      def self.node_kind(node : BinaryNode) : NodeKind
        NodeKind::Binary
      end

      def self.node_kind(node : CallNode) : NodeKind
        NodeKind::Call
      end

      def self.node_kind(node : SplatNode) : NodeKind
        # There is no dedicated NodeKind for SplatNode in this lightweight
        # classification. Treat it as a Call argument-level construct; most
        # places won't branch on this. We can reuse Unary as closest fit.
        NodeKind::Unary
      end

      def self.node_kind(node : IfNode) : NodeKind
        NodeKind::If
      end

      def self.node_kind(node : StringNode) : NodeKind
        NodeKind::String
      end

      def self.node_kind(node : CharNode) : NodeKind
        NodeKind::Char
      end

      def self.node_kind(node : RegexNode) : NodeKind
        NodeKind::Regex
      end

      def self.node_kind(node : BoolNode) : NodeKind
        NodeKind::Bool
      end

      def self.node_kind(node : NilNode) : NodeKind
        NodeKind::Nil
      end

      def self.node_kind(node : SymbolNode) : NodeKind
        NodeKind::Symbol
      end

      def self.node_kind(node : ArrayLiteralNode) : NodeKind
        NodeKind::ArrayLiteral
      end

      def self.node_kind(node : HashLiteralNode) : NodeKind
        NodeKind::HashLiteral
      end

      def self.node_kind(node : TupleLiteralNode) : NodeKind
        NodeKind::TupleLiteral
      end

      def self.node_kind(node : NamedTupleLiteralNode) : NodeKind
        NodeKind::NamedTupleLiteral
      end

      def self.node_kind(node : RangeNode) : NodeKind
        NodeKind::Range
      end

      def self.node_kind(node : UnaryNode) : NodeKind
        NodeKind::Unary
      end

      def self.node_kind(node : TernaryNode) : NodeKind
        NodeKind::Ternary
      end

      def self.node_kind(node : InstanceVarNode) : NodeKind
        NodeKind::InstanceVar
      end

      def self.node_kind(node : ClassVarNode) : NodeKind
        NodeKind::ClassVar
      end

      def self.node_kind(node : GlobalNode) : NodeKind
        NodeKind::Global
      end

      def self.node_kind(node : SelfNode) : NodeKind
        NodeKind::Self
      end

      def self.node_kind(node : ImplicitObjNode) : NodeKind
        NodeKind::ImplicitObj
      end

      def self.node_kind(node : UnlessNode) : NodeKind
        NodeKind::Unless
      end

      def self.node_kind(node : WhileNode) : NodeKind
        NodeKind::While
      end

      def self.node_kind(node : UntilNode) : NodeKind
        NodeKind::Until
      end

      def self.node_kind(node : ForNode) : NodeKind
        NodeKind::For
      end

      def self.node_kind(node : LoopNode) : NodeKind
        NodeKind::Loop
      end

      def self.node_kind(node : CaseNode) : NodeKind
        NodeKind::Case
      end

      def self.node_kind(node : BreakNode) : NodeKind
        NodeKind::Break
      end

      def self.node_kind(node : NextNode) : NodeKind
        NodeKind::Next
      end

      def self.node_kind(node : ReturnNode) : NodeKind
        NodeKind::Return
      end

      def self.node_kind(node : YieldNode) : NodeKind
        NodeKind::Yield
      end

      def self.node_kind(node : SpawnNode) : NodeKind
        NodeKind::Spawn
      end

      def self.node_kind(node : IndexNode) : NodeKind
        NodeKind::Index
      end

      def self.node_kind(node : MemberAccessNode) : NodeKind
        NodeKind::MemberAccess
      end

      def self.node_kind(node : SafeNavigationNode) : NodeKind
        NodeKind::SafeNavigation
      end

      def self.node_kind(node : AssignNode) : NodeKind
        NodeKind::Assign
      end

      def self.node_kind(node : MultipleAssignNode) : NodeKind
        NodeKind::MultipleAssign
      end

      def self.node_kind(node : BlockNode) : NodeKind
        NodeKind::Block
      end

      def self.node_kind(node : ProcLiteralNode) : NodeKind
        NodeKind::ProcLiteral
      end

      def self.node_kind(node : StringInterpolationNode) : NodeKind
        NodeKind::StringInterpolation
      end

      def self.node_kind(node : GroupingNode) : NodeKind
        NodeKind::Grouping
      end

      def self.node_kind(node : DefNode) : NodeKind
        NodeKind::Def
      end

      def self.node_kind(node : ClassNode) : NodeKind
        if node.is_union
          NodeKind::Union
        elsif node.is_struct
          NodeKind::Struct
        else
          NodeKind::Class
        end
      end

      def self.node_kind(node : ModuleNode) : NodeKind
        NodeKind::Module
      end

      def self.node_kind(node : StructNode) : NodeKind
        NodeKind::Struct
      end

      def self.node_kind(node : UnionNode) : NodeKind
        NodeKind::Union
      end

      def self.node_kind(node : EnumNode) : NodeKind
        NodeKind::Enum
      end

      def self.node_kind(node : AliasNode) : NodeKind
        NodeKind::Alias
      end

      def self.node_kind(node : ConstantNode) : NodeKind
        NodeKind::Constant
      end

      def self.node_kind(node : IncludeNode) : NodeKind
        NodeKind::Include
      end

      def self.node_kind(node : ExtendNode) : NodeKind
        NodeKind::Extend
      end

      def self.node_kind(node : GetterNode) : NodeKind
        NodeKind::Getter
      end

      def self.node_kind(node : SetterNode) : NodeKind
        NodeKind::Setter
      end

      def self.node_kind(node : PropertyNode) : NodeKind
        NodeKind::Property
      end

      def self.node_kind(node : AnnotationDefNode) : NodeKind
        NodeKind::AnnotationDef
      end

      def self.node_kind(node : AnnotationNode) : NodeKind
        NodeKind::Annotation
      end

      def self.node_kind(node : AsNode) : NodeKind
        NodeKind::As
      end

      def self.node_kind(node : AsQuestionNode) : NodeKind
        NodeKind::AsQuestion
      end

      def self.node_kind(node : IsANode) : NodeKind
        NodeKind::IsA
      end

      def self.node_kind(node : RespondsToNode) : NodeKind
        NodeKind::RespondsTo
      end

      def self.node_kind(node : TypeofNode) : NodeKind
        NodeKind::Typeof
      end

      def self.node_kind(node : SizeofNode) : NodeKind
        NodeKind::Sizeof
      end

      def self.node_kind(node : PointerofNode) : NodeKind
        NodeKind::Pointerof
      end

      def self.node_kind(node : UninitializedNode) : NodeKind
        NodeKind::Uninitialized
      end

      def self.node_kind(node : OffsetofNode) : NodeKind
        NodeKind::Offsetof
      end

      def self.node_kind(node : AlignofNode) : NodeKind
        NodeKind::Alignof
      end

      def self.node_kind(node : InstanceAlignofNode) : NodeKind
        NodeKind::InstanceAlignof
      end

      def self.node_kind(node : SuperNode) : NodeKind
        NodeKind::Super
      end

      def self.node_kind(node : PreviousDefNode) : NodeKind
        NodeKind::PreviousDef
      end

      def self.node_kind(node : OutNode) : NodeKind
        NodeKind::Out
      end

      def self.node_kind(node : BeginNode) : NodeKind
        NodeKind::Begin
      end

      def self.node_kind(node : RaiseNode) : NodeKind
        NodeKind::Raise
      end

      def self.node_kind(node : RequireNode) : NodeKind
        NodeKind::Require
      end

      def self.node_kind(node : TypeDeclarationNode) : NodeKind
        NodeKind::TypeDeclaration
      end

      def self.node_kind(node : InstanceVarDeclNode) : NodeKind
        NodeKind::InstanceVarDecl
      end

      def self.node_kind(node : ClassVarDeclNode) : NodeKind
        NodeKind::ClassVarDecl
      end

      def self.node_kind(node : GlobalVarDeclNode) : NodeKind
        NodeKind::GlobalVarDecl
      end

      def self.node_kind(node : WithNode) : NodeKind
        NodeKind::With
      end

      def self.node_kind(node : LibNode) : NodeKind
        NodeKind::Lib
      end

      def self.node_kind(node : FunNode) : NodeKind
        NodeKind::Fun
      end

      def self.node_kind(node : GenericNode) : NodeKind
        NodeKind::Generic
      end

      def self.node_kind(node : PathNode) : NodeKind
        NodeKind::Path
      end

      def self.node_kind(node : VisibilityModifierNode) : NodeKind
        NodeKind::VisibilityModifier
      end

      def self.node_kind(node : MacroExpressionNode) : NodeKind
        NodeKind::MacroExpression
      end

      def self.node_kind(node : MacroLiteralNode) : NodeKind
        NodeKind::MacroLiteral
      end

      def self.node_kind(node : MacroDefNode) : NodeKind
        NodeKind::MacroDef
      end

      def self.node_kind(node : MacroIfNode) : NodeKind
        NodeKind::MacroIf
      end

      def self.node_kind(node : MacroForNode) : NodeKind
        NodeKind::MacroFor
      end

      def self.node_kind(node : SelectNode) : NodeKind
        NodeKind::Select
      end

      def self.node_kind(node : AsmNode) : NodeKind
        NodeKind::Asm
      end

      # ============================================================================
      # Helper: Get literal/name for nodes that have string data
      # ============================================================================


      def self.node_literal(node : IdentifierNode) : Slice(UInt8)?
        node.name
      end

      def self.node_literal(node : MacroVarNode) : Slice(UInt8)?
        node.name
      end

      def self.node_literal(node : InstanceVarNode) : Slice(UInt8)?
        node.name
      end

      def self.node_literal(node : ClassVarNode) : Slice(UInt8)?
        node.name
      end

      def self.node_literal(node : GlobalNode) : Slice(UInt8)?
        node.name
      end

      def self.node_literal(node : CharNode) : Slice(UInt8)?
        node.value
      end

      def self.node_literal(node : StringNode) : Slice(UInt8)?
        node.value
      end

      def self.node_literal(node : SymbolNode) : Slice(UInt8)?
        node.name
      end

      def self.node_literal(node : NumberNode) : Slice(UInt8)?
        node.value
      end

      # Helper: Get NumberNode kind (I32, F64, etc.)

      def self.node_number_kind(node : NumberNode) : NumberKind?
        node.kind
      end

      def self.node_number_kind(node : TypedNode) : NumberKind?
        nil
      end

      def self.node_literal(node : RegexNode) : Slice(UInt8)?
        node.pattern
      end

      def self.node_literal(node : ConstantNode) : Slice(UInt8)?
        node.name
      end

      def self.node_literal(node : InstanceVarDeclNode) : Slice(UInt8)?
        node.name
      end

      # Default: return nil for nodes that don't have literal data
      def self.node_literal(node : TypedNode) : Slice(UInt8)?
        nil
      end

      # Helper: Get literal as String (convenience method)
      def self.node_literal_string(node : TypedNode) : String?
        # Special case: BoolNode stores Bool value, not Slice(UInt8)
        if node.is_a?(BoolNode)
          return node.value ? "true" : "false"
        end

        node_literal(node).try { |slice| String.new(slice) }
      end

      # ============================================================================
      # Field access helpers for typed nodes (polymorphic dispatch)
      # ============================================================================

      # assign_value: Get value from assignment

      def self.node_assign_value(node : AssignNode) : ExprId
        node.value
      end

      def self.node_assign_value(node : TypedNode) : ExprId?
        nil  # Other typed nodes don't have assign_value
      end

      # assign_target: Get target from assignment

      def self.node_assign_target(node : AssignNode) : ExprId
        node.target
      end

      def self.node_assign_target(node : TypedNode) : ExprId?
        nil
      end

      # left: Get left operand (binary operations)

      def self.node_left(node : BinaryNode) : ExprId
        node.left
      end

      def self.node_left(node : GroupingNode) : ExprId
        node.expression
      end

      def self.node_left(node : IndexNode) : ExprId
        node.object
      end

      def self.node_left(node : MemberAccessNode) : ExprId
        node.object
      end

      def self.node_left(node : SafeNavigationNode) : ExprId
        node.object
      end

      def self.node_left(node : MacroDefNode) : ExprId
        node.body
      end

      def self.node_left(node : TypedNode) : ExprId?
        nil
      end

      # right: Get right operand (binary operations)

      def self.node_right(node : BinaryNode) : ExprId
        node.right
      end

      def self.node_right(node : UnaryNode) : ExprId
        node.operand  # UnaryNode uses 'operand' instead of 'right'
      end

      def self.node_right(node : TypedNode) : ExprId?
        nil
      end

      # macro_expr: Get macro expression (MacroExpressionNode.expression vs ExpressionNode.macro_expr)

      def self.node_macro_expr(node : MacroExpressionNode) : ExprId?
        node.expression  # Different field name!
      end

      def self.node_macro_expr(node : TypedNode) : ExprId?
        nil
      end

      # condition: Get condition (if/while/until/etc)

      def self.node_condition(node : IfNode) : ExprId
        node.condition
      end

      def self.node_condition(node : WhileNode) : ExprId
        node.condition
      end

      def self.node_condition(node : UntilNode) : ExprId
        node.condition
      end

      def self.node_condition(node : UnlessNode) : ExprId
        node.condition
      end

      def self.node_condition(node : TernaryNode) : ExprId
        node.condition
      end

      def self.node_condition(node : TypedNode) : ExprId?
        nil
      end

      # return_value: Get return value

      def self.node_return_value(node : ReturnNode) : ExprId?
        node.value
      end

      def self.node_return_value(node : TypedNode) : ExprId?
        nil
      end

      # if_elsifs: Get elsif branches

      def self.node_if_elsifs(node : IfNode) : Array(ElsifBranch)?
        node.elsifs
      end

      def self.node_if_elsifs(node : TypedNode) : Array(ElsifBranch)?
        nil
      end

      # operator_string: Get operator as string (convenience method)

      def self.node_operator_string(node : BinaryNode) : String?
        String.new(node.operator)
      end

      def self.node_operator_string(node : UnaryNode) : String?
        String.new(node.operator)
      end

      def self.node_operator_string(node : TypedNode) : String?
        nil
      end

      # string_pieces: Get string interpolation pieces

def self.node_string_pieces(node : StringInterpolationNode) : Array(StringPiece)
  node.pieces
end

def self.node_string_pieces(node : TypedNode) : Array(StringPiece)?
  nil
end


      # asm_args: Get asm args

def self.node_asm_args(node : AsmNode)
  node.args
end

def self.node_asm_args(node : TypedNode)
  nil
end

      # assign_targets: Get assign targets

      def self.node_assign_targets(node : TypedNode)
        nil
      end

      def self.node_hash_entries(node : HashLiteralNode)
        node.entries
      end

      def self.node_hash_of_key_type(node : HashLiteralNode)
        node.of_key_type
      end

      def self.node_hash_of_value_type(node : HashLiteralNode)
        node.of_value_type
      end

      def self.node_case_value(node : CaseNode)
        node.value
      end

      def self.node_when_branches(node : CaseNode)
        node.when_branches
      end

      def self.node_case_else(node : CaseNode)
        node.else_branch
      end

      # call_block: Get call block

def self.node_call_block(node : CallNode) : ExprId?
  node.block
end

def self.node_call_block(node : TypedNode)
  nil
end

      # case_else: Get case else

      def self.node_case_else(node : TypedNode)
        nil
      end

      # case_value: Get case value

      def self.node_case_value(node : TypedNode)
        nil
      end

      # for_body: Get for body

      def self.node_for_body(node : TypedNode)
        nil
      end

      # for_collection: Get for collection

      def self.node_for_collection(node : TypedNode)
        nil
      end

      # hash_entries: Get hash entries

      def self.node_hash_entries(node : TypedNode)
        nil
      end

      # hash_of_key_type: Get hash of key type

      def self.node_hash_of_key_type(node : TypedNode)
        nil
      end

      # hash_of_value_type: Get hash of value type

      def self.node_hash_of_value_type(node : TypedNode)
        nil
      end

      # member_string: Get member string

      def self.node_member_string(node : MemberAccessNode)
        String.new(node.member)
      end

      def self.node_member_string(node : SafeNavigationNode)
        String.new(node.member)
      end

      def self.node_member_string(node : TypedNode)
        nil
      end

      # named_args: Get named args

      def self.node_named_args(node : TypedNode)
        nil
      end

      # when_branches: Get when branches

      def self.node_when_branches(node : TypedNode)
        nil
      end

      # yield_args: Get yield args

      def self.node_yield_args(node : TypedNode)
        nil
      end

      # as_question_target_type: Get target type for safe cast

      def self.node_as_question_target_type(node : AsQuestionNode) : Slice(UInt8)?
        node.target_type
      end

      def self.node_as_question_target_type(node : TypedNode) : Slice(UInt8)?
        nil
      end

      # case_whens: Get when branches (note: different name!)

      def self.node_case_whens(node : CaseNode)
        node.when_branches
      end

      def self.node_case_whens(node : TypedNode)
        nil
      end

      # array_elements: Get array elements

      def self.node_array_elements(node : ArrayLiteralNode)
        node.elements
      end

      def self.node_array_elements(node : TypedNode)
        nil
      end

      # tuple_elements: Get tuple elements

      def self.node_tuple_elements(node : TupleLiteralNode)
        node.elements
      end

      def self.node_tuple_elements(node : TypedNode)
        nil
      end

      # operand: Get operand for unary operations

      def self.node_operand(node : UnaryNode) : ExprId
        node.operand
      end

      def self.node_operand(node : TypedNode) : ExprId?
        nil
      end

# Generated helpers for fields actually used in tests
# ============================================================================

# accessor_specs

def self.node_accessor_specs(node : GetterNode)
  node.specs
end

def self.node_accessor_specs(node : SetterNode)
  node.specs
end

def self.node_accessor_specs(node : PropertyNode)
  node.specs
end

def self.node_accessor_specs(node : TypedNode)
  nil
end

# alias_name

def self.node_alias_name(node : AliasNode)
  node.name
end

def self.node_alias_name(node : TypedNode)
  nil
end

# alias_value

def self.node_alias_value(node : AliasNode)
  node.value
end

def self.node_alias_value(node : TypedNode)
  nil
end

# alignof_args

def self.node_alignof_args(node : AlignofNode)
  node.args
end

def self.node_alignof_args(node : InstanceAlignofNode)
  node.args
end

def self.node_alignof_args(node : TypedNode)
  nil
end

# annotation_name

def self.node_annotation_name(node : AnnotationDefNode)
  node.name
end

def self.node_annotation_name(node : AnnotationNode)
  node.name
end

def self.node_annotation_name(node : TypedNode)
  nil
end

# as_question_value

def self.node_as_question_value(node : AsQuestionNode)
  node.expression
end

def self.node_as_question_value(node : TypedNode)
  nil
end

# as_target_type

def self.node_as_target_type(node : AsNode)
  node.target_type
end

def self.node_as_target_type(node : TypedNode)
  nil
end

# as_value

def self.node_as_value(node : AsNode)
  node.expression
end

def self.node_as_value(node : TypedNode)
  nil
end

# begin_body

def self.node_begin_body(node : BeginNode)
  node.body
end

def self.node_begin_body(node : TypedNode)
  nil
end

# else_body

def self.node_else_body(node : BeginNode)
  node.else_body
end

def self.node_else_body(node : TypedNode)
  nil
end

# block_body

def self.node_block_body(node : BlockNode)
  node.body
end

def self.node_block_body(node : ProcLiteralNode)
  node.body
end

def self.node_block_body(node : TypedNode)
  nil
end

# block_params

def self.node_block_params(node : BlockNode)
  node.params
end

def self.node_block_params(node : ProcLiteralNode)
  node.params
end

def self.node_block_params(node : TypedNode)
  nil
end

# class_body

def self.node_class_body(node : ClassNode)
  node.body
end

def self.node_class_body(node : StructNode)
  node.body
end

def self.node_class_body(node : UnionNode)
  node.body
end

def self.node_class_body(node : TypedNode)
  nil
end

# class_is_abstract

def self.node_class_is_abstract(node : ClassNode)
  node.is_abstract
end

def self.node_class_is_abstract(node : TypedNode)
  nil
end


def self.node_class_is_union(node : ClassNode)
  node.is_union
end

def self.node_class_is_union(node : TypedNode)
  nil
end


def self.node_class_is_struct(node : ClassNode)
  node.is_struct
end

def self.node_class_is_struct(node : TypedNode)
  nil
end

# class_name

def self.node_class_name(node : ClassNode)
  node.name
end

def self.node_class_name(node : StructNode)
  node.name
end

def self.node_class_name(node : UnionNode)
  node.name
end

def self.node_class_name(node : TypedNode)
  nil
end

# class_super_name

def self.node_class_super_name(node : ClassNode)
  node.super_name
end

def self.node_class_super_name(node : TypedNode)
  nil
end

# constant_name

def self.node_constant_name(node : ConstantNode)
  node.name
end

def self.node_constant_name(node : TypedNode)
  nil
end

# constant_value

def self.node_constant_value(node : ConstantNode)
  node.value
end

def self.node_constant_value(node : TypedNode)
  nil
end

# def_body

def self.node_def_body(node : DefNode)
  node.body
end

def self.node_def_body(node : TypedNode)
  nil
end

# def_is_abstract

def self.node_def_is_abstract(node : DefNode)
  node.is_abstract
end

def self.node_def_is_abstract(node : TypedNode)
  nil
end

# def_name

def self.node_def_name(node : DefNode)
  node.name
end

def self.node_def_name(node : TypedNode)
  nil
end

# def_params

def self.node_def_params(node : DefNode)
  node.params
end

def self.node_def_params(node : TypedNode)
  nil
end

# def_return_type

def self.node_def_return_type(node : DefNode)
  node.return_type
end

def self.node_def_return_type(node : TypedNode)
  nil
end

# def_visibility

def self.node_def_visibility(node : DefNode)
  node.visibility
end

def self.node_def_visibility(node : TypedNode)
  nil
end

# ensure_body

def self.node_ensure_body(node : BeginNode)
  node.ensure_body
end

def self.node_ensure_body(node : TypedNode)
  nil
end

# enum_base_type

def self.node_enum_base_type(node : EnumNode)
  node.base_type
end

def self.node_enum_base_type(node : TypedNode)
  nil
end

# enum_members

def self.node_enum_members(node : EnumNode)
  node.members
end

def self.node_enum_members(node : TypedNode)
  nil
end

# enum_body

def self.node_enum_body(node : EnumNode)
  node.body
end

def self.node_enum_body(node : TypedNode)
  nil
end

# enum_name

def self.node_enum_name(node : EnumNode)
  node.name
end

def self.node_enum_name(node : TypedNode)
  nil
end

# generic_name

def self.node_generic_name(node : GenericNode)
  node.base_type
end

def self.node_generic_name(node : TypedNode)
  nil
end

# generic_type_args

def self.node_generic_type_args(node : GenericNode)
  node.type_args
end

def self.node_generic_type_args(node : TypedNode)
  nil
end

# if_else

def self.node_if_else(node : IfNode)
  node.else_body
end

def self.node_if_else(node : UnlessNode)
  node.else_branch
end

def self.node_if_else(node : TypedNode)
  nil
end

# if_then

def self.node_if_then(node : IfNode)
  node.then_body
end

def self.node_if_then(node : UnlessNode)
  node.then_branch
end

def self.node_if_then(node : TypedNode)
  nil
end

# instance_alignof_args

def self.node_instance_alignof_args(node : InstanceAlignofNode)
  node.args
end

def self.node_instance_alignof_args(node : TypedNode)
  nil
end

# is_a_target_type

def self.node_is_a_target_type(node : IsANode)
  node.target_type
end

def self.node_is_a_target_type(node : TypedNode)
  nil
end

# is_a_value

def self.node_is_a_value(node : IsANode)
  node.expression
end

def self.node_is_a_value(node : TypedNode)
  nil
end

# lib_body

def self.node_lib_body(node : LibNode)
  node.body
end

def self.node_lib_body(node : TypedNode)
  nil
end

# lib_name

def self.node_lib_name(node : LibNode)
  node.name
end

def self.node_lib_name(node : TypedNode)
  nil
end

# loop_body

def self.node_loop_body(node : LoopNode)
  node.body
end

def self.node_loop_body(node : TypedNode)
  nil
end

# member

def self.node_member(node : MemberAccessNode)
  node.member
end

def self.node_member(node : SafeNavigationNode)
  node.member
end

def self.node_member(node : TypedNode)
  nil
end

# module_body

def self.node_module_body(node : ModuleNode)
  node.body
end

def self.node_module_body(node : TypedNode)
  nil
end

# module_name

def self.node_module_name(node : ModuleNode)
  node.name
end

def self.node_module_name(node : TypedNode)
  nil
end

# named_tuple_entries

def self.node_named_tuple_entries(node : NamedTupleLiteralNode)
  node.entries
end

def self.node_named_tuple_entries(node : TypedNode)
  nil
end

# offsetof_args

def self.node_offsetof_args(node : OffsetofNode)
  node.args
end

def self.node_offsetof_args(node : TypedNode)
  nil
end

# out_identifier

def self.node_out_identifier(node : OutNode)
  node.identifier
end

def self.node_out_identifier(node : TypedNode)
  nil
end

# pointerof_args

def self.node_pointerof_args(node : PointerofNode)
  node.args
end

def self.node_pointerof_args(node : TypedNode)
  nil
end

# previous_def_args

def self.node_previous_def_args(node : PreviousDefNode)
  node.args
end

def self.node_previous_def_args(node : TypedNode)
  nil
end

# proc_return_type

def self.node_proc_return_type(node : ProcLiteralNode)
  node.return_type
end

def self.node_proc_return_type(node : TypedNode)
  nil
end

# raise_value

def self.node_raise_value(node : RaiseNode)
  node.value
end

def self.node_raise_value(node : TypedNode)
  nil
end

# require_path

def self.node_require_path(node : RequireNode)
  node.path
end

def self.node_require_path(node : TypedNode)
  nil
end

# rescue_clauses

def self.node_rescue_clauses(node : BeginNode)
  node.rescue_clauses
end

def self.node_rescue_clauses(node : TypedNode)
  nil
end

# responds_to_method_name (RespondsTo uses ExpressionNode, not RespondsToNode typed struct)

def self.node_responds_to_method_name(node : TypedNode) : ExprId?
  nil  # RespondsToNode exists but is unused by parser
end

# responds_to_value

def self.node_responds_to_value(node : RespondsToNode)
  node.expression
end

def self.node_responds_to_value(node : TypedNode)
  nil
end

# sizeof_args

def self.node_sizeof_args(node : SizeofNode)
  node.args
end

def self.node_sizeof_args(node : TypedNode)
  nil
end

# spawn_body

def self.node_spawn_body(node : SpawnNode)
  node.body
end

def self.node_spawn_body(node : TypedNode)
  nil
end

# spawn_expression

def self.node_spawn_expression(node : SpawnNode)
  node.expression
end

def self.node_spawn_expression(node : TypedNode)
  nil
end

# super_args

def self.node_super_args(node : SuperNode)
  node.args
end

def self.node_super_args(node : TypedNode)
  nil
end

# ternary_condition

def self.node_ternary_condition(node : TernaryNode)
  node.condition
end

def self.node_ternary_condition(node : TypedNode)
  nil
end

# ternary_false_branch

def self.node_ternary_false_branch(node : TernaryNode)
  node.false_branch
end

def self.node_ternary_false_branch(node : TypedNode)
  nil
end

# ternary_true_branch

def self.node_ternary_true_branch(node : TernaryNode)
  node.true_branch
end

def self.node_ternary_true_branch(node : TypedNode)
  nil
end

# type_decl_var (for TypeDeclarationNode only)

def self.node_type_decl_var(node : TypeDeclarationNode)
  node.var
end

def self.node_type_decl_var(node : TypedNode)
  ExprId::INVALID
end

# type_decl_declared_type (for TypeDeclarationNode only)

def self.node_type_decl_declared_type(node : TypeDeclarationNode)
  node.declared_type
end

def self.node_type_decl_declared_type(node : TypedNode)
  ExprId::INVALID
end

# type_decl_name (legacy for InstanceVarDeclNode etc)

def self.node_type_decl_name(node : TypeDeclarationNode)
  node.name
end

def self.node_type_decl_name(node : TypedNode)
  nil
end

# type_decl_type (legacy for InstanceVarDeclNode etc)

def self.node_type_decl_type(node : TypeDeclarationNode)
  node.declared_type
end

def self.node_type_decl_type(node : InstanceVarDeclNode)
  node.type
end

def self.node_type_decl_type(node : TypedNode)
  nil
end

# type_decl_value (Phase 103)

def self.node_type_decl_value(node : TypeDeclarationNode)
  node.value
end

def self.node_type_decl_value(node : InstanceVarDeclNode)
  node.value
end

def self.node_type_decl_value(node : TypedNode)
  nil
end

# typeof_args

def self.node_typeof_args(node : TypeofNode)
  node.args
end

def self.node_typeof_args(node : TypedNode)
  nil
end

# uninitialized_type

def self.node_uninitialized_type(node : UninitializedNode)
  node.type
end

def self.node_uninitialized_type(node : TypedNode)
  nil
end

# while_body

def self.node_while_body(node : WhileNode)
  node.body
end

def self.node_while_body(node : UntilNode)
  node.body
end

def self.node_while_body(node : TypedNode)
  nil
end

# with_body

def self.node_with_body(node : WithNode)
  node.body
end

def self.node_with_body(node : TypedNode)
  nil
end

# with_receiver

def self.node_with_receiver(node : WithNode)
  node.receiver
end

def self.node_with_receiver(node : TypedNode)
  nil
end

# ============================================================================
# PHASE C WEEK 3: Added 8 missing helpers for failing parser tests
# ============================================================================

# range_begin (RangeNode.begin_expr vs ExpressionNode.range_begin)

def self.node_range_begin(node : RangeNode) : ExprId?
  node.begin_expr  # Different field name!
end

def self.node_range_begin(node : TypedNode) : ExprId?
  nil
end

# range_end (RangeNode.end_expr vs ExpressionNode.range_end)

def self.node_range_end(node : RangeNode) : ExprId?
  node.end_expr  # Different field name!
end

def self.node_range_end(node : TypedNode) : ExprId?
  nil
end

# range_exclusive (RangeNode.exclusive vs ExpressionNode.range_exclusive)

def self.node_range_exclusive(node : RangeNode) : Bool?
  node.exclusive  # Different field name!
end

def self.node_range_exclusive(node : TypedNode) : Bool?
  nil
end

# macro_name (MacroDefNode.name vs ExpressionNode.macro_name)

def self.node_macro_name(node : MacroDefNode) : Slice(UInt8)?
  node.name  # Different field name!
end

def self.node_macro_name(node : TypedNode) : Slice(UInt8)?
  nil
end

# macro_pieces (only exists in ExpressionNode, MacroLiteral uses ExpressionNode not typed node)

def self.node_macro_pieces(node : MacroLiteralNode) : Array(MacroPiece)?
  node.pieces
end

def self.node_macro_pieces(node : TypedNode) : Array(MacroPiece)?
  nil  # No typed node has this field
end

# args

def self.node_args(node : CallNode) : Array(ExprId)?
  node.args
end

def self.node_args(node : IndexNode) : Array(ExprId)
  node.indexes
end

def self.node_args(node : TypedNode) : Array(ExprId)?
  nil
end

# array_of_type

def self.node_array_of_type(node : ArrayLiteralNode) : ExprId?
  node.of_type
end

def self.node_array_of_type(node : TypedNode) : ExprId?
  nil
end

# callee

def self.node_callee(node : CallNode) : ExprId?
  node.callee
end

def self.node_callee(node : TypedNode) : ExprId?
  nil
end

# extend_name

def self.node_extend_name(node : ExtendNode) : Slice(UInt8)?
  node.name
end

def self.node_extend_name(node : TypedNode) : Slice(UInt8)?
  nil
end

def self.node_extend_target(node : ExtendNode) : ExprId
  node.target
end

def self.node_extend_target(node : TypedNode) : ExprId
  ExprId.new(-1)
end

# include_name

def self.node_include_name(node : IncludeNode) : Slice(UInt8)?
  node.name
end

def self.node_include_name(node : TypedNode) : Slice(UInt8)?
  nil
end

def self.node_include_target(node : IncludeNode) : ExprId
  node.target
end

def self.node_include_target(node : TypedNode) : ExprId
  ExprId.new(-1)
end

# operator

def self.node_operator(node : BinaryNode) : Slice(UInt8)?
  node.operator
end

def self.node_operator(node : UnaryNode) : Slice(UInt8)?
  node.operator
end

def self.node_operator(node : TypedNode) : Slice(UInt8)?
  nil
end

# trim_left

def self.node_trim_left(node : MacroLiteralNode) : Bool
  node.trim_left
end

def self.node_trim_left(node : TypedNode) : Bool?
  nil
end

# trim_right

def self.node_trim_right(node : MacroLiteralNode) : Bool
  node.trim_right
end

def self.node_trim_right(node : TypedNode) : Bool?
  nil
end

# ============================================================================
# Total: 75 fields × ~3 overloads = 225 methods (includes 67 original + 8 new)
      # ============================================================================

      class AstArena
        getter nodes : Array(TypedNode)

        def initialize(capacity : Int32 = 0)
          @nodes = capacity > 0 ? Array(TypedNode).new(capacity) : [] of TypedNode
        end

        @[AlwaysInline]
        def add(node : TypedNode) : ExprId
          id = ExprId.new(@nodes.size)
          @nodes << node
          id
        end

        # Compatibility shim while callers migrate off add_typed
        @[AlwaysInline]
        def add_typed(node : TypedNode) : ExprId
          add(node)
        end

        # Phase 103: Inline for performance (compiler will optimize bounds check)
        @[AlwaysInline]
        def [](id : ExprId) : TypedNode
          @nodes[id.index]
        end

        # Compatibility helpers while callers migrate off legacy helper signatures
        @[AlwaysInline]
        def typed?(id : ExprId) : Bool
          true
        end

        @[AlwaysInline]
        def get_typed(id : ExprId) : TypedNode
          @nodes[id.index]
        end

        def size
          @nodes.size
        end
      end

      # VirtualArena: Multi-file arena with offset mapping
      #
      # For LSP server: maintains per-file arenas while providing
      # unified global addressing through offset calculation.
      #
      # Benefits:
      # - Zero-copy: keeps original per-file arenas
      # - Incremental: can replace single file's arena
      # - Traceable: maps global ExprId back to source file
      # - Fast: O(log N) lookup via binary search
      # - Mutable: supports adding generated nodes (accessor expansion, etc)
      class VirtualArena
        getter file_arenas : Array(AstArena)
        getter file_paths : Array(String)  # arena index → file path
        @offsets : Array(Int32)  # offsets[i] = global start for arena[i]
        @generated_arena : AstArena  # For newly created nodes (macro expansion, etc)

        def initialize
          @file_arenas = [] of AstArena
          @file_paths = [] of String
          @offsets = [0]
          @generated_arena = AstArena.new
        end

        # Add a file's arena (for LSP: track which file)
        def add_file_arena(path : String, arena : AstArena)
          @file_arenas << arena
          @file_paths << path
          @offsets << (@offsets.last + arena.size)
        end

        # Add node to generated arena (for macro expansion, accessor generation, etc)
        def add(node : TypedNode) : ExprId
          local_id = @generated_arena.add(node)
          # Offset by all file arenas
          ExprId.new(local_id.index + @offsets.last)
        end

        # Compatibility shim
        def add_typed(node : TypedNode) : ExprId
          add(node)
        end

        # Access node by global ExprId (O(log N) where N = number of files)
        def [](id : ExprId) : TypedNode
          # Check if in generated arena first (most recent additions)
          if id.index >= @offsets.last
            local_idx = id.index - @offsets.last
            return @generated_arena[ExprId.new(local_idx)]
          end

          # Otherwise search in file arenas
          arena_idx, local_idx = decompose_id(id.index)
          @file_arenas[arena_idx][ExprId.new(local_idx)]
        end

        # For LSP: find which file contains this global ID
        def file_for_id(id : ExprId) : String?
          arena_idx, _ = decompose_id(id.index)
          @file_paths[arena_idx]?
        end

        # For LSP: get arena for specific file
        def arena_for_file(path : String) : AstArena?
          idx = @file_paths.index(path)
          idx ? @file_arenas[idx] : nil
        end

        # For LSP: replace file's arena (incremental recompilation)
        def replace_file_arena(path : String, new_arena : AstArena)
          idx = @file_paths.index(path)
          return unless idx

          @file_arenas[idx] = new_arena
          recalculate_offsets
        end

        def size
          @offsets.last + @generated_arena.size
        end

        # Compatibility helpers
        def typed?(id : ExprId) : Bool
          true
        end

        def get_typed(id : ExprId) : TypedNode
          self[id]
        end

        private def decompose_id(global_id : Int32) : {Int32, Int32}
          # Binary search to find which arena contains this ID
          left = 0
          right = @file_arenas.size - 1

          while left <= right
            mid = (left + right) // 2

            if global_id < @offsets[mid]
              right = mid - 1
            elsif mid + 1 < @offsets.size && global_id >= @offsets[mid + 1]
              left = mid + 1
            else
              # Found: arena[mid] contains this ID
              local_id = global_id - @offsets[mid]
              return {mid, local_id}
            end
          end

          # Fallback: last arena
          last_idx = @file_arenas.size - 1
          local_id = global_id - @offsets[last_idx]
          {last_idx, local_id}
        end

        private def recalculate_offsets
          @offsets = [0]
          offset = 0
          @file_arenas.each do |arena|
            offset += arena.size
            @offsets << offset
          end
        end
      end

      # Arena type that can be either single-file or multi-file
      # PageArena: Page-backed arena to reduce GC reallocations
      class PageArena
        PAGE = 1024
        @pages : Array(StaticArray(TypedNode, PAGE))
        @count : Int32

        def initialize
          @pages = [] of StaticArray(TypedNode, PAGE)
          @count = 0
        end

        @[AlwaysInline]
        def add(node : TypedNode) : ExprId
          idx = @count
          page_index = idx // PAGE
          offset = idx % PAGE
          if page_index >= @pages.size
            page = uninitialized StaticArray(TypedNode, PAGE)
            @pages << page
          end
          @pages[page_index][offset] = node
          @count = idx + 1
          ExprId.new(idx)
        end

        # Compatibility shim
        @[AlwaysInline]
        def add_typed(node : TypedNode) : ExprId
          add(node)
        end

        @[AlwaysInline]
        def [](id : ExprId) : TypedNode
          idx = id.index
          page_index = idx // PAGE
          offset = idx % PAGE
          @pages[page_index][offset]
        end

        def size
          @count
        end

        @[AlwaysInline]
        def typed?(id : ExprId) : Bool
          true
        end

        @[AlwaysInline]
        def get_typed(id : ExprId) : TypedNode
          self[id]
        end
      end

      alias ArenaLike = AstArena | VirtualArena | PageArena

      struct Program
        getter arena : ArenaLike
        getter roots : Array(ExprId)

        def initialize(@arena : ArenaLike, @roots : Array(ExprId))
        end
      end
    end
  end
end

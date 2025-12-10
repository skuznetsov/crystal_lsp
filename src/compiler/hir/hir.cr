# High-Level Intermediate Representation (HIR)
#
# HIR preserves Crystal semantics and scope structure, making it ideal for:
# - Escape analysis (what values leave their defining scope?)
# - Taint propagation (thread-shared, FFI-exposed, cyclic?)
# - Lifetime assignment (StackLocal, HeapEscape, etc.)
#
# See docs/codegen_architecture.md for full specification.

module Crystal::HIR
  # ═══════════════════════════════════════════════════════════════════════════
  # IDENTIFIERS
  # ═══════════════════════════════════════════════════════════════════════════

  # Unique ID for each value/instruction
  alias ValueId = UInt32

  # Unique ID for each basic block
  alias BlockId = UInt32

  # Unique ID for each scope region
  alias ScopeId = UInt32

  # Unique ID for each function
  alias FunctionId = UInt32

  # Unique ID for interned types
  alias TypeId = UInt32

  # Unique ID for interned strings
  alias StringId = UInt32

  # ═══════════════════════════════════════════════════════════════════════════
  # LIFETIME & TAINTS
  # ═══════════════════════════════════════════════════════════════════════════

  # Lifetime classification from escape analysis
  enum LifetimeTag : UInt8
    Unknown      = 0  # Not yet analyzed
    StackLocal   = 1  # Does not escape, can be stack-allocated
    ArgEscape    = 2  # Escapes via argument (container.add)
    HeapEscape   = 3  # Escapes to heap (return, closure capture)
    GlobalEscape = 4  # Escapes to global/class variable

    # Comparison: higher value = more escaped
    def escapes_more_than?(other : LifetimeTag) : Bool
      self.value > other.value
    end

    def merge(other : LifetimeTag) : LifetimeTag
      self.value >= other.value ? self : other
    end
  end

  # Taint flags for special characteristics
  @[Flags]
  enum Taint : UInt8
    None         = 0
    ThreadShared = 1  # May be accessed from another fiber/thread
    FFIExposed   = 2  # Passed to C code via lib fun
    Cyclic       = 4  # Type can form reference cycles
    Mutable      = 8  # Value is mutated after creation
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # TYPE REFERENCE
  # ═══════════════════════════════════════════════════════════════════════════

  # Reference to a type in the type table
  # We use our own type representation to avoid coupling to Semantic::Type
  struct TypeRef
    getter id : TypeId

    def initialize(@id : TypeId)
    end

    # Special type IDs for primitives
    VOID    = new(0_u32)
    BOOL    = new(1_u32)
    INT8    = new(2_u32)
    INT16   = new(3_u32)
    INT32   = new(4_u32)
    INT64   = new(5_u32)
    INT128  = new(6_u32)
    UINT8   = new(7_u32)
    UINT16  = new(8_u32)
    UINT32  = new(9_u32)
    UINT64  = new(10_u32)
    UINT128 = new(11_u32)
    FLOAT32 = new(12_u32)
    FLOAT64 = new(13_u32)
    CHAR    = new(14_u32)
    STRING  = new(15_u32)
    NIL     = new(16_u32)
    SYMBOL  = new(17_u32)

    FIRST_USER_TYPE = 32_u32

    def primitive? : Bool
      @id < FIRST_USER_TYPE
    end

    def ==(other : TypeRef) : Bool
      @id == other.id
    end

    def hash(hasher)
      hasher = @id.hash(hasher)
      hasher
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # VALUES (Instructions that produce a result)
  # ═══════════════════════════════════════════════════════════════════════════

  # Base class for all HIR values/instructions
  abstract class Value
    getter id : ValueId
    getter type : TypeRef
    property lifetime : LifetimeTag = LifetimeTag::Unknown
    property taints : Taint = Taint::None

    def initialize(@id : ValueId, @type : TypeRef)
    end

    # For debug printing
    abstract def to_s(io : IO) : Nil
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Constants
  # ─────────────────────────────────────────────────────────────────────────────

  # Compile-time constant value
  class Literal < Value
    getter value : LiteralValue

    def initialize(id : ValueId, type : TypeRef, @value : LiteralValue)
      super(id, type)
      # Literals don't escape and aren't mutable
      @lifetime = LifetimeTag::StackLocal
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = literal "
      case v = @value
      when Int64   then io << v
      when Float64 then io << v
      when String
        # Check if this is a symbol (type = SYMBOL)
        if @type == TypeRef::SYMBOL
          io << ":" << v
        else
          io << v.inspect
        end
      when Bool    then io << v
      when Char    then io << "'" << v << "'"
      when Nil     then io << "nil"
      else              io << v.inspect
      end
      # Use actual type from TypeRef
      io << " : " << type_name
    end

    private def type_name : String
      case @type
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
      else                       "Type(#{@type.id})"
      end
    end
  end

  alias LiteralValue = Int64 | UInt64 | Float64 | String | Bool | Char | Nil

  # ─────────────────────────────────────────────────────────────────────────────
  # Variables
  # ─────────────────────────────────────────────────────────────────────────────

  # Local variable reference
  class Local < Value
    getter name : String
    getter scope : ScopeId
    getter mutable : Bool

    def initialize(id : ValueId, type : TypeRef, @name : String, @scope : ScopeId, @mutable : Bool = true)
      super(id, type)
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = local \"" << @name << "\" : " << @type.id
      io << " (scope." << @scope << ")"
      io << " [mut]" if @mutable
    end
  end

  # Function parameter
  class Parameter < Value
    getter index : Int32
    getter name : String

    def initialize(id : ValueId, type : TypeRef, @index : Int32, @name : String)
      super(id, type)
      # Parameters come from outside, conservative lifetime
      @lifetime = LifetimeTag::HeapEscape
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = param " << @index << " \"" << @name << "\" : " << @type.id
    end
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Memory Operations
  # ─────────────────────────────────────────────────────────────────────────────

  # Heap allocation (new object)
  class Allocate < Value
    getter constructor_args : Array(ValueId)

    def initialize(id : ValueId, type : TypeRef, @constructor_args : Array(ValueId) = [] of ValueId)
      super(id, type)
      # Default: may escape, will be refined by analysis
      @lifetime = LifetimeTag::Unknown
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = allocate " << @type.id
      unless @constructor_args.empty?
        io << "("
        @constructor_args.join(io, ", ") { |arg, o| o << "%" << arg }
        io << ")"
      end
    end
  end

  # Read instance variable
  class FieldGet < Value
    getter object : ValueId
    getter field_name : String

    def initialize(id : ValueId, type : TypeRef, @object : ValueId, @field_name : String)
      super(id, type)
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = field_get %" << @object << ".@" << @field_name << " : " << @type.id
    end
  end

  # Write instance variable
  class FieldSet < Value
    getter object : ValueId
    getter field_name : String
    getter value : ValueId

    def initialize(id : ValueId, type : TypeRef, @object : ValueId, @field_name : String, @value : ValueId)
      super(id, type)
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = field_set %" << @object << ".@" << @field_name << " = %" << @value
    end
  end

  # Read class variable
  class ClassVarGet < Value
    getter class_name : String
    getter var_name : String

    def initialize(id : ValueId, type : TypeRef, @class_name : String, @var_name : String)
      super(id, type)
      # Class vars are global scope
      @lifetime = LifetimeTag::GlobalEscape
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = classvar_get " << @class_name << ".@@" << @var_name << " : " << @type.id
    end
  end

  # Write class variable
  class ClassVarSet < Value
    getter class_name : String
    getter var_name : String
    getter value : ValueId

    def initialize(id : ValueId, type : TypeRef, @class_name : String, @var_name : String, @value : ValueId)
      super(id, type)
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = classvar_set " << @class_name << ".@@" << @var_name << " = %" << @value
    end
  end

  # Array/Hash indexing: obj[key]
  class IndexGet < Value
    getter object : ValueId
    getter index : ValueId

    def initialize(id : ValueId, type : TypeRef, @object : ValueId, @index : ValueId)
      super(id, type)
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = index_get %" << @object << "[%" << @index << "] : " << @type.id
    end
  end

  # Array/Hash assignment: obj[key] = value
  class IndexSet < Value
    getter object : ValueId
    getter index : ValueId
    getter value : ValueId

    def initialize(id : ValueId, type : TypeRef, @object : ValueId, @index : ValueId, @value : ValueId)
      super(id, type)
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = index_set %" << @object << "[%" << @index << "] = %" << @value
    end
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Calls
  # ─────────────────────────────────────────────────────────────────────────────

  # Method/function call
  class Call < Value
    getter receiver : ValueId?
    getter method_name : String
    getter args : Array(ValueId)
    getter block : BlockId?
    getter virtual : Bool

    def initialize(
      id : ValueId,
      type : TypeRef,
      @receiver : ValueId?,
      @method_name : String,
      @args : Array(ValueId) = [] of ValueId,
      @block : BlockId? = nil,
      @virtual : Bool = false
    )
      super(id, type)
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = call "
      if recv = @receiver
        io << "%" << recv << "."
      end
      io << @method_name << "("
      @args.join(io, ", ") { |arg, o| o << "%" << arg }
      io << ")"
      io << " : " << @type.id
      io << " [virtual]" if @virtual
      if blk = @block
        io << " with_block block." << blk
      end
    end
  end

  # Yield to block
  class Yield < Value
    getter args : Array(ValueId)

    def initialize(id : ValueId, type : TypeRef, @args : Array(ValueId) = [] of ValueId)
      super(id, type)
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = yield"
      unless @args.empty?
        io << " "
        @args.join(io, ", ") { |arg, o| o << "%" << arg }
      end
      io << " : " << @type.id
    end
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Closures
  # ─────────────────────────────────────────────────────────────────────────────

  # Captured variable in a closure
  struct CapturedVar
    getter value_id : ValueId
    getter name : String
    getter by_reference : Bool

    def initialize(@value_id : ValueId, @name : String, @by_reference : Bool = true)
    end
  end

  # Create a closure (proc/lambda)
  class MakeClosure < Value
    getter body_block : BlockId
    getter captures : Array(CapturedVar)

    def initialize(id : ValueId, type : TypeRef, @body_block : BlockId, @captures : Array(CapturedVar) = [] of CapturedVar)
      super(id, type)
      # Closures typically escape
      @lifetime = LifetimeTag::HeapEscape
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = make_closure block." << @body_block
      unless @captures.empty?
        io << ", captures=["
        @captures.join(io, ", ") do |cap, o|
          o << "%" << cap.value_id
          o << " by_ref" if cap.by_reference
        end
        io << "]"
      end
      io << " : " << @type.id
    end
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Type Operations
  # ─────────────────────────────────────────────────────────────────────────────

  # Cast / type assertion
  class Cast < Value
    getter value : ValueId
    getter target_type : TypeRef
    getter safe : Bool  # as vs as?

    def initialize(id : ValueId, type : TypeRef, @value : ValueId, @target_type : TypeRef, @safe : Bool = false)
      super(id, type)
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = cast %" << @value
      io << (@safe ? " as? " : " as ")
      io << @target_type.id
    end
  end

  # Type check (is_a?)
  class IsA < Value
    getter value : ValueId
    getter check_type : TypeRef

    def initialize(id : ValueId, @value : ValueId, @check_type : TypeRef)
      super(id, TypeRef::BOOL)
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = is_a %" << @value << ", " << @check_type.id
    end
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Arithmetic & Logic
  # ─────────────────────────────────────────────────────────────────────────────

  enum BinaryOp : UInt8
    Add
    Sub
    Mul
    Div
    Mod
    BitAnd
    BitOr
    BitXor
    Shl
    Shr
    Eq
    Ne
    Lt
    Le
    Gt
    Ge
    And
    Or
  end

  class BinaryOperation < Value
    getter op : BinaryOp
    getter left : ValueId
    getter right : ValueId

    def initialize(id : ValueId, type : TypeRef, @op : BinaryOp, @left : ValueId, @right : ValueId)
      super(id, type)
      @lifetime = LifetimeTag::StackLocal  # Primitives don't escape
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = binop " << @op << " %" << @left << ", %" << @right << " : " << @type.id
    end
  end

  enum UnaryOp : UInt8
    Neg
    Not
    BitNot
  end

  class UnaryOperation < Value
    getter op : UnaryOp
    getter operand : ValueId

    def initialize(id : ValueId, type : TypeRef, @op : UnaryOp, @operand : ValueId)
      super(id, type)
      @lifetime = LifetimeTag::StackLocal
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = unop " << @op << " %" << @operand << " : " << @type.id
    end
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Misc
  # ─────────────────────────────────────────────────────────────────────────────

  # Phi node (for SSA, used minimally in HIR, more in MIR)
  class Phi < Value
    getter incoming : Array(Tuple(BlockId, ValueId))

    def initialize(id : ValueId, type : TypeRef, @incoming : Array(Tuple(BlockId, ValueId)) = [] of Tuple(BlockId, ValueId))
      super(id, type)
    end

    def add_incoming(block : BlockId, value : ValueId)
      @incoming << {block, value}
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = phi "
      @incoming.join(io, ", ") do |(blk, val), o|
        o << "[block." << blk << ": %" << val << "]"
      end
      io << " : " << @type.id
    end
  end

  # Copy/assignment
  class Copy < Value
    getter source : ValueId

    def initialize(id : ValueId, type : TypeRef, @source : ValueId)
      super(id, type)
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = copy %" << @source << " : " << @type.id
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # TERMINATORS (End a basic block)
  # ═══════════════════════════════════════════════════════════════════════════

  abstract class Terminator
    abstract def to_s(io : IO) : Nil
    abstract def successors : Array(BlockId)
  end

  # Return from function
  class Return < Terminator
    getter value : ValueId?

    def initialize(@value : ValueId? = nil)
    end

    def successors : Array(BlockId)
      [] of BlockId
    end

    def to_s(io : IO) : Nil
      io << "return"
      if v = @value
        io << " %" << v
      end
    end
  end

  # Conditional branch
  class Branch < Terminator
    getter condition : ValueId
    getter then_block : BlockId
    getter else_block : BlockId

    def initialize(@condition : ValueId, @then_block : BlockId, @else_block : BlockId)
    end

    def successors : Array(BlockId)
      [@then_block, @else_block]
    end

    def to_s(io : IO) : Nil
      io << "branch %" << @condition << ", then block." << @then_block << ", else block." << @else_block
    end
  end

  # Unconditional jump
  class Jump < Terminator
    getter target : BlockId

    def initialize(@target : BlockId)
    end

    def successors : Array(BlockId)
      [@target]
    end

    def to_s(io : IO) : Nil
      io << "jump block." << @target
    end
  end

  # Multi-way branch (for case/when)
  class Switch < Terminator
    getter value : ValueId
    getter cases : Array(Tuple(ValueId, BlockId))
    getter default : BlockId

    def initialize(@value : ValueId, @cases : Array(Tuple(ValueId, BlockId)), @default : BlockId)
    end

    def successors : Array(BlockId)
      result = @cases.map(&.[1])
      result << @default
      result
    end

    def to_s(io : IO) : Nil
      io << "switch %" << @value << " ["
      @cases.join(io, ", ") do |(val, blk), o|
        o << "%" << val << " -> block." << blk
      end
      io << "], default block." << @default
    end
  end

  # Unreachable (after raise, etc.)
  class Unreachable < Terminator
    def successors : Array(BlockId)
      [] of BlockId
    end

    def to_s(io : IO) : Nil
      io << "unreachable"
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # BLOCKS & SCOPES
  # ═══════════════════════════════════════════════════════════════════════════

  # Basic block: sequence of instructions ending with terminator
  class Block
    getter id : BlockId
    getter scope : ScopeId
    getter instructions : Array(Value)
    property terminator : Terminator

    def initialize(@id : BlockId, @scope : ScopeId)
      @instructions = [] of Value
      @terminator = Unreachable.new
    end

    def add(instruction : Value) : Value
      @instructions << instruction
      instruction
    end

    def to_s(io : IO) : Nil
      io << "  block." << @id << " (scope." << @scope << "):\n"
      @instructions.each do |inst|
        io << "    "
        inst.to_s(io)
        # Add lifetime/taint annotations
        if inst.lifetime != LifetimeTag::Unknown
          io << "  ; lifetime: " << inst.lifetime
        end
        if inst.taints != Taint::None
          io << ", taints: " << inst.taints
        end
        io << "\n"
      end
      io << "    "
      @terminator.to_s(io)
      io << "\n"
    end
  end

  # Scope kind
  enum ScopeKind : UInt8
    Function = 0  # Top-level function scope
    Block    = 1  # if/while/begin block
    Loop     = 2  # while/until/loop (for break/next)
    Closure  = 3  # Proc/lambda body
    Rescue   = 4  # rescue/ensure region
  end

  # Scope region
  class Scope
    getter id : ScopeId
    getter parent : ScopeId?
    getter kind : ScopeKind
    getter locals : Array(ValueId)

    def initialize(@id : ScopeId, @kind : ScopeKind, @parent : ScopeId? = nil)
      @locals = [] of ValueId
    end

    def add_local(value_id : ValueId)
      @locals << value_id
    end

    def to_s(io : IO) : Nil
      io << "scope." << @id << " (" << @kind << ")"
      if p = @parent
        io << " parent=scope." << p
      end
      unless @locals.empty?
        io << " locals=["
        @locals.join(io, ", ") { |l, o| o << "%" << l }
        io << "]"
      end
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # FUNCTION
  # ═══════════════════════════════════════════════════════════════════════════

  class Function
    getter id : FunctionId
    getter name : String
    getter params : Array(Parameter)
    getter return_type : TypeRef
    getter scopes : Array(Scope)
    getter blocks : Array(Block)
    getter entry_block : BlockId

    # For incremental ID generation
    @next_value_id : ValueId = 0_u32
    @next_block_id : BlockId = 0_u32
    @next_scope_id : ScopeId = 0_u32

    def initialize(@id : FunctionId, @name : String, @return_type : TypeRef)
      @params = [] of Parameter
      @scopes = [] of Scope
      @blocks = [] of Block

      # Create entry block and function scope
      @entry_block = create_block(create_scope(ScopeKind::Function))
    end

    def next_value_id : ValueId
      id = @next_value_id
      @next_value_id += 1
      id
    end

    def create_scope(kind : ScopeKind, parent : ScopeId? = nil) : ScopeId
      id = @next_scope_id
      @next_scope_id += 1
      scope = Scope.new(id, kind, parent)
      @scopes << scope
      id
    end

    def create_block(scope : ScopeId) : BlockId
      id = @next_block_id
      @next_block_id += 1
      block = Block.new(id, scope)
      @blocks << block
      id
    end

    def get_block(id : BlockId) : Block
      @blocks[id]
    end

    def get_scope(id : ScopeId) : Scope
      @scopes[id]
    end

    def add_param(name : String, type : TypeRef) : Parameter
      param = Parameter.new(next_value_id, type, @params.size, name)
      @params << param
      param
    end

    def to_s(io : IO) : Nil
      io << "func @" << @name << "("
      @params.join(io, ", ") do |param, o|
        o << "%" << param.id << ": " << param.type.id
      end
      io << ") -> " << @return_type.id << " {\n"

      # Print scopes
      @scopes.each do |scope|
        io << "  "
        scope.to_s(io)
        io << "\n"
      end
      io << "\n"

      # Print blocks
      @blocks.each do |block|
        block.to_s(io)
      end

      io << "}\n"
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # MODULE
  # ═══════════════════════════════════════════════════════════════════════════

  class Module
    getter name : String
    getter functions : Array(Function)
    getter types : Array(TypeDescriptor)
    getter strings : Array(String)

    @next_function_id : FunctionId = 0_u32
    @next_type_id : TypeId = TypeRef::FIRST_USER_TYPE
    @string_intern : Hash(String, StringId)

    def initialize(@name : String = "main")
      @functions = [] of Function
      @types = [] of TypeDescriptor
      @strings = [] of String
      @string_intern = {} of String => StringId
    end

    def create_function(name : String, return_type : TypeRef) : Function
      id = @next_function_id
      @next_function_id += 1
      func = Function.new(id, name, return_type)
      @functions << func
      func
    end

    def intern_string(str : String) : StringId
      @string_intern[str] ||= begin
        id = @strings.size.to_u32
        @strings << str
        id
      end
    end

    def intern_type(desc : TypeDescriptor) : TypeRef
      # Check if already interned
      @types.each_with_index do |existing, idx|
        if existing == desc
          return TypeRef.new(TypeRef::FIRST_USER_TYPE + idx.to_u32)
        end
      end
      # Add new type
      id = @next_type_id
      @next_type_id += 1
      @types << desc
      TypeRef.new(id)
    end

    def to_s(io : IO) : Nil
      io << "module " << @name << "\n\n"

      # Print type table
      unless @types.empty?
        io << "types:\n"
        @types.each_with_index do |type, idx|
          io << "  type." << (TypeRef::FIRST_USER_TYPE + idx) << " = "
          type.to_s(io)
          io << "\n"
        end
        io << "\n"
      end

      # Print functions
      @functions.each do |func|
        func.to_s(io)
        io << "\n"
      end
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # TYPE DESCRIPTORS (for type table)
  # ═══════════════════════════════════════════════════════════════════════════

  enum TypeKind : UInt8
    Primitive
    Class
    Struct
    Module
    Union
    Tuple
    NamedTuple
    Proc
    Array
    Hash
    Pointer
    Generic
  end

  class TypeDescriptor
    getter kind : TypeKind
    getter name : String
    getter type_params : Array(TypeRef)

    def initialize(@kind : TypeKind, @name : String, @type_params : Array(TypeRef) = [] of TypeRef)
    end

    def ==(other : TypeDescriptor) : Bool
      @kind == other.kind && @name == other.name && @type_params == other.type_params
    end

    def to_s(io : IO) : Nil
      io << @kind << " " << @name
      unless @type_params.empty?
        io << "("
        @type_params.join(io, ", ") { |t, o| o << t.id }
        io << ")"
      end
    end
  end
end

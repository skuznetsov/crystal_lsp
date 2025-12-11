# Mid-Level IR (MIR) for Crystal v2
#
# SSA form with explicit memory operations, ready for LLVM lowering.
# Key differences from HIR:
#   - SSA: each value assigned exactly once
#   - Explicit control flow: basic blocks, phi nodes
#   - Explicit memory: alloc, free, rc_inc, rc_dec
#   - Closures lowered to struct + function pointer
#
# See docs/codegen_architecture.md Section 4 for specification.

module Crystal::MIR
  # ═══════════════════════════════════════════════════════════════════════════
  # TYPE ALIASES
  # ═══════════════════════════════════════════════════════════════════════════

  alias ValueId = UInt32
  alias BlockId = UInt32
  alias FunctionId = UInt32
  alias TypeId = UInt32

  # ═══════════════════════════════════════════════════════════════════════════
  # MEMORY STRATEGY (assigned during HIR → MIR lowering)
  # ═══════════════════════════════════════════════════════════════════════════

  enum MemoryStrategy
    Stack       # LLVM alloca, automatic cleanup
    Slab        # Fiber-local arena, bump allocation
    ARC         # Reference counting (non-atomic)
    AtomicARC   # Reference counting (atomic, thread-safe)
    GC          # Garbage collected (Boehm GC)

    def to_s : String
      case self
      when Stack     then "stack"
      when Slab      then "slab"
      when ARC       then "arc"
      when AtomicARC then "atomic_arc"
      else                "gc"  # GC
      end
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # TYPE KIND - What kind of type this is
  # ═══════════════════════════════════════════════════════════════════════════

  enum TypeKind
    Void
    Bool
    Int8
    Int16
    Int32
    Int64
    Int128
    UInt8
    UInt16
    UInt32
    UInt64
    UInt128
    Float32
    Float64
    Char
    Symbol
    Pointer
    Reference
    Struct
    Union
    Proc
    Tuple
    Array

    def primitive?
      case self
      when Void, Bool, Int8, Int16, Int32, Int64, Int128,
           UInt8, UInt16, UInt32, UInt64, UInt128,
           Float32, Float64, Char, Symbol
        true
      else
        false
      end
    end

    def integer?
      case self
      when Int8, Int16, Int32, Int64, Int128,
           UInt8, UInt16, UInt32, UInt64, UInt128
        true
      else
        false
      end
    end

    def signed_integer?
      case self
      when Int8, Int16, Int32, Int64, Int128 then true
      else                                        false
      end
    end

    def floating?
      self == Float32 || self == Float64
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # FIELD - Struct/class field definition
  # ═══════════════════════════════════════════════════════════════════════════

  struct Field
    getter name : String
    getter type_ref : TypeRef
    getter offset : UInt32
    getter flags : UInt32

    FLAG_NILABLE  = 0x0001_u32
    FLAG_CAPTURED = 0x0002_u32  # Captured in closure

    def initialize(@name, @type_ref, @offset, @flags = 0_u32)
    end

    def nilable? : Bool
      (@flags & FLAG_NILABLE) != 0
    end

    def captured? : Bool
      (@flags & FLAG_CAPTURED) != 0
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # TYPE - Full type definition with metadata
  # ═══════════════════════════════════════════════════════════════════════════

  class Type
    getter id : TypeId
    getter kind : TypeKind
    getter name : String
    getter size : UInt64
    getter alignment : UInt32
    getter fields : Array(Field)?
    getter variants : Array(Type)?     # For union types
    getter element_types : Array(Type)? # For tuples
    getter element_type : Type?         # For arrays/pointers
    getter parent_type_id : TypeId?
    property is_closure : Bool = false

    def initialize(@id, @kind, @name, @size, @alignment)
    end

    def is_value_type? : Bool
      @kind == TypeKind::Struct
    end

    def signed? : Bool
      @kind.signed_integer?
    end

    def add_field(name : String, type_ref : TypeRef, offset : UInt32, flags : UInt32 = 0_u32)
      @fields ||= [] of Field
      @fields.not_nil! << Field.new(name, type_ref, offset, flags)
    end

    def add_variant(variant : Type)
      @variants ||= [] of Type
      @variants.not_nil! << variant
    end

    def add_element_type(element : Type)
      @element_types ||= [] of Type
      @element_types.not_nil! << element
    end

    def set_element_type(@element_type : Type)
    end

    def set_parent_type_id(@parent_type_id : TypeId)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # TYPE REGISTRY - Stores all type definitions
  # ═══════════════════════════════════════════════════════════════════════════

  class TypeRegistry
    getter types : Array(Type)
    @type_map : Hash(TypeId, Type)
    @name_map : Hash(String, Type)
    @next_type_id : TypeId

    def initialize
      @types = [] of Type
      @type_map = {} of TypeId => Type
      @name_map = {} of String => Type
      @next_type_id = 100_u32  # Reserve 0-99 for primitive types

      # Register primitive types
      register_primitive(TypeRef::VOID, TypeKind::Void, "Void", 0, 1)
      register_primitive(TypeRef::NIL, TypeKind::Void, "Nil", 0, 1)
      register_primitive(TypeRef::BOOL, TypeKind::Bool, "Bool", 1, 1)
      register_primitive(TypeRef::INT8, TypeKind::Int8, "Int8", 1, 1)
      register_primitive(TypeRef::INT16, TypeKind::Int16, "Int16", 2, 2)
      register_primitive(TypeRef::INT32, TypeKind::Int32, "Int32", 4, 4)
      register_primitive(TypeRef::INT64, TypeKind::Int64, "Int64", 8, 8)
      register_primitive(TypeRef::INT128, TypeKind::Int128, "Int128", 16, 16)
      register_primitive(TypeRef::UINT8, TypeKind::UInt8, "UInt8", 1, 1)
      register_primitive(TypeRef::UINT16, TypeKind::UInt16, "UInt16", 2, 2)
      register_primitive(TypeRef::UINT32, TypeKind::UInt32, "UInt32", 4, 4)
      register_primitive(TypeRef::UINT64, TypeKind::UInt64, "UInt64", 8, 8)
      register_primitive(TypeRef::UINT128, TypeKind::UInt128, "UInt128", 16, 16)
      register_primitive(TypeRef::FLOAT32, TypeKind::Float32, "Float32", 4, 4)
      register_primitive(TypeRef::FLOAT64, TypeKind::Float64, "Float64", 8, 8)
      register_primitive(TypeRef::CHAR, TypeKind::Char, "Char", 4, 4)
      register_primitive(TypeRef::STRING, TypeKind::Reference, "String", 8, 8)
      register_primitive(TypeRef::SYMBOL, TypeKind::Symbol, "Symbol", 4, 4)
      register_primitive(TypeRef::POINTER, TypeKind::Pointer, "Pointer", 8, 8)
    end

    private def register_primitive(ref : TypeRef, kind : TypeKind, name : String, size : UInt64, alignment : UInt32)
      type = Type.new(ref.id, kind, name, size, alignment)
      @types << type
      @type_map[ref.id] = type
      @name_map[name] = type
    end

    def create_type(kind : TypeKind, name : String, size : UInt64, alignment : UInt32) : Type
      id = @next_type_id
      @next_type_id += 1
      type = Type.new(id, kind, name, size, alignment)
      @types << type
      @type_map[id] = type
      @name_map[name] = type
      type
    end

    # Create type with explicit id (for mapping HIR TypeRef to MIR Type)
    def create_type_with_id(id : TypeId, kind : TypeKind, name : String, size : UInt64, alignment : UInt32) : Type
      type = Type.new(id, kind, name, size, alignment)
      @types << type
      @type_map[id] = type
      @name_map[name] = type
      type
    end

    def get(id : TypeId) : Type?
      @type_map[id]?
    end

    def get(ref : TypeRef) : Type?
      @type_map[ref.id]?
    end

    def get_by_name(name : String) : Type?
      @name_map[name]?
    end

    # Human-readable layout snapshot for ABI sanity checks.
    # Includes size/alignment and field offsets for non-primitive types,
    # plus variant info for unions and element info for tuples/arrays.
    def layout_snapshot : String
      String.build do |io|
        @types.each do |type|
          next if type.kind.primitive?
          io << type.name << " (" << type.kind << "): size=" << type.size << " align=" << type.alignment << "\n"
          if fields = type.fields
            fields.each do |f|
              io << "  @" << f.name << " : type#" << f.type_ref.id << " @offset " << f.offset << "\n"
            end
          end
          if variants = type.variants
            variants.each do |v|
              io << "  variant " << v.name << " size=" << v.size << " align=" << v.alignment << "\n"
              if vf = v.fields
                vf.each do |f|
                  io << "    @" << f.name << " : type#" << f.type_ref.id << " @offset " << f.offset << "\n"
                end
              end
            end
          end
          if elem = type.element_type
            io << "  element " << elem.name << "\n"
          end
          if elems = type.element_types
            elems.each_with_index do |e, idx|
              io << "  element[" << idx << "] " << e.name << "\n"
            end
          end
        end
      end
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # TYPE REFERENCES (simplified for MIR - full types resolved)
  # ═══════════════════════════════════════════════════════════════════════════

  struct TypeRef
    getter id : TypeId

    def initialize(@id : TypeId)
    end

    # Primitive types (well-known IDs)
    VOID    = new(0_u32)
    NIL     = new(1_u32)
    BOOL    = new(2_u32)
    INT8    = new(3_u32)
    INT16   = new(4_u32)
    INT32   = new(5_u32)
    INT64   = new(6_u32)
    INT128  = new(7_u32)
    UINT8   = new(8_u32)
    UINT16  = new(9_u32)
    UINT32  = new(10_u32)
    UINT64  = new(11_u32)
    UINT128 = new(12_u32)
    FLOAT32 = new(13_u32)
    FLOAT64 = new(14_u32)
    CHAR    = new(15_u32)
    STRING  = new(16_u32)
    SYMBOL  = new(17_u32)
    POINTER = new(18_u32)  # Generic pointer type

    def ==(other : TypeRef) : Bool
      @id == other.id
    end

    def hash(hasher)
      hasher = @id.hash(hasher)
      hasher
    end

    def to_s(io : IO) : Nil
      case self
      when VOID    then io << "void"
      when NIL     then io << "Nil"
      when BOOL    then io << "Bool"
      when INT8    then io << "Int8"
      when INT16   then io << "Int16"
      when INT32   then io << "Int32"
      when INT64   then io << "Int64"
      when INT128  then io << "Int128"
      when UINT8   then io << "UInt8"
      when UINT16  then io << "UInt16"
      when UINT32  then io << "UInt32"
      when UINT64  then io << "UInt64"
      when UINT128 then io << "UInt128"
      when FLOAT32 then io << "Float32"
      when FLOAT64 then io << "Float64"
      when CHAR    then io << "Char"
      when STRING  then io << "String"
      when SYMBOL  then io << "Symbol"
      when POINTER then io << "Pointer"
      else              io << "Type#" << @id
      end
    end

    def inspect(io : IO) : Nil
      to_s(io)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # VALUES - SSA Values (single assignment)
  # ═══════════════════════════════════════════════════════════════════════════

  abstract class Value
    getter id : ValueId
    getter type : TypeRef

    def initialize(@id : ValueId, @type : TypeRef)
    end

    abstract def to_s(io : IO) : Nil

    def inspect(io : IO) : Nil
      to_s(io)
    end

    # All operand value IDs this instruction uses
    def operands : Array(ValueId)
      [] of ValueId
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # CONSTANTS
  # ═══════════════════════════════════════════════════════════════════════════

  # Compile-time constant
  class Constant < Value
    getter value : Int64 | UInt64 | Float64 | Bool | Nil | String

    def initialize(id : ValueId, type : TypeRef, @value)
      super(id, type)
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = const "
      case v = @value
      when String then io << v.inspect
      when Bool   then io << (v ? "true" : "false")
      when Nil    then io << "nil"
      else             io << v
      end
      io << " : " << @type
    end
  end

  # Reference to undefined value (for phi incoming from unreachable blocks)
  class Undef < Value
    def initialize(id : ValueId, type : TypeRef)
      super(id, type)
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = undef : " << @type
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # MEMORY OPERATIONS
  # ═══════════════════════════════════════════════════════════════════════════

  # Allocate memory with specific strategy
  class Alloc < Value
    getter strategy : MemoryStrategy
    getter alloc_type : TypeRef
    getter size : UInt64  # Static size in bytes (0 = compute from type)
    getter align : UInt32 # Alignment in bytes
    property no_alias : Bool = true

    def initialize(
      id : ValueId,
      type : TypeRef,
      @strategy : MemoryStrategy,
      @alloc_type : TypeRef,
      @size : UInt64 = 0_u64,
      @align : UInt32 = 8_u32
    )
      super(id, type)
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = alloc " << @strategy
      io << " " << @alloc_type
      io << ", size=" << @size if @size > 0
      io << ", align=" << @align
      io << " [noalias]" if @no_alias
      io << " : " << @type
    end
  end

  # Free memory (for Slab strategy; no-op for others typically)
  class Free < Value
    getter ptr : ValueId
    getter strategy : MemoryStrategy

    def initialize(id : ValueId, @ptr : ValueId, @strategy : MemoryStrategy)
      super(id, TypeRef::VOID)
    end

    def operands : Array(ValueId)
      [@ptr]
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = free %" << @ptr << " (" << @strategy << ")"
    end
  end

  # Increment reference count (ARC)
  class RCIncrement < Value
    getter ptr : ValueId
    getter atomic : Bool

    def initialize(id : ValueId, @ptr : ValueId, @atomic : Bool = false)
      super(id, TypeRef::VOID)
    end

    def operands : Array(ValueId)
      [@ptr]
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = rc_inc"
      io << "_atomic" if @atomic
      io << " %" << @ptr
    end
  end

  # Decrement reference count (ARC) - may trigger destructor
  class RCDecrement < Value
    getter ptr : ValueId
    getter atomic : Bool
    getter destructor : FunctionId?

    def initialize(
      id : ValueId,
      @ptr : ValueId,
      @atomic : Bool = false,
      @destructor : FunctionId? = nil
    )
      super(id, TypeRef::VOID)
    end

    def operands : Array(ValueId)
      [@ptr]
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = rc_dec"
      io << "_atomic" if @atomic
      io << " %" << @ptr
      if d = @destructor
        io << ", destructor=@" << d
      end
    end
  end

  # Load from memory
  class Load < Value
    getter ptr : ValueId
    property no_alias : Bool = false

    def initialize(id : ValueId, type : TypeRef, @ptr : ValueId)
      super(id, type)
    end

    def operands : Array(ValueId)
      [@ptr]
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = load %" << @ptr << " : " << @type
    end
  end

  # Store to memory
  class Store < Value
    getter ptr : ValueId
    getter value : ValueId

    def initialize(id : ValueId, @ptr : ValueId, @value : ValueId)
      super(id, TypeRef::VOID)
    end

    def operands : Array(ValueId)
      [@ptr, @value]
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = store %" << @ptr << ", %" << @value
    end
  end

  # Get element pointer (GEP) - compute address of field/element
  class GetElementPtr < Value
    getter base : ValueId
    getter indices : Array(UInt32)  # Field indices / array offsets
    getter base_type : TypeRef  # Type of base pointer (for struct GEP)

    def initialize(id : ValueId, type : TypeRef, @base : ValueId, @indices : Array(UInt32), @base_type : TypeRef = TypeRef::POINTER)
      super(id, type)
    end

    def operands : Array(ValueId)
      [@base]
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = gep %" << @base
      @indices.each { |idx| io << ", " << idx }
      io << " : " << @type
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # ARITHMETIC AND LOGIC
  # ═══════════════════════════════════════════════════════════════════════════

  enum BinOp
    Add
    Sub
    Mul
    Div
    Rem
    Shl
    Shr
    And
    Or
    Xor
    Eq
    Ne
    Lt
    Le
    Gt
    Ge

    def to_s : String
      case self
      when Add then "add"
      when Sub then "sub"
      when Mul then "mul"
      when Div then "div"
      when Rem then "rem"
      when Shl then "shl"
      when Shr then "shr"
      when And then "and"
      when Or  then "or"
      when Xor then "xor"
      when Eq  then "eq"
      when Ne  then "ne"
      when Lt  then "lt"
      when Le  then "le"
      when Gt  then "gt"
      else          "ge"  # Ge
      end
    end
  end

  class BinaryOp < Value
    getter op : BinOp
    getter left : ValueId
    getter right : ValueId

    def initialize(id : ValueId, type : TypeRef, @op : BinOp, @left : ValueId, @right : ValueId)
      super(id, type)
    end

    def operands : Array(ValueId)
      [@left, @right]
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = " << @op << " %" << @left << ", %" << @right << " : " << @type
    end
  end

  enum UnOp
    Neg
    Not
    BitNot

    def to_s : String
      case self
      when Neg then "neg"
      when Not then "not"
      else          "bitnot"  # BitNot
      end
    end
  end

  class UnaryOp < Value
    getter op : UnOp
    getter operand : ValueId

    def initialize(id : ValueId, type : TypeRef, @op : UnOp, @operand : ValueId)
      super(id, type)
    end

    def operands : Array(ValueId)
      [@operand]
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = " << @op << " %" << @operand << " : " << @type
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # CONVERSIONS
  # ═══════════════════════════════════════════════════════════════════════════

  enum CastKind
    Bitcast     # Same size, different type interpretation
    Trunc       # Integer truncation
    ZExt        # Zero extension
    SExt        # Sign extension
    FPToSI      # Float to signed int
    FPToUI      # Float to unsigned int
    SIToFP      # Signed int to float
    UIToFP      # Unsigned int to float
    FPTrunc     # Float truncation (double → float)
    FPExt       # Float extension (float → double)
    PtrToInt    # Pointer to integer
    IntToPtr    # Integer to pointer

    def to_s : String
      case self
      when Bitcast  then "bitcast"
      when Trunc    then "trunc"
      when ZExt     then "zext"
      when SExt     then "sext"
      when FPToSI   then "fptosi"
      when FPToUI   then "fptoui"
      when SIToFP   then "sitofp"
      when UIToFP   then "uitofp"
      when FPTrunc  then "fptrunc"
      when FPExt    then "fpext"
      when PtrToInt then "ptrtoint"
      else               "inttoptr"  # IntToPtr
      end
    end
  end

  class Cast < Value
    getter kind : CastKind
    getter value : ValueId

    def initialize(id : ValueId, type : TypeRef, @kind : CastKind, @value : ValueId)
      super(id, type)
    end

    def operands : Array(ValueId)
      [@value]
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = " << @kind << " %" << @value << " : " << @type
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # CONTROL FLOW (within block)
  # ═══════════════════════════════════════════════════════════════════════════

  # SSA Phi node - merges values from different control flow paths
  class Phi < Value
    # (BlockId, ValueId) - which value comes from which predecessor block
    getter incoming : Array(Tuple(BlockId, ValueId))

    def initialize(id : ValueId, type : TypeRef)
      super(id, type)
      @incoming = [] of Tuple(BlockId, ValueId)
    end

    def add_incoming(block : BlockId, value : ValueId)
      @incoming << {block, value}
    end

    def operands : Array(ValueId)
      @incoming.map { |(_, v)| v }
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = phi "
      @incoming.each_with_index do |(block, val), idx|
        io << ", " if idx > 0
        io << "[block." << block << ": %" << val << "]"
      end
      io << " : " << @type
    end
  end

  # Select instruction (ternary: cond ? a : b)
  class Select < Value
    getter condition : ValueId
    getter then_value : ValueId
    getter else_value : ValueId

    def initialize(
      id : ValueId,
      type : TypeRef,
      @condition : ValueId,
      @then_value : ValueId,
      @else_value : ValueId
    )
      super(id, type)
    end

    def operands : Array(ValueId)
      [@condition, @then_value, @else_value]
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = select %" << @condition
      io << ", %" << @then_value << ", %" << @else_value
      io << " : " << @type
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # DISCRIMINATED UNION OPERATIONS
  # ═══════════════════════════════════════════════════════════════════════════

  # Union variant descriptor for MIR-level union metadata
  record UnionVariantDescriptor,
    type_id : Int32,         # Discriminator value
    type_ref : TypeRef,      # Type of this variant
    full_name : String,      # Qualified type name
    size : Int32,            # Size in bytes
    alignment : Int32,       # Alignment requirement
    field_offsets : Hash(String, Int32)?  # For struct variants

  # Full union type descriptor
  record UnionDescriptor,
    name : String,                           # e.g., "Int32 | String | Nil"
    variants : Array(UnionVariantDescriptor),
    total_size : Int32,
    alignment : Int32,
    source_file : String? = nil,
    source_line : Int32? = nil do

    def header_size : Int32
      4  # i32 type_id discriminator
    end

    def payload_offset : Int32
      max_align = variants.map(&.alignment).max? || 8
      ((header_size + max_align - 1) // max_align) * max_align
    end

    def max_payload_size : Int32
      variants.map(&.size).max? || 0
    end
  end

  # Wrap value into union (sets discriminator + stores payload)
  class UnionWrap < Value
    getter value : ValueId          # Value to wrap
    getter variant_type_id : Int32  # Discriminator for this variant
    getter union_type : TypeRef     # Type of the resulting union

    def initialize(id : ValueId, type : TypeRef, @value : ValueId, @variant_type_id : Int32, @union_type : TypeRef)
      super(id, type)
    end

    def operands : Array(ValueId)
      [@value]
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = union_wrap %" << @value << " as variant " << @variant_type_id << " : " << @type
    end
  end

  # Unwrap value from union (extracts payload)
  class UnionUnwrap < Value
    getter union_value : ValueId    # Union to unwrap
    getter variant_type_id : Int32  # Expected discriminator
    getter safe : Bool              # true = return nil on mismatch; false = UB/trap

    def initialize(id : ValueId, type : TypeRef, @union_value : ValueId, @variant_type_id : Int32, @safe : Bool = false)
      super(id, type)
    end

    def operands : Array(ValueId)
      [@union_value]
    end

    def to_s(io : IO) : Nil
      op = @safe ? "union_unwrap_safe" : "union_unwrap"
      io << "%" << @id << " = " << op << " %" << @union_value << " as variant " << @variant_type_id << " : " << @type
    end
  end

  # Get discriminator (type_id) from union
  class UnionTypeIdGet < Value
    getter union_value : ValueId

    def initialize(id : ValueId, @union_value : ValueId)
      super(id, TypeRef::INT32)
    end

    def operands : Array(ValueId)
      [@union_value]
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = union_type_id %" << @union_value << " : i32"
    end
  end

  # Check if union is specific variant
  class UnionIs < Value
    getter union_value : ValueId
    getter variant_type_id : Int32

    def initialize(id : ValueId, @union_value : ValueId, @variant_type_id : Int32)
      super(id, TypeRef::BOOL)
    end

    def operands : Array(ValueId)
      [@union_value]
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = union_is %" << @union_value << ", " << @variant_type_id << " : i1"
    end
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Array Operations
  # ─────────────────────────────────────────────────────────────────────────────

  # Static array literal [1, 2, 3]
  # Creates stack-allocated array struct { i32 size, [N x T] data }
  class ArrayLiteral < Value
    getter element_type : TypeRef
    getter elements : Array(ValueId)

    def initialize(id : ValueId, @element_type : TypeRef, @elements : Array(ValueId))
      super(id, TypeRef::VOID)  # Returns ptr to array struct
    end

    def size : Int32
      @elements.size
    end

    def operands : Array(ValueId)
      @elements
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = array_literal ["
      @elements.join(io, ", ") { |e, o| o << "%" << e }
      io << "] : " << @element_type.id
    end
  end

  # Get array size
  class ArraySize < Value
    getter array_value : ValueId

    def initialize(id : ValueId, @array_value : ValueId)
      super(id, TypeRef::INT32)
    end

    def operands : Array(ValueId)
      [@array_value]
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = array_size %" << @array_value << " : i32"
    end
  end

  # Get array element by index
  class ArrayGet < Value
    getter array_value : ValueId
    getter index_value : ValueId
    getter element_type : TypeRef

    def initialize(id : ValueId, @element_type : TypeRef, @array_value : ValueId, @index_value : ValueId)
      super(id, @element_type)
    end

    def operands : Array(ValueId)
      [@array_value, @index_value]
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = array_get %" << @array_value << "[%" << @index_value << "] : " << @element_type.id
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # GLOBAL VARIABLE ACCESS
  # ═══════════════════════════════════════════════════════════════════════════

  # Load from global variable (class var)
  class GlobalLoad < Value
    getter global_name : String

    def initialize(id : ValueId, type : TypeRef, @global_name : String)
      super(id, type)
    end

    def operands : Array(ValueId)
      [] of ValueId
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = global_load @" << @global_name << " : " << @type
    end
  end

  # Store to global variable (class var)
  class GlobalStore < Value
    getter global_name : String
    getter value : ValueId

    def initialize(id : ValueId, type : TypeRef, @global_name : String, @value : ValueId)
      super(id, type)
    end

    def operands : Array(ValueId)
      [@value]
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = global_store @" << @global_name << ", %" << @value
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # FUNCTION CALLS
  # ═══════════════════════════════════════════════════════════════════════════

  # Direct function call
  class Call < Value
    getter callee : FunctionId
    getter args : Array(ValueId)

    def initialize(id : ValueId, type : TypeRef, @callee : FunctionId, @args : Array(ValueId))
      super(id, type)
    end

    def operands : Array(ValueId)
      @args.dup
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = call @" << @callee << "("
      @args.each_with_index do |arg, idx|
        io << ", " if idx > 0
        io << "%" << arg
      end
      io << ") : " << @type
    end
  end

  # External/runtime function call by name
  class ExternCall < Value
    getter extern_name : String
    getter args : Array(ValueId)

    def initialize(id : ValueId, type : TypeRef, @extern_name : String, @args : Array(ValueId))
      super(id, type)
    end

    def operands : Array(ValueId)
      @args.dup
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = extern_call @" << @extern_name << "("
      @args.each_with_index do |arg, idx|
        io << ", " if idx > 0
        io << "%" << arg
      end
      io << ") : " << @type
    end
  end

  # Indirect call through function pointer
  class IndirectCall < Value
    getter callee_ptr : ValueId
    getter args : Array(ValueId)

    def initialize(id : ValueId, type : TypeRef, @callee_ptr : ValueId, @args : Array(ValueId))
      super(id, type)
    end

    def operands : Array(ValueId)
      [@callee_ptr] + @args
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = call_indirect %" << @callee_ptr << "("
      @args.each_with_index do |arg, idx|
        io << ", " if idx > 0
        io << "%" << arg
      end
      io << ") : " << @type
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # TERMINATORS - End a basic block
  # ═══════════════════════════════════════════════════════════════════════════

  abstract class Terminator
    abstract def to_s(io : IO) : Nil
    abstract def successors : Array(BlockId)

    def inspect(io : IO) : Nil
      to_s(io)
    end
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
      io << "ret"
      if v = @value
        io << " %" << v
      end
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
      io << "br %" << @condition
      io << ", block." << @then_block
      io << ", block." << @else_block
    end
  end

  # Multi-way branch (switch)
  class Switch < Terminator
    getter value : ValueId
    getter cases : Array(Tuple(Int64, BlockId))  # value → block
    getter default_block : BlockId

    def initialize(@value : ValueId, @cases : Array(Tuple(Int64, BlockId)), @default_block : BlockId)
    end

    def successors : Array(BlockId)
      @cases.map { |(_, b)| b } << @default_block
    end

    def to_s(io : IO) : Nil
      io << "switch %" << @value << " ["
      @cases.each_with_index do |(val, block), idx|
        io << ", " if idx > 0
        io << val << " → block." << block
      end
      io << "] default block." << @default_block
    end
  end

  # Unreachable (after noreturn calls like raise)
  class Unreachable < Terminator
    def successors : Array(BlockId)
      [] of BlockId
    end

    def to_s(io : IO) : Nil
      io << "unreachable"
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # BASIC BLOCK
  # ═══════════════════════════════════════════════════════════════════════════

  class BasicBlock
    getter id : BlockId
    getter instructions : Array(Value)
    property terminator : Terminator

    # Predecessor blocks (computed)
    property predecessors : Array(BlockId)

    def initialize(@id : BlockId)
      @instructions = [] of Value
      @terminator = Unreachable.new
      @predecessors = [] of BlockId
    end

    def add(instruction : Value)
      @instructions << instruction
    end

    # Insert phi node at beginning
    def add_phi(phi : Phi)
      # Phi nodes must come first
      phi_count = @instructions.count { |i| i.is_a?(Phi) }
      @instructions.insert(phi_count, phi)
    end

    def to_s(io : IO) : Nil
      io << "block." << @id << ":"
      if !@predecessors.empty?
        io << "  ; preds: "
        @predecessors.each_with_index do |pred, idx|
          io << ", " if idx > 0
          io << "block." << pred
        end
      end
      io << "\n"
      @instructions.each do |inst|
        io << "  "
        inst.to_s(io)
        io << "\n"
      end
      io << "  "
      @terminator.to_s(io)
      io << "\n"
    end

    def inspect(io : IO) : Nil
      to_s(io)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # FUNCTION
  # ═══════════════════════════════════════════════════════════════════════════

  struct Parameter
    getter index : UInt32
    getter name : String
    getter type : TypeRef

    def initialize(@index : UInt32, @name : String, @type : TypeRef)
    end
  end

  # Source location for debug info
  struct SourceLocation
    getter file : String
    getter line : Int32
    getter column : Int32

    def initialize(@file, @line, @column = 0)
    end
  end

  class Function
    getter id : FunctionId
    getter name : String
    getter params : Array(Parameter)
    getter return_type : TypeRef
    getter blocks : Array(BasicBlock)
    getter entry_block : BlockId
    property source_location : SourceLocation?

    @next_value_id : ValueId = 0_u32
    @next_block_id : BlockId = 0_u32
    @block_map : Hash(BlockId, BasicBlock)

    def initialize(@id : FunctionId, @name : String, @return_type : TypeRef)
      @params = [] of Parameter
      @blocks = [] of BasicBlock
      @block_map = {} of BlockId => BasicBlock

      # Create entry block
      @entry_block = create_block
    end

    def add_param(name : String, type : TypeRef) : UInt32
      idx = @params.size.to_u32
      @params << Parameter.new(idx, name, type)
      # Reserve value IDs so instruction IDs don't clash with param indices
      @next_value_id = @params.size.to_u32
      idx
    end

    def next_value_id : ValueId
      id = @next_value_id
      @next_value_id += 1
      id
    end

    def create_block : BlockId
      id = @next_block_id
      @next_block_id += 1
      block = BasicBlock.new(id)
      @blocks << block
      @block_map[id] = block
      id
    end

    def get_block(id : BlockId) : BasicBlock
      @block_map[id]
    end

    def get_block?(id : BlockId) : BasicBlock?
      @block_map[id]?
    end

    # Compute predecessor information for all blocks
    def compute_predecessors
      @blocks.each { |b| b.predecessors.clear }

      @blocks.each do |block|
        block.terminator.successors.each do |succ_id|
          if succ = @block_map[succ_id]?
            succ.predecessors << block.id unless succ.predecessors.includes?(block.id)
          end
        end
      end
    end

    def to_s(io : IO) : Nil
      io << "func @" << @name << "("
      @params.each_with_index do |param, idx|
        io << ", " if idx > 0
        io << "%" << param.index << ": " << param.type
      end
      io << ") -> " << @return_type << " {\n"

      @blocks.each do |block|
        block.to_s(io)
      end

      io << "}\n"
    end

    def inspect(io : IO) : Nil
      to_s(io)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # MODULE
  # ═══════════════════════════════════════════════════════════════════════════

  # Global variable info
  record GlobalVar, name : String, type : TypeRef, initial_value : Int64?

  class Module
    getter name : String
    getter functions : Array(Function)
    getter type_registry : TypeRegistry
    getter globals : Array(GlobalVar)
    getter union_descriptors : Hash(TypeRef, UnionDescriptor)
    property source_file : String?

    @next_function_id : FunctionId = 0_u32
    @function_map : Hash(String, Function)

    def initialize(@name : String = "main")
      @functions = [] of Function
      @function_map = {} of String => Function
      @type_registry = TypeRegistry.new
      @globals = [] of GlobalVar
      @union_descriptors = {} of TypeRef => UnionDescriptor
    end

    # Register a union type with full descriptor for debug info
    def register_union(type_ref : TypeRef, descriptor : UnionDescriptor)
      @union_descriptors[type_ref] = descriptor
    end

    # Get union descriptor by type ref
    def get_union_descriptor(type_ref : TypeRef) : UnionDescriptor?
      @union_descriptors[type_ref]?
    end

    def add_global(name : String, type : TypeRef, initial_value : Int64? = nil)
      @globals << GlobalVar.new(name, type, initial_value)
    end

    def types : Array(Type)
      @type_registry.types
    end

    def create_function(name : String, return_type : TypeRef) : Function
      id = @next_function_id
      @next_function_id += 1
      func = Function.new(id, name, return_type)
      @functions << func
      @function_map[name] = func
      func
    end

    def get_function(name : String) : Function?
      @function_map[name]?
    end

    def to_s(io : IO) : Nil
      io << "; MIR Module: " << @name << "\n\n"
      @functions.each do |func|
        func.to_s(io)
        io << "\n"
      end
    end

    def inspect(io : IO) : Nil
      to_s(io)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # BUILDER - Helper for constructing MIR
  # ═══════════════════════════════════════════════════════════════════════════

  class Builder
    getter function : Function
    property current_block : BlockId

    def initialize(@function : Function)
      @current_block = @function.entry_block
    end

    private def block : BasicBlock
      @function.get_block(@current_block)
    end

    def emit(value : Value) : ValueId
      block.add(value)
      value.id
    end

    def next_id : ValueId
      @function.next_value_id
    end

    # Constants
    def const_int(value : Int64, type : TypeRef = TypeRef::INT64) : ValueId
      emit(Constant.new(@function.next_value_id, type, value))
    end

    def const_uint(value : UInt64, type : TypeRef = TypeRef::UINT64) : ValueId
      emit(Constant.new(@function.next_value_id, type, value))
    end

    def const_float(value : Float64, type : TypeRef = TypeRef::FLOAT64) : ValueId
      emit(Constant.new(@function.next_value_id, type, value))
    end

    def const_bool(value : Bool) : ValueId
      emit(Constant.new(@function.next_value_id, TypeRef::BOOL, value))
    end

    def const_nil : ValueId
      emit(Constant.new(@function.next_value_id, TypeRef::NIL, nil))
    end

    def const_string(value : String) : ValueId
      emit(Constant.new(@function.next_value_id, TypeRef::STRING, value))
    end

    # Memory operations
    def alloc(strategy : MemoryStrategy, alloc_type : TypeRef, size : UInt64 = 0_u64, align : UInt32 = 8_u32) : ValueId
      # Result type is pointer to alloc_type
      alloc = Alloc.new(@function.next_value_id, TypeRef::POINTER, strategy, alloc_type, size, align)
      alloc.no_alias = true
      emit(alloc)
    end

    def free(ptr : ValueId, strategy : MemoryStrategy) : ValueId
      emit(Free.new(@function.next_value_id, ptr, strategy))
    end

    def rc_inc(ptr : ValueId, atomic : Bool = false) : ValueId
      emit(RCIncrement.new(@function.next_value_id, ptr, atomic))
    end

    def rc_dec(ptr : ValueId, atomic : Bool = false, destructor : FunctionId? = nil) : ValueId
      emit(RCDecrement.new(@function.next_value_id, ptr, atomic, destructor))
    end

    def load(ptr : ValueId, type : TypeRef) : ValueId
      emit(Load.new(@function.next_value_id, type, ptr))
    end

    def store(ptr : ValueId, value : ValueId) : ValueId
      emit(Store.new(@function.next_value_id, ptr, value))
    end

    def global_load(global_name : String, type : TypeRef) : ValueId
      emit(GlobalLoad.new(@function.next_value_id, type, global_name))
    end

    def global_store(global_name : String, value : ValueId, type : TypeRef) : ValueId
      emit(GlobalStore.new(@function.next_value_id, type, global_name, value))
    end

    def gep(base : ValueId, indices : Array(UInt32), result_type : TypeRef, base_type : TypeRef = TypeRef::POINTER) : ValueId
      emit(GetElementPtr.new(@function.next_value_id, result_type, base, indices, base_type))
    end

    # Arithmetic
    def add(left : ValueId, right : ValueId, type : TypeRef) : ValueId
      emit(BinaryOp.new(@function.next_value_id, type, BinOp::Add, left, right))
    end

    def sub(left : ValueId, right : ValueId, type : TypeRef) : ValueId
      emit(BinaryOp.new(@function.next_value_id, type, BinOp::Sub, left, right))
    end

    def mul(left : ValueId, right : ValueId, type : TypeRef) : ValueId
      emit(BinaryOp.new(@function.next_value_id, type, BinOp::Mul, left, right))
    end

    def div(left : ValueId, right : ValueId, type : TypeRef) : ValueId
      emit(BinaryOp.new(@function.next_value_id, type, BinOp::Div, left, right))
    end

    def rem(left : ValueId, right : ValueId, type : TypeRef) : ValueId
      emit(BinaryOp.new(@function.next_value_id, type, BinOp::Rem, left, right))
    end

    # Comparisons
    def eq(left : ValueId, right : ValueId) : ValueId
      emit(BinaryOp.new(@function.next_value_id, TypeRef::BOOL, BinOp::Eq, left, right))
    end

    def ne(left : ValueId, right : ValueId) : ValueId
      emit(BinaryOp.new(@function.next_value_id, TypeRef::BOOL, BinOp::Ne, left, right))
    end

    def lt(left : ValueId, right : ValueId) : ValueId
      emit(BinaryOp.new(@function.next_value_id, TypeRef::BOOL, BinOp::Lt, left, right))
    end

    def le(left : ValueId, right : ValueId) : ValueId
      emit(BinaryOp.new(@function.next_value_id, TypeRef::BOOL, BinOp::Le, left, right))
    end

    def gt(left : ValueId, right : ValueId) : ValueId
      emit(BinaryOp.new(@function.next_value_id, TypeRef::BOOL, BinOp::Gt, left, right))
    end

    def ge(left : ValueId, right : ValueId) : ValueId
      emit(BinaryOp.new(@function.next_value_id, TypeRef::BOOL, BinOp::Ge, left, right))
    end

    # Bitwise
    def bit_and(left : ValueId, right : ValueId, type : TypeRef) : ValueId
      emit(BinaryOp.new(@function.next_value_id, type, BinOp::And, left, right))
    end

    def bit_or(left : ValueId, right : ValueId, type : TypeRef) : ValueId
      emit(BinaryOp.new(@function.next_value_id, type, BinOp::Or, left, right))
    end

    def bit_xor(left : ValueId, right : ValueId, type : TypeRef) : ValueId
      emit(BinaryOp.new(@function.next_value_id, type, BinOp::Xor, left, right))
    end

    def shl(left : ValueId, right : ValueId, type : TypeRef) : ValueId
      emit(BinaryOp.new(@function.next_value_id, type, BinOp::Shl, left, right))
    end

    def shr(left : ValueId, right : ValueId, type : TypeRef) : ValueId
      emit(BinaryOp.new(@function.next_value_id, type, BinOp::Shr, left, right))
    end

    # Unary
    def neg(operand : ValueId, type : TypeRef) : ValueId
      emit(UnaryOp.new(@function.next_value_id, type, UnOp::Neg, operand))
    end

    def not(operand : ValueId) : ValueId
      emit(UnaryOp.new(@function.next_value_id, TypeRef::BOOL, UnOp::Not, operand))
    end

    def bit_not(operand : ValueId, type : TypeRef) : ValueId
      emit(UnaryOp.new(@function.next_value_id, type, UnOp::BitNot, operand))
    end

    # Casts
    def cast(kind : CastKind, value : ValueId, target_type : TypeRef) : ValueId
      emit(Cast.new(@function.next_value_id, target_type, kind, value))
    end

    def bitcast(value : ValueId, target_type : TypeRef) : ValueId
      cast(CastKind::Bitcast, value, target_type)
    end

    # Control flow values
    def phi(type : TypeRef) : Phi
      phi = Phi.new(@function.next_value_id, type)
      block.add_phi(phi)
      phi
    end

    def select(condition : ValueId, then_value : ValueId, else_value : ValueId, type : TypeRef) : ValueId
      emit(Select.new(@function.next_value_id, type, condition, then_value, else_value))
    end

    # Calls
    def call(callee : FunctionId, args : Array(ValueId), return_type : TypeRef) : ValueId
      emit(Call.new(@function.next_value_id, return_type, callee, args))
    end

    def call_indirect(callee_ptr : ValueId, args : Array(ValueId), return_type : TypeRef) : ValueId
      emit(IndirectCall.new(@function.next_value_id, return_type, callee_ptr, args))
    end

    def extern_call(extern_name : String, args : Array(ValueId), return_type : TypeRef) : ValueId
      emit(ExternCall.new(@function.next_value_id, return_type, extern_name, args))
    end

    # Terminators
    def ret(value : ValueId? = nil)
      block.terminator = Return.new(value)
    end

    def jump(target : BlockId)
      block.terminator = Jump.new(target)
    end

    def branch(condition : ValueId, then_block : BlockId, else_block : BlockId)
      block.terminator = Branch.new(condition, then_block, else_block)
    end

    def switch(value : ValueId, cases : Array(Tuple(Int64, BlockId)), default_block : BlockId)
      block.terminator = Switch.new(value, cases, default_block)
    end

    def unreachable
      block.terminator = Unreachable.new
    end
  end
end

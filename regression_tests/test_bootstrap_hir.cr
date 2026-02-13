# Simplified HIR node definitions from the real compiler
# EXPECT: bootstrap_hir_done
# This tests struct/class patterns, enums, and the instruction hierarchy

module CrystalV2
  module Compiler
    module HIR
      struct TypeRef
        getter id : Int32
        
        def initialize(@id : Int32)
        end

        VOID   = TypeRef.new(0)
        NIL    = TypeRef.new(1)
        BOOL   = TypeRef.new(2)
        INT8   = TypeRef.new(3)
        INT32  = TypeRef.new(5)
        INT64  = TypeRef.new(6)
        STRING = TypeRef.new(16)
        
        def ==(other : TypeRef) : Bool
          @id == other.id
        end
        
        def to_s : String
          case @id
          when 0 then "Void"
          when 1 then "Nil"
          when 2 then "Bool"
          when 5 then "Int32"
          when 6 then "Int64"
          when 16 then "String"
          else "Type(#{@id})"
          end
        end
      end

      struct ValueId
        getter index : Int32
        def initialize(@index : Int32); end
        
        def ==(other : ValueId) : Bool
          @index == other.index
        end
      end

      alias BlockId = Int32

      enum BinaryOp
        Add; Sub; Mul; Div; Mod
        Eq; Ne; Lt; Le; Gt; Ge
        And; Or; Xor; Shl; Shr
      end

      enum UnaryOp
        Neg; Not; BitNot
      end

      # Base instruction
      abstract class Instruction
        getter id : ValueId
        getter type_ref : TypeRef
        
        def initialize(@id : ValueId, @type_ref : TypeRef)
        end
      end

      class Literal < Instruction
        getter value : Int64
        
        def initialize(id : ValueId, type_ref : TypeRef, @value : Int64)
          super(id, type_ref)
        end
      end

      class StringLiteral < Instruction
        getter value : String
        
        def initialize(id : ValueId, type_ref : TypeRef, @value : String)
          super(id, type_ref)
        end
      end

      class BinaryOperation < Instruction
        getter op : BinaryOp
        getter left : ValueId
        getter right : ValueId
        
        def initialize(id : ValueId, type_ref : TypeRef, @op : BinaryOp, @left : ValueId, @right : ValueId)
          super(id, type_ref)
        end
      end

      class Call < Instruction
        getter name : String
        getter args : Array(ValueId)
        getter receiver : ValueId?
        
        def initialize(id : ValueId, type_ref : TypeRef, @receiver : ValueId?, @name : String, @args : Array(ValueId))
          super(id, type_ref)
        end
      end

      class Local < Instruction
        getter name : String
        getter scope : Int32
        getter mutable : Bool
        
        def initialize(id : ValueId, type_ref : TypeRef, @name : String, @scope : Int32, @mutable : Bool)
          super(id, type_ref)
        end
      end

      class Return < Instruction
        getter value : ValueId?
        
        def initialize(id : ValueId, @value : ValueId?)
          super(id, TypeRef::VOID)
        end
      end

      # Block
      class Block
        getter id : BlockId
        getter instructions : Array(Instruction)
        property terminator_kind : Int32  # 0=none, 1=jump, 2=branch, 3=return
        property jump_target : BlockId
        property branch_cond : ValueId?
        property branch_true : BlockId
        property branch_false : BlockId
        
        def initialize(@id : BlockId)
          @instructions = [] of Instruction
          @terminator_kind = 0
          @jump_target = 0
          @branch_cond = nil
          @branch_true = 0
          @branch_false = 0
        end
        
        def add(inst : Instruction)
          @instructions << inst
        end
      end

      # Function
      class Function
        getter name : String
        getter blocks : Array(Block)
        getter return_type : TypeRef
        @next_value : Int32
        @next_block : Int32
        
        def initialize(@name : String, @return_type : TypeRef)
          @blocks = [] of Block
          @next_value = 0
          @next_block = 0
        end
        
        def next_value_id : ValueId
          id = ValueId.new(@next_value)
          @next_value += 1
          id
        end
        
        def create_block : BlockId
          id = @next_block
          @next_block += 1
          @blocks << Block.new(id)
          id
        end
        
        def get_block(id : BlockId) : Block
          @blocks[id]
        end
      end
    end
  end
end

# Build a simple function: add(a: Int32, b: Int32) -> Int32
func = CrystalV2::Compiler::HIR::Function.new("add", CrystalV2::Compiler::HIR::TypeRef::INT32)
entry = func.create_block

# Parameters
a_id = func.next_value_id
a = CrystalV2::Compiler::HIR::Local.new(a_id, CrystalV2::Compiler::HIR::TypeRef::INT32, "a", 0, false)
func.get_block(entry).add(a)

b_id = func.next_value_id
b = CrystalV2::Compiler::HIR::Local.new(b_id, CrystalV2::Compiler::HIR::TypeRef::INT32, "b", 0, false)
func.get_block(entry).add(b)

# a + b
sum_id = func.next_value_id
sum = CrystalV2::Compiler::HIR::BinaryOperation.new(sum_id, CrystalV2::Compiler::HIR::TypeRef::INT32, CrystalV2::Compiler::HIR::BinaryOp::Add, a_id, b_id)
func.get_block(entry).add(sum)

# return sum
ret_id = func.next_value_id
ret = CrystalV2::Compiler::HIR::Return.new(ret_id, sum_id)
func.get_block(entry).add(ret)
func.get_block(entry).terminator_kind = 3  # return

puts "Function: #{func.name}"
puts "Blocks: #{func.blocks.size}"
puts "Instructions: #{func.get_block(entry).instructions.size}"

# Walk instructions using is_a? dispatch on abstract class hierarchy
count_locals = 0
count_binops = 0
count_returns = 0
count_other = 0
func.get_block(entry).instructions.each do |inst|
  if inst.is_a?(CrystalV2::Compiler::HIR::Local)
    l = inst.as(CrystalV2::Compiler::HIR::Local)
    puts "  local #{l.name}"
    count_locals += 1
  elsif inst.is_a?(CrystalV2::Compiler::HIR::BinaryOperation)
    puts "  binop"
    count_binops += 1
  elsif inst.is_a?(CrystalV2::Compiler::HIR::Return)
    puts "  return"
    count_returns += 1
  else
    count_other += 1
  end
end

puts "locals=#{count_locals} binops=#{count_binops} returns=#{count_returns}"

# Verify TypeRef struct equality
t1 = CrystalV2::Compiler::HIR::TypeRef::INT32
t2 = CrystalV2::Compiler::HIR::TypeRef::INT32
t3 = CrystalV2::Compiler::HIR::TypeRef::STRING
puts "eq=#{t1 == t2}"
puts "ne=#{t1 == t3}"

# Verify enum values
puts "add_val=#{CrystalV2::Compiler::HIR::BinaryOp::Add.value}"
puts "sub_val=#{CrystalV2::Compiler::HIR::BinaryOp::Sub.value}"

puts "bootstrap_hir_done"

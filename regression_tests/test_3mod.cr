module Named
# EXPECT: done
  abstract def name : String
end

module Sized
  abstract def byte_size : Int32
end

module Described
  def describe : String
    "#{name}(#{byte_size})"
  end
end

abstract class Type
  include Named
  include Sized
  include Described
  getter id : Int32
  def initialize(@id : Int32); end
end

class IntType < Type
  def name : String; "Int32"; end
  def byte_size : Int32; 4; end
end

class PtrType < Type
  getter target : Type
  def initialize(id : Int32, @target : Type); super(id); end
  def name : String; "Ptr(#{@target.name})"; end
  def byte_size : Int32; 8; end
end

class ArrType < Type
  getter element : Type
  def initialize(id : Int32, @element : Type); super(id); end
  def name : String; "Arr(#{@element.name})"; end
  def byte_size : Int32; 24; end
end

int_type = IntType.new(1)
ptr_type = PtrType.new(2, int_type)
arr_type = ArrType.new(3, int_type)

puts int_type.describe
puts ptr_type.describe
puts arr_type.describe
puts "done"

# Test method overloading patterns common in compiler code
# EXPECT: done
class TypeRef
  getter id : Int32
  getter name : String

  def initialize(@id : Int32, @name : String)
  end

  def ==(other : TypeRef) : Bool
    @id == other.id
  end

  def to_s : String
    @name
  end
end

# Test method with default args
def format_type(t : TypeRef, verbose : Bool = false) : String
  if verbose
    "TypeRef(#{t.id}, #{t.name})"
  else
    t.name
  end
end

t1 = TypeRef.new(1, "Int32")
t2 = TypeRef.new(2, "String")
t3 = TypeRef.new(1, "Int32")

puts t1 == t3
puts t1 == t2

puts format_type(t1)
puts format_type(t2, true)

# Test with array of custom types
types = [t1, t2, t3]
puts types.size
types.each { |t| puts t.name }

# Test case/when with custom types
def describe(t : TypeRef) : String
  case t.name
  when "Int32"
    "integer"
  when "String"
    "text"
  else
    "other"
  end
end

puts describe(t1)
puts describe(t2)

puts "done"

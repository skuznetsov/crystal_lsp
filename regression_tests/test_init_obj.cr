class Env
# EXPECT: done
  @vars : Hash(String, Int32)

  def initialize
    @vars = {} of String => Int32
  end

  def initialize(other : Env)
    @vars = {} of String => Int32
    # Copy vars from other
    other.vars.each { |k, v| @vars[k] = v }
  end

  def vars : Hash(String, Int32)
    @vars
  end

  def set(name : String, val : Int32)
    @vars[name] = val
  end

  def get(name : String) : Int32
    @vars[name]? || 0
  end
end

e = Env.new
e.set("x", 10)
puts e.get("x")

e2 = Env.new(e)
puts e2.get("x")
puts "done"

# Regression test: forall T with T=Nil must correctly return nil in union types
# Bug: String | T where T=Nil became String | Pointer, making nil values non-nil
# EXPECT: forall_nil_ok

def identity_forall(x : T) : T forall T
  x
end

def union_forall(x : T) : String | T forall T
  x
end

module TestModule
  def self.fetch(key, default : T) : String | T forall T
    fetch_impl(key) { default }
  end

  def self.fetch_impl(key : String, &block : String -> T) : String | T forall T
    if value = LibC.getenv(key)
      String.new(value)
    else
      yield key
    end
  end
end

# Test 1: identity(nil) should be nil
r1 = identity_forall(nil)
unless r1.nil?
  puts "FAIL: identity(nil).nil? = false"
  exit 1
end

# Test 2: union return with T=Nil should be nil
r2 = union_forall(nil)
unless r2.nil?
  puts "FAIL: union_forall(nil).nil? = false"
  exit 1
end

# Test 3: module method with forall T fetch pattern
r3 = TestModule.fetch("NONEXIST_FOR_TEST_XYZ", nil)
unless r3.nil?
  puts "FAIL: TestModule.fetch(nonexist, nil).nil? = false"
  exit 1
end

# Test 4: ENV.[]? should return nil for non-existent keys
r4 = ENV["NONEXIST_FOR_TEST_XYZ"]?
unless r4.nil?
  puts "FAIL: ENV[nonexist]?.nil? = false"
  exit 1
end

puts "forall_nil_ok"

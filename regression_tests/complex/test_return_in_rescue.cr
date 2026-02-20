# Test: return inside rescue block should actually return from the method
# Bug: lower_begin() unconditionally emitted Jump after rescue body,
#      overwriting the Return terminator set by lower_return()

def foo_return_rescue : Int32
  begin
    raise "test_error"
  rescue ex
    return 42
  end
  return 99
end

def bar_return_rescue_with_ensure : Int32
  result = 0
  begin
    raise "test_error"
  rescue ex
    result = 42
    return result
  ensure
    # ensure should still execute even with return
  end
  return 99
end

def baz_no_return_rescue : Int32
  begin
    raise "test_error"
  rescue ex
    42
  end
end

# Test 1: return in rescue
r1 = foo_return_rescue
raise "FAIL: return_rescue expected 42, got #{r1}" unless r1 == 42

# Test 2: rescue without return (value flows through)
r3 = baz_no_return_rescue
raise "FAIL: no_return_rescue expected 42, got #{r3}" unless r3 == 42

puts "return_in_rescue_ok"

# Test basic functionality to ensure union code didn't break anything
# Real union tests will be added when parser supports union syntax

class Result
  @value : Int32
  @error_code : Int32

  def initialize(value : Int32, error_code : Int32)
    @value = value
    @error_code = error_code
  end

  def get_value() : Int32
    @value
  end

  def is_success() : Int32
    if @error_code == 0
      1
    else
      0
    end
  end
end

def make_success(val : Int32) : Result
  Result.new(val, 0)
end

def make_error(code : Int32) : Result
  Result.new(0, code)
end

def main() : Int32
  # Test creating Result objects
  success = make_success(42)
  error = make_error(1)

  # Test success case: 1 + 42 = 43
  result = success.is_success() + success.get_value()

  # Test error case: 0 + 0 = 0, so result stays 43
  result = result + error.get_value()

  # Return 43
  result
end

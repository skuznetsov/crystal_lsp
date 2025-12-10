# Test nested case/when
# Expected: returns 42

def outer(x : Int32) : Int32
  case x
  when 1
    case x
    when 1
      42
    else
      0
    end
  else
    99
  end
end

def main() : Int32
  outer(1)  # should return 42
end

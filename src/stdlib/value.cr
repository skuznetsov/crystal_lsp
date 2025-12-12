# Value types - passed by value, stored on stack

struct Value
end

struct Nil
  def to_s() : String
    ""
  end
end

struct Bool
  def to_s() : String
    if self
      "true"
    else
      "false"
    end
  end
end

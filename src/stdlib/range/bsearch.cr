# Cast integer as floating number value with keeping binary structure.
private def int_as_float(i : Int64) : Float64
  # Integer uses two's complement to represent signed number, but
  # floating number value uses sign bit. This difference causes a
  # problem that it reproduce incorrect value when you sum positive
  # number and negative number. It fixes this problem.
  if i < 0
    i = -i
    -i.unsafe_as(Float64)
  else
    i.unsafe_as(Float64)
  end
end

private def int_as_float(i : Int32) : Float32
  if i < 0
    i = -i
    -i.unsafe_as(Float32)
  else
    i.unsafe_as(Float32)
  end
end

# Cast floating number value as integer with keeping binary structure.
private def float_as_int(f : Float64) : Int64
  if f < 0
    f = -f
    -f.unsafe_as(Int64)
  else
    f.unsafe_as(Int64)
  end
end

private def float_as_int(f : Float32) : Int32
  if f < 0
    f = -f
    -f.unsafe_as(Int32)
  else
    f.unsafe_as(Int32)
  end
end

private def bsearch_internal(from : Float64, to, exclusive, &block)
  bsearch_internal from, to.to_f64, exclusive do |value|
    yield value
  end
end

private def bsearch_internal(from, to : Float64, exclusive, &block)
  bsearch_internal from.to_f64, to, exclusive do |value|
    yield value
  end
end

private def bsearch_internal(from : Float64, to : Float64, exclusive, &block)
  from = float_as_int from
  to = float_as_int to
  to -= 1 if exclusive

  bsearch_internal(from, to, false) { |i| yield int_as_float i }
    .try { |i| int_as_float i }
end

private def bsearch_internal(from : Int64, to : Int64, exclusive, &block)
  saved_to = to
  satisfied = nil
  while from < to
    mid = (from < 0) == (to < 0) ? from + ((to - from) >> 1)
        : (from < -to) ? -(((- from - to - 1) >> 1) + 1) : ((from + to) >> 1)

    if yield mid
      satisfied = mid
      to = mid
    else
      from = mid + 1
    end
  end

  if !exclusive && from == saved_to && yield from
    satisfied = from
  end

  satisfied
end

private def bsearch_internal(from : Float32, to, exclusive, &block)
  bsearch_internal from, to.to_f32, exclusive do |value|
    yield value
  end
end

private def bsearch_internal(from, to : Float32, exclusive, &block)
  bsearch_internal from.to_f32, to, exclusive do |value|
    yield value
  end
end

private def bsearch_internal(from : Float32, to : Float32, exclusive, &block)
  from = float_as_int from
  to = float_as_int to
  to -= 1 if exclusive

  bsearch_internal(from, to, false) { |i| yield int_as_float i }
    .try { |i| int_as_float i }
end

private def bsearch_internal(from : Int32, to : Int32, exclusive, &block)
  saved_to = to
  satisfied = nil
  while from < to
    mid = (from < 0) == (to < 0) ? from + ((to - from) >> 1)
        : (from < -to) ? -(((- from - to - 1) >> 1) + 1) : ((from + to) >> 1)

    if yield mid
      satisfied = mid
      to = mid
    else
      from = mid + 1
    end
  end

  if !exclusive && from == saved_to && yield from
    satisfied = from
  end

  satisfied
end

struct Range(B, E)
  # By using binary search, returns the first element
  # for which the passed block returns a truthy value.
  #
  # If the block returns a falsey value, the element to be found lies
  # behind. If the block returns a truthy value, the element to be found
  # is itself or lies in front.
  #
  # Returns `nil` if the block didn't return a truthy value for any element.
  #
  # ```
  # (0..10).bsearch { |x| x >= 5 }                       # => 5
  # (0..Float64::INFINITY).bsearch { |x| x ** 4 >= 256 } # => 4
  # ```
  def bsearch(&block : B | E -> _)
    from = self.begin
    to = self.end

    # If the range consists of floating value,
    # it uses specialized implementation for floating value.
    # This implementation is very fast. For example,
    # `(1..1e300).bsearch{ false }` loops over 2000 times in
    # popular implementation, but in this implementation loops 65 times
    # at most.
    if from.is_a?(Float64) || to.is_a?(Float64)
      return bsearch_internal from.to_f64, to.to_f64, self.excludes_end? do |value|
        yield value
      end
    end

    if from.is_a?(Float32) || to.is_a?(Float32)
      return bsearch_internal from.to_f32, to.to_f32, self.excludes_end? do |value|
        yield value
      end
    end

    saved_to = to
    satisfied = nil
    while from < to
      mid = from + ((to - from) >> 1)

      if yield mid
        satisfied = mid
        to = mid
      else
        from = mid + 1
      end
    end

    if !self.excludes_end? && from == saved_to && yield from
      satisfied = from
    end

    satisfied
  end
end

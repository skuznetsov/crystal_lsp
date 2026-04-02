def sample(segments : Array(Int32))
  first, *rest = segments
  rest
end

sample([1, 2, 3])

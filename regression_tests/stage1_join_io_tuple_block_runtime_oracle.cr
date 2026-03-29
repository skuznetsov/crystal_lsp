private def render_pairs(io : IO, items : Array(Tuple(Int32, String))) : Nil
  items.join(io, ", ") do |(left, right), out|
    out << left
    out << ':'
    out << right
  end
end

items = [{1, "a"}, {2, "b"}, {3, "c"}]

puts String.build { |io| render_pairs(io, items) }

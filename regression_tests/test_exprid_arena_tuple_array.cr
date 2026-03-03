# EXPECT: exprid_arena_tuple_array_ok

require "../src/compiler/frontend/ast"

alias F = CrystalV2::Compiler::Frontend

arena = F::AstArena.new
items = [] of Tuple(F::ExprId, F::ArenaLike)

items << {F::ExprId.new(10), arena}
items << {F::ExprId.new(20), arena}

sum = 0
items.each do |(expr_id, _a)|
  sum += expr_id.index
end

if sum == 30
  puts "exprid_arena_tuple_array_ok"
else
  puts "exprid_arena_tuple_array_bad"
end

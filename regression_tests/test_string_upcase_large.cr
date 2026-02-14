# EXPECT: string_upcase_large_ok

source = "a" * 50_000
up = source.upcase

if up.size == 50_000 && up[0] == 'A' && up[-1] == 'A'
  puts "string_upcase_large_ok"
else
  puts "string_upcase_large_bad: #{up.size}"
end

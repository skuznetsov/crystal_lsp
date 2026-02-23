# EXPECT: FF
left = false
right = false
result = "ELSE"

case {left, right}
when {true, true}
  result = "TT"
when {false, false}
  result = "FF"
else
  result = "ELSE"
end

puts result

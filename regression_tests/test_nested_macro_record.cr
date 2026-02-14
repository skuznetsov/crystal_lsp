# EXPECT: nested_macro_record_ok

macro define_token(name)
  record {{name.id}}, value : Int32, ok : Bool
end

define_token NestedToken

token = NestedToken.new(11, true)
if token.value == 11 && token.ok
  puts "nested_macro_record_ok"
else
  puts "nested_macro_record_bad"
end

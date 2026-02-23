# Regression test: ENV.[]? must return nil for non-existent keys
# Bug: union ABI extraction + re-wrap lost nil type_id, causing
# ENV.[]? to return non-nil (empty String) for missing env vars.
# Root cause: emit_union_wrap set type_id unconditionally without
# checking for null pointers from prior union ABI extraction.
# EXPECT: env_nil_ok

key = "THIS_DOES_NOT_EXIST_XYZ_REGRESSION_987654"
v = ENV[key]?
if v.nil?
  puts "env_nil_ok"
else
  puts "FAIL: ENV[non_existent]? returned non-nil"
end

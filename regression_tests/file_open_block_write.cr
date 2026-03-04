# Direct regression for File.open(path, "w") block form.
# This path previously crashed at runtime in stage1-built binaries.
# EXPECT: file_open_block_ok
path = "/tmp/crystal_v2_file_open_block_write.txt"

File.open(path, "w") do |f|
  f.puts "ok"
end

content = File.read(path)
puts content.chomp
File.delete(path)
puts "file_open_block_ok"

# Test file I/O needed for bootstrap
# EXPECT: file_io_ok
path = "/tmp/test_output_crystal_v2.txt"
File.write(path, "hello from crystal v2\n")
content = File.read(path)
puts content.chomp
puts content.size
File.delete(path)
puts File.exists?(path)
puts "file_io_ok"

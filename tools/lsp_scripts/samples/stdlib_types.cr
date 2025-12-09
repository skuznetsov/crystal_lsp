# Tests stdlib type inference and navigation
require "json"

arr = [1, 2, 3]
str = "hello"
hash = {"a" => 1, "b" => 2}

# Array methods
mapped = arr.map { |x| x * 2 }
first = arr.first
size = arr.size

# String methods
upcase = str.upcase
chars = str.chars
split = str.split("")

# Hash methods
keys = hash.keys
values = hash.values

# Time
now = Time.utc
year = now.year

# File
exists = File.exists?("/tmp")

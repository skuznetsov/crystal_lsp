# EXPECT: Stage 1/6 parse 1.3ms user=1.3ms files=1 loaded=1

parse_details = [] of String
parse_details << "user=1.3ms"
parse_details << "files=1"
parse_details << "loaded=1"

parts = ["Stage 1/6 parse", "1.3ms"]
parts.concat(parse_details)

puts parts.join(" ")

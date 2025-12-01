require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/ast"

# Analyze actual node sizes
puts "=== AST Node Size Analysis ==="
puts

# Check type sizes
puts "Basic types:"
puts "  ExprId size:     #{sizeof(CrystalV2::Compiler::Frontend::ExprId)} bytes"
puts "  Span size:       #{sizeof(CrystalV2::Compiler::Frontend::Span)} bytes"
puts "  String size:     #{sizeof(String)} bytes (pointer)"
puts "  Array(ExprId):   #{sizeof(Array(CrystalV2::Compiler::Frontend::ExprId))} bytes (pointer)"
puts

# Check instance sizes of some concrete node classes
puts "Node class instance sizes (approximate):"
AST = CrystalV2::Compiler::Frontend

# We can estimate based on field count
puts "  IntegerLiteral: ExprId(4) + Span(8) + value(8) + negative(1) = ~21 + header"
puts "  StringLiteral:  ExprId(4) + Span(8) + String(8) = ~20 + header"
puts "  Call:           ExprId + Span + receiver + name + args + named_args + block = ~50 + header"
puts "  Def:            ExprId + Span + name + args + body + return_type + ... = ~80 + header"
puts

# Crystal object header is typically 8 bytes (type_id) + 8 bytes for vtable ref = 16 bytes
puts "Crystal object overhead: ~16 bytes per object (type_id + vtable)"
puts

# Current vs theoretical comparison
puts "=== Memory Comparison ==="
nodes = 183322
current_bytes_per_node = 362.8
current_mb = nodes * current_bytes_per_node / (1024 * 1024)

# Theoretical packed struct approach
# Each node: 4 (id) + 8 (span) + 1 (kind) + ~20 (avg data) = 33 bytes
theoretical_packed = 33
theoretical_mb = nodes * theoretical_packed / (1024 * 1024)

# Even simpler: flat union approach (fixed size per node)
flat_size = 64  # 64 bytes fixed per node, no pointers
flat_mb = nodes * flat_size / (1024 * 1024)

puts "For #{nodes} nodes:"
puts "  Current:              #{current_mb.round(1)} MB (#{current_bytes_per_node.round(1)} bytes/node)"
puts "  Packed structs:       #{theoretical_mb.round(1)} MB (#{theoretical_packed} bytes/node)"
puts "  Flat 64-byte nodes:   #{flat_mb.round(1)} MB (#{flat_size} bytes/node)"
puts
puts "Potential savings:"
puts "  vs Packed:  #{((1 - theoretical_mb / current_mb) * 100).round(1)}%"
puts "  vs Flat:    #{((1 - flat_mb / current_mb) * 100).round(1)}%"

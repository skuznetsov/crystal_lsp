require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Test specific patterns
test_cases = [
  # Hash literal
  %(private SUBSTITUTIONS = {
    '&'  => "&amp;",
  }),

  # Uninitialized with type
  %(chars = uninitialized UInt8[4]
i = 0),

  # Type cast
  %(x = (1_f32 / 0_f32).as Float32),

  # Scoped constant
  %(@state = DeliveryState::None),

  # Pointerof
  %(read(Slice.new(pointerof(byte), 1))),
]

test_cases.each_with_index do |source, idx|
  puts "\n" + "="*70
  puts "Test #{idx + 1}:"
  puts source
  puts "-"*70

  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program

  id_errors = parser.diagnostics.select { |d| d.message == "unexpected Identifier" }

  if id_errors.size > 0
    puts "✗ #{id_errors.size} 'unexpected Identifier' error(s)"
    id_errors.each do |err|
      lines = source.lines
      line = lines[err.span.start_line]? || ""
      col = err.span.start_column

      # Extract the identifier at error position
      identifier = line[col...col+20].split(/[^a-zA-Z0-9_]/).first? || ""
      puts "  Line #{err.span.start_line + 1}, col #{col + 1}: '#{identifier}'"
    end
  else
    puts "✓ No errors"
  end
end

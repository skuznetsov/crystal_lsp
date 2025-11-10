require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Find "unexpected AndAnd" errors and show context
count = 0

Dir.glob("/Users/sergey/Projects/Crystal/crystal/src/**/*.cr") do |file|
  next if count >= 10  # Show first 10
  
  source = File.read(file)
  
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program
  
  andand_errors = parser.diagnostics.select { |d| d.message.includes?("unexpected AndAnd") }
  
  if andand_errors.size > 0 && count < 10
    puts "\n" + "="*80
    puts file
    puts "="*80
    
    andand_errors.first(3).each do |diag|
      count += 1
      break if count > 10
      
      lines = source.lines
      line_num = diag.span.start_line
      
      # Extract context
      start_line = [0, line_num - 2].max
      end_line = [lines.size - 1, line_num + 1].min
      
      puts "\nLine #{line_num}:"
      (start_line..end_line).each do |i|
        marker = i == line_num - 1 ? ">>> " : "    "
        puts "#{marker}#{i + 1}: #{lines[i]}"
      end
      puts "Error: #{diag.message}"
    end
  end
end

puts "\nShowed #{count} examples"

require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Find all "unexpected Def" errors and show context
count = 0

Dir.glob("/Users/sergey/Projects/Crystal/crystal/src/**/*.cr") do |file|
  source = File.read(file)
  
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program
  
  def_errors = parser.diagnostics.select { |d| d.message.includes?("unexpected Def") }
  
  if def_errors.size > 0
    count += def_errors.size
    puts "\n" + "="*80
    puts file
    puts "="*80
    
    def_errors.each do |diag|
      # Find line containing the error
      lines = source.lines
      line_num = diag.span.start_line
      
      puts "\nLine #{line_num}:"
      # Show context: 2 lines before, error line, 2 lines after
      start_line = [0, line_num - 3].max
      end_line = [lines.size - 1, line_num + 1].min
      
      (start_line..end_line).each do |i|
        marker = i == line_num - 1 ? ">>> " : "    "
        puts "#{marker}#{i + 1}: #{lines[i]}"
      end
      
      puts "Error: #{diag.message}"
    end
  end
end

puts "\n" + "="*80
puts "Total 'unexpected Def' errors: #{count}"

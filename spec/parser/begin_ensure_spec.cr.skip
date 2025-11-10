require "../spec_helper"

describe "CrystalV2::Compiler::Frontend::Parser - begin/rescue/ensure" do
  it "parses begin/end block" do
    source = <<-CRYSTAL
      begin
        puts "hello"
      end
      CRYSTAL

    program, errors = parse_source(source)
    errors.should be_empty
    program.top_level_expressions.size.should eq(1)
  end

  it "parses begin/rescue/end block" do
    source = <<-CRYSTAL
      begin
        puts "hello"
      rescue
        puts "error"
      end
      CRYSTAL

    program, errors = parse_source(source)
    errors.should be_empty
    program.top_level_expressions.size.should eq(1)
  end

  it "parses begin/rescue with exception type/end" do
    source = <<-CRYSTAL
      begin
        puts "hello"
      rescue ArgumentError
        puts "arg error"
      end
      CRYSTAL

    program, errors = parse_source(source)
    errors.should be_empty
    program.top_level_expressions.size.should eq(1)
  end

  it "parses begin/rescue with variable binding/end" do
    source = <<-CRYSTAL
      begin
        puts "hello"
      rescue => e
        puts e
      end
      CRYSTAL

    program, errors = parse_source(source)
    errors.should be_empty
    program.top_level_expressions.size.should eq(1)
  end

  it "parses begin/rescue with type and variable/end" do
    source = <<-CRYSTAL
      begin
        puts "hello"
      rescue ArgumentError => e
        puts e
      end
      CRYSTAL

    program, errors = parse_source(source)
    errors.should be_empty
    program.top_level_expressions.size.should eq(1)
  end

  it "parses begin/ensure/end block" do
    source = <<-CRYSTAL
      begin
        puts "hello"
      ensure
        puts "cleanup"
      end
      CRYSTAL

    program, errors = parse_source(source)
    errors.should be_empty
    program.top_level_expressions.size.should eq(1)
  end

  it "parses begin/rescue/ensure/end block" do
    source = <<-CRYSTAL
      begin
        puts "hello"
      rescue
        puts "error"
      ensure
        puts "cleanup"
      end
      CRYSTAL

    program, errors = parse_source(source)
    errors.should be_empty
    program.top_level_expressions.size.should eq(1)
  end

  it "parses begin with multiple rescue clauses and ensure" do
    source = <<-CRYSTAL
      begin
        puts "hello"
      rescue ArgumentError => e
        puts "arg error"
      rescue RuntimeError => e
        puts "runtime error"
      rescue => e
        puts "other error"
      ensure
        puts "cleanup"
      end
      CRYSTAL

    program, errors = parse_source(source)
    errors.should be_empty
    program.top_level_expressions.size.should eq(1)
  end

  it "parses real-world example: process cleanup" do
    source = <<-CRYSTAL
      process = Process.new("ls")
      begin
        result = process.wait
        puts result
      ensure
        process.terminate unless process.terminated?
      end
      CRYSTAL

    program, errors = parse_source(source)
    errors.should be_empty
    program.top_level_expressions.size.should eq(2)  # process assignment + begin block
  end

  it "parses nested begin blocks" do
    source = <<-CRYSTAL
      begin
        begin
          puts "inner"
        rescue
          puts "inner error"
        end
      ensure
        puts "outer cleanup"
      end
      CRYSTAL

    program, errors = parse_source(source)
    errors.should be_empty
    program.top_level_expressions.size.should eq(1)
  end
end
